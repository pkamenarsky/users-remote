{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Command
  ( handleUserCommand
  , initOAuthBackend
  , queryUserData
  , queryUsers
  ) where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Monad

import           Data.Aeson
import           Data.Bifunctor              as BF
import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString             as BS
import           Data.Maybe
import           Data.Monoid                 ((<>))
import           Data.Proxy
import           Data.String
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Time.Clock
import qualified Data.Time.Clock.POSIX        as POSIX

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.SqlQQ

import qualified Facebook                     as FB

import qualified Network.HTTP.Conduit         as C
import           Network.WebSockets.Sync

import           System.Random

import           Web.Users.Types              hiding (UserId)
import qualified Web.Users.Types              as U
import           Web.Users.Postgresql         ()

import           Web.Users.Remote.Types
import           Web.Users.Remote.Types.Shared

initOAuthBackend :: Connection -> IO ()
initOAuthBackend conn = do
  initUserBackend conn
  void $ execute conn
    [sql|
          create table if not exists login_facebook (
             lid             integer not null references login(lid) on delete restrict,
             fb_id           varchar(128)   not null unique,
             fb_email        varchar(128),
             fb_info         jsonb
          );
    |]
    ()

  void $ execute conn
    [sql|
          create table if not exists login_user_data (
             lid             integer not null references login(lid) on delete restrict,
             user_data       jsonb
          );
    |]
    ()

queryOAuthInfo :: Connection -> UserId -> IO (Maybe OAuthProviderInfo)
queryOAuthInfo conn uid = do
  r <- query conn [sql|select fb_id, fb_email from login_facebook where lid = ? limit 1;|] (Only uid)
  case r of
    [(fbId, fbEmail)] -> return $ Just (FacebookInfo fbId fbEmail)
    _ -> return Nothing

insertOAuthInfo :: Connection -> UserId -> OAuthProviderInfo -> IO Bool
insertOAuthInfo conn uid (FacebookInfo fbId fbEmail) = do
   r <- execute conn [sql|insert into login_facebook (lid, fb_id, fb_email, fb_info) values (?, ?, ?, '{}')|] (uid, fbId, fbEmail)
   return (r > 0)

queryUserData :: (FromJSON ud) => Connection -> UserId -> IO (Maybe (T.Text, (Bool, ud)))
queryUserData conn uid = do
  r <- query conn
    [sql| select login.email, login.is_active, login_user_data.user_data
          from login inner join login_user_data
            on login.lid = login_user_data.lid
          where login.lid = ? limit 1
    |] (Only uid)
  case r of
    [(email, active, ud)] -> case fromJSON ud of
      Success ud -> return $ Just (email, (active, ud))
      _ -> return Nothing
    _ -> return Nothing

-- TODO: insert user data in a searchable format
insertUserData :: ToJSON ud => Connection -> UserId -> ud -> IO Bool
insertUserData conn uid udata = do
   r <- execute conn [sql|insert into login_user_data (lid, user_data) values (?, ?)|] (uid, toJSON udata)
   return (r > 0)

updateUserData :: ToJSON ud => Connection -> UserId -> ud -> IO Bool
updateUserData conn uid udata = do
   r <- execute conn [sql|update login_user_data set user_data = ? where lid = ?|] (toJSON udata, uid)
   return (r > 0)

queryUsers :: (Show uid, FromJSON ud, FromField uid) => Connection -> T.Text -> IO [(T.Text, (Bool, (uid, ud)))]
queryUsers conn pattern = do
  rs <- query conn
    [sql| select login.email, login.is_active, login_user_data.lid, login_user_data.user_data
          from login inner join login_user_data
            on login.lid = login_user_data.lid
          where login.username ilike ? or login.email ilike ? or login_user_data.user_data->>'userFullName' ilike ?
    |] ("%" <> pattern <> "%", "%" <> pattern <> "%", "%" <> pattern <> "%")
  return [ (email, (active, (uid, ud))) | (email, active, uid, ud') <- rs, Success ud <- [fromJSON ud'] ]

checkRights :: forall udata err. (FromJSON udata, ToJSON udata)
            => Config udata err
            -> Connection
            -> SessionId
            -> UserId
            -> IO Bool
checkRights (Config {..}) conn sid uid = do
  uidMine <- verifySession conn sid (fromIntegral 0)
  case uidMine of
    Just uidMine -> do
      if uidMine == uid
        then return True
        else do
          udataMine <- queryUserData conn uidMine :: IO (Maybe (T.Text, (Bool, udata)))
          udataTheirsOld <- queryUserData conn uid :: IO (Maybe (T.Text, (Bool, udata)))

          -- only update user data if we have the access rights
          case (udataMine, udataTheirsOld) of
            (Just (_, (True, udataMine)), Just (_, (_, udataTheirsOld)))
              | udataMine `cmpAccessRights` udataTheirsOld == GT -> return True
            _ -> return False
    _ -> return False

authFB :: forall udata err. (ToJSON err, FromJSON udata, ToJSON udata)
       => Connection
       -> Config udata err
       -> FB.UserAccessToken
       -> udata
       -> Int
       -> IO (Either (FacebookLoginError err) SessionId)
authFB conn (Config {..}) token udata t = do
  fbUser <- runResourceT $ FB.runFacebookT fbCredentials httpManager $
    FB.getUser "me" [] (Just token)

  let fbUserName = FB.appId fbCredentials <> FB.idCode (FB.userId fbUser)

  uid <- getUserIdByName conn fbUserName

  case uid of
    Just uid -> do
      user <- getUserById conn uid
      case user of
        Just user | u_active user -> do
          sid <- createSession conn uid (fromIntegral t)
          return $ maybe (Left FacebookCreateSessionError) Right sid
        _ -> return (Left FacebookUserBanned)
    Nothing  -> do
      -- create random password just in case
      g <- newStdGen
      let pwd = PasswordPlain $ T.pack $ take 32 $ randomRs ('A','z') g
          udata' = augmentUserDataWithFbUser fbUser $ maskUserDataFromClient udata

      case validateUserData udata' of
        Left e -> return $ Left $ FacebookUserValidationError e
        Right _ -> do
          uid <- createUser conn $ User fbUserName (fromMaybe "" $ FB.userEmail fbUser) (makePassword pwd) True

          case uid of
            Left e -> return $ Left $ FacebookCreateUserError e
            Right uid -> do
              r1 <- insertOAuthInfo conn uid (FacebookInfo (FB.userId fbUser) (FB.userEmail fbUser))
              r2 <- insertUserData conn uid udata'

              case (r1, r2) of
                (True, True) -> do
                  sid <- createSession conn uid (fromIntegral t)
                  return $ maybe (Left FacebookCreateSessionError) Right sid
                _ -> return $ Left FacebookCreateSessionError

handleUserCommand :: forall udata err. (ToJSON err, FromJSON udata, ToJSON udata)
                  => Connection
                  -> Config udata err
                  -> UserCommand udata UserId SessionId err
                  -> IO Value
handleUserCommand conn _ (VerifySession sid r)   = respond r <$> verifySession conn sid 0

handleUserCommand conn _ (AuthUser name pwd t r) = respond r <$> do
  uid <- getUserIdByName conn name

  case uid of
    Just uid -> do
      fbinfo <- queryOAuthInfo conn uid

      case fbinfo of
        Nothing -> fmap join $ withAuthUser conn name (\user -> u_active user && verifyPassword (PasswordPlain pwd) (u_password user)) $ \userId ->
           createSession conn userId (fromIntegral t)
        _ -> return Nothing
    Nothing -> return Nothing

handleUserCommand _ (Config {..}) (AuthFacebookUrl url perms r) = respond r <$> do
  FB.runFacebookT fbCredentials httpManager $
    FB.getUserAccessTokenStep1 url (map (fromString . T.unpack) $ perms ++ ["email", "public_profile"])

handleUserCommand conn cfg (AuthFacebookWithToken fbUserId tokenData tokenTime udata t r) = respond r <$> do
  authFB conn cfg (FB.UserAccessToken (FB.Id fbUserId) tokenData (POSIX.posixSecondsToUTCTime $ fromIntegral tokenTime)) udata t

handleUserCommand conn cfg@(Config {..}) (AuthFacebook url args udata t r) = respond r <$> do
  -- try to fetch facebook user
  token <- runResourceT $ FB.runFacebookT fbCredentials httpManager $
    FB.getUserAccessTokenStep2 url (map (TE.encodeUtf8 *** TE.encodeUtf8) args)
  authFB conn cfg token udata t

handleUserCommand conn (Config {..}) (CreateUser u_name u_email password udata r) = respond r <$> do
  case validateUserData udata of
    Left e -> return $ Left $ CreateUserValidationError e
    Right _ -> do
      uid <- createUser conn (User { u_active = True, u_password = makePassword (PasswordPlain password), .. })
      case uid of
        Right uid -> do
          r <- insertUserData conn uid (maskUserDataFromClient udata)
          if r
            then return $ Right uid
            else do
              deleteUser conn uid
              return $ Left $ CreateUserError UsernameAlreadyTaken
        Left e -> return $ Left $ CreateUserError e

handleUserCommand conn cfg (UpdateUserData sid uid udata r) = respond r <$> do
  rights <- checkRights cfg conn sid uid

  if rights
    then updateUserData conn uid udata >> return (Just udata)
    else return Nothing

handleUserCommand conn cfg (SetUserBanStatus sid uid status r) = respond r <$> do
  rights <- checkRights cfg conn sid uid

  if rights
    then fmap (fmap $ const Ok) $ updateUser conn uid $ \user -> user { u_active = status }
    else return $ Left UserDoesntExist

handleUserCommand conn cfg (SetUserEmail sid uid email r) = respond r <$> do
  rights <- checkRights cfg conn sid uid

  if rights
    then fmap (fmap $ const Ok) $ updateUser conn uid $ \user -> user { u_email = email }
    else return $ Left UserDoesntExist

handleUserCommand conn cfg (SetUserPassword sid oldPassword password r) = respond r <$> do
  uid <- verifySession conn sid (fromIntegral 0)

  case uid of
    Just uid -> do
      user <- getUserById conn uid

      case user of
        Just user -> do
          auth <- withAuthUser conn (u_name user) (\user -> u_active user && verifyPassword (PasswordPlain oldPassword) (u_password user)) $ \_ -> return True

          case auth of
            Just True -> fmap (fmap $ const Ok) $ updateUser conn uid $ \user -> user { u_password = makePassword (PasswordPlain password) }
            _ -> return $ Left UserDoesntExist
        _ -> return $ Left UserDoesntExist
    _ -> return $ Left UserDoesntExist

handleUserCommand conn cfg (GetUserData uid r) = respond r <$> do
  queryUserData conn uid

handleUserCommand conn cfg (QueryUsers query r) = respond r <$> do
  queryUsers conn query

handleUserCommand conn cfg (ResetPassword name r) = respond r <$> do
  uid <- getUserIdByName conn name
  case uid of
    Just uid -> do
      token <- requestPasswordReset conn uid (fromIntegral 3600)
      sendResetPasswordMail cfg name token
    Nothing -> return ()

  return Ok

handleUserCommand conn cfg (SetNewPassword token password r) = respond r <$> do
  uid <- applyNewPassword conn (PasswordResetToken token) (makePassword $ PasswordPlain password)
  return Ok

handleUserCommand conn (Config {..}) (Logout sid r) = respond r <$> do
  destroySession conn sid
  return Ok
