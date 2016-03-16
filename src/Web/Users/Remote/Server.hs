{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Server where

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

import           Database.PostgreSQL.Simple
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
initOAuthBackend conn =
  void $ execute conn
    [sql|
          create table if not exists login_facebook (
             lid             serial references login on delete cascade,
             fb_id           varchar(64)    not null unique,
             fb_email        varchar(128)   unique,
             fb_info         jsonb
          );
    |]

queryOAuthInfo :: Connection -> UserId Connection -> IO (Maybe UserProviderInfo)
queryOAuthInfo = return Nothing

handleUserCommand :: (UserStorageBackend Connection)
                  => Connection
                  -> FB.Credentials
                  -> C.Manager
                  -> UserCommand (U.UserId Connection) SessionId
                  -> IO Value
handleUserCommand conn _ _ (VerifySession sid r)   = respond r <$> verifySession conn sid 0

handleUserCommand conn _ _ (AuthUser name pwd t r) = respond r <$> do
  uid <- getUserIdByName conn name

  case uid of
    Just uid -> do
      fbinfo <- queryOAuthInfo conn uid

      case fbinfo of
        Nothing -> authUser conn name pwd (fromIntegral t)
        _ -> return Nothing
    Nothing -> return Nothing

handleUserCommand _ cred manager (AuthFacebookUrl url perms r) = respond r <$> do
  FB.runFacebookT cred manager $
    FB.getUserAccessTokenStep1 url (map (fromString . T.unpack) $ perms ++ ["email", "public_profile"])

handleUserCommand conn cred manager (AuthFacebook url args t r) = respond r <$> do
  -- try to fetch facebook user
  fbUser <- runResourceT $ FB.runFacebookT cred manager $ do
    token <- FB.getUserAccessTokenStep2 url (map (TE.encodeUtf8 *** TE.encodeUtf8) args)
    FB.getUser "me" [] (Just token)

  let fbUserName = FB.appId cred <> FB.idCode (FB.userId fbUser)

  uid <- getUserIdByName conn fbUserName

  case uid of
    Just uid -> do
      sid <- createSession conn uid (fromIntegral t)
      return $ maybe (Left CreateSessionError) Right sid
    Nothing  -> do
      -- create random password just in case
      g <- newStdGen
      let pwd = PasswordPlain $ T.pack $ take 32 $ randomRs ('A','z') g
      uid <- createUser conn
            $ User fbUserName (fromMaybe fbUserName $ FB.userEmail fbUser) (makePassword pwd) True
              (UserBackendInfo
                (UserAdditionalInfo (fromMaybe "Facebook User" $ FB.userName fbUser) defaultValue)
                (FacebookInfo (FB.userId fbUser) (FB.userEmail fbUser)) :: UserBackendInfo uinfo)

      case uid of
        Left e -> return $ Left $ CreateUserError e
        Right uid -> do
          sid <- createSession conn uid (fromIntegral t)
          return $ maybe (Left CreateSessionError) Right sid

handleUserCommand conn cred manager (CreateUser user pwd r) = respond r <$> do
  if T.null $ userAIFullName $ u_more user
    then return $ Left UserFullNameEmptyError
    else do
      r <- createUser conn $ user
             { u_password = makePassword (PasswordPlain pwd)
             -- userInfo is set to default value here, since it may involve setting
             -- permissions and the like
             , u_more = UserBackendInfo ((u_more user) { userInfo = defaultValue }) None :: UserBackendInfo uinfo
             }
      return $ BF.first CreateUserExtraError r

handleUserCommand conn cred manager (GetUserById uid r) = respond r <$> do
  user <- getUserById conn uid
  return $ flip fmap user $ \user -> user { u_more = userAdditionalInfo (u_more (user :: User (UserBackendInfo uinfo))) }

handleUserCommand conn cred manager (Logout sid r) = respond r <$> do
  destroySession conn sid
  return Ok

runAuthServer :: forall a. (FromJSON a, ToJSON a, Default a) => Proxy a -> FB.Credentials -> C.Manager -> Connection -> Int -> IO ()
runAuthServer proxy cred manager conn port = do
  initUserBackend conn
  runSyncServer port (handleUserCommand conn cred manager :: UserCommand a (U.UserId Connection) SessionId -> IO Value)

-- Test server -----------------------------------------------------------------

instance Default String where
  defaultValue = ""

runTestAuthServer :: T.Text -> T.Text -> T.Text -> IO ()
runTestAuthServer appName appId appSecret = do
  manager <- C.newManager C.tlsManagerSettings
  conn <- connectPostgreSQL "host=localhost port=5432 dbname=postgres connect_timeout=10"

  runAuthServer (undefined :: Proxy String)
                (FB.Credentials appName appId appSecret)
                manager
                conn
                8538
