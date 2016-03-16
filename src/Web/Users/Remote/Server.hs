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
    ()

queryOAuthInfo :: Connection -> UserId -> IO (Maybe OAuthProviderInfo)
queryOAuthInfo conn uid = do
  r <- query conn [sql|select fb_id, fb_email from login_facebook where lid = ? limit 1;|] (Only uid)
  case r of
    [(fbId, fbEmail)] -> return $ Just (FacebookInfo fbId fbEmail)
    _ -> return Nothing

insertOAuthInfo :: Connection -> UserId -> OAuthProviderInfo -> IO ()
insertOAuthInfo conn uid (FacebookInfo fbId fbEmail) =
   void $ execute conn [sql|insert into facebook_login (lid, fb_id, fb_email, fb_info) values (?, ?, ?, '{}')|] (uid, fbId, fbEmail)

handleUserCommand :: Connection
                  -> FB.Credentials
                  -> C.Manager
                  -> UserCommand UserId SessionId
                  -> IO Value
handleUserCommand conn _ _ (VerifySession sid r)   = respond r <$> verifySession conn sid 0

handleUserCommand conn _ _ (AuthUser name pwd t r) = respond r <$> do
  uid <- getUserIdByName conn name

  case uid of
    Just uid -> do
      fbinfo <- queryOAuthInfo conn uid

      case fbinfo of
        Nothing -> authUser conn name (PasswordPlain pwd) (fromIntegral t)
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

      uid <- createUser conn $ User fbUserName (fromMaybe fbUserName $ FB.userEmail fbUser) (makePassword pwd) True

      case uid of
        Left e -> return $ Left $ CreateUserError e
        Right uid -> do
          insertOAuthInfo conn uid (FacebookInfo (FB.userId fbUser) (FB.userEmail fbUser))
          sid <- createSession conn uid (fromIntegral t)
          return $ maybe (Left CreateSessionError) Right sid

handleUserCommand conn cred manager (CreateUser u_name u_email password r) = respond r <$> do
  createUser conn (User { u_active = True, u_password = makePassword (PasswordPlain password), .. })

handleUserCommand conn cred manager (GetUserById uid r) = respond r <$> do
  getUserById conn uid

handleUserCommand conn cred manager (Logout sid r) = respond r <$> do
  destroySession conn sid
  return Ok
