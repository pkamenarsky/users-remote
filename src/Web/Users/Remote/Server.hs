{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Server where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Monad

import           Data.Aeson
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

import qualified Facebook                     as FB

import qualified Network.HTTP.Conduit         as C
import           Network.WebSockets.Sync

import           System.Random

import           Web.Users.Types              hiding (UserId)
import qualified Web.Users.Types              as U
import           Web.Users.Postgresql         ()

import           Web.Users.Remote.Types
import           Web.Users.Remote.Types.Shared

-- FIXME: find a better solution for internal user data
getInternalUserById :: forall uinfo conn. (UserStorageBackend conn, FromJSON uinfo, ToJSON uinfo) => conn -> U.UserId conn -> IO (Maybe (U.User uinfo))
getInternalUserById conn uid = do
  user <- getUserById conn uid
  return $ flip fmap user $ \user -> user { u_more = fst (u_more (user :: User (UserInfo uinfo))) }

handleUserCommand :: forall uinfo conn. (UserStorageBackend conn, FromJSON uinfo, ToJSON uinfo, Default uinfo)
                  => conn
                  -> FB.Credentials
                  -> C.Manager
                  -> UserCommand uinfo (U.UserId conn) SessionId
                  -> IO Value
handleUserCommand conn _ _ (VerifySession sid r)   = respond r <$> verifySession conn sid 0

handleUserCommand conn _ _ (AuthUser name pwd t r) = respond r <$> do
  join <$> withAuthUser conn name verifyUser createSession'
    where
      createSession' uid = createSession conn uid (fromIntegral t)

      -- don't allow facebook users to login with a password
      verifyUser :: User (UserInfo uinfo) -> Bool
      verifyUser user = case u_more user of
        (_, None) -> verifyPassword (PasswordPlain pwd) $ u_password user
        (_, FacebookInfo _ _) -> False

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
      uid <- createUser conn (User fbUserName (fromMaybe "" $ FB.userEmail fbUser) (makePassword pwd) True ((defaultValue :: uinfo), FacebookInfo (FB.userId fbUser) (FB.userEmail fbUser)))

      case uid of
        Left e -> return $ Left $ CreateUserError e
        Right uid -> do
          sid <- createSession conn uid (fromIntegral t)
          return $ maybe (Left CreateSessionError) Right sid

handleUserCommand conn cred manager (CreateUser user pwd r) = respond r <$> do
  createUser conn (user { u_password = makePassword (PasswordPlain pwd), u_more = (defaultValue :: uinfo, None)})

handleUserCommand conn cred manager (GetUserById uid r) = respond r <$> do
  user <- getUserById conn uid
  return $ flip fmap user $ \user -> user { u_more = fst (u_more (user :: User (UserInfo uinfo))) }

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
