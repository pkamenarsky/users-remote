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
import           Data.MessagePack.Aeson
import           Data.Maybe
import           Data.Proxy
import           Data.String
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Time.Clock

import           Database.PostgreSQL.Simple

import qualified Facebook                     as FB

import qualified Network.HTTP.Conduit         as C
import           Network.MessagePack.Server
import           Network.MessagePack.Client
import           Network.WebSockets.Sync

import           System.Random

import           Web.Users.Types              hiding (UserId)
import qualified Web.Users.Types              as U
import           Web.Users.Postgresql         ()

import           Web.Users.Remote.Types
import           Web.Users.Remote.Types.Shared

-- "host=localhost port=5432 dbname=postgres connect_timeout=10"
runVerificationServer :: BS.ByteString -> FB.Credentials -> IO ()
runVerificationServer url appCredentials = do
  conn <- connectPostgreSQL url
  initUserBackend conn

  manager <- C.newManager C.tlsManagerSettings

  let verifySession' :: AsMessagePack SessionId -> AsMessagePack NominalDiffTime -> Server (AsMessagePack (Maybe UserId))
      verifySession' sid t = liftIO $ AsMessagePack <$> verifySession conn (getAsMessagePack sid) (getAsMessagePack t)

  serve 8537 [ method "verifySession" verifySession' ]

handleUserCommand :: forall uinfo conn. (UserStorageBackend conn, FromJSON uinfo, ToJSON uinfo, Default uinfo)
                  => Proxy uinfo
                  -> conn
                  -> FB.Credentials
                  -> C.Manager
                  -> UserCommand uinfo (U.UserId conn) SessionId
                  -> IO Value
handleUserCommand _ conn _ _ (VerifySession sid r)   = respond r <$> verifySession conn sid 0

handleUserCommand _ conn _ _ (AuthUser name pwd t r) = respond r <$> do
  join <$> withAuthUser conn name verifyUser createSession'
    where
      createSession' uid = createSession conn uid (fromIntegral t)

      -- don't allow facebook users to login with a password
      verifyUser :: User (UserInfo uinfo) -> Bool
      verifyUser user = case u_more user of
        (_, None) -> verifyPassword (PasswordPlain pwd) $ u_password user
        (_, FacebookInfo _) -> False

handleUserCommand _ _ cred manager (AuthFacebookUrl url perms r) = respond r <$> do
  FB.runFacebookT cred manager $
    FB.getUserAccessTokenStep1 url (map (fromString . show) $ perms ++ ["email"])

handleUserCommand _ conn cred manager (AuthFacebook url args t r) = respond r <$> do
  -- try to fetch facebook user
  fbUser <- runResourceT $ FB.runFacebookT cred manager $ do
    token <- FB.getUserAccessTokenStep2 url (map (TE.encodeUtf8 *** TE.encodeUtf8) args)
    FB.getUser "me" [] (Just token)

  case FB.userEmail fbUser of
    Just email -> do
      uid <- getUserIdByName conn email

      case uid of
        Just uid -> do
          sid <- createSession conn uid (fromIntegral t)
          return $ maybe (Left CreateSessionError) Right sid
        Nothing  -> do
          -- create random password just in case
          g <- newStdGen
          let pwd = PasswordPlain $ T.pack $ take 32 $ randomRs ('A','z') g
          uid <- createUser conn (User email email (makePassword pwd) True ((defaultValue :: uinfo), FacebookInfo fbUser))

          case uid of
            Left e -> return $ Left $ CreateUserError e
            Right uid -> do
              sid <- createSession conn uid (fromIntegral t)
              return $ maybe (Left CreateSessionError) Right sid

    Nothing -> return $ Left UserEmailEmptyError

handleUserCommand _ conn cred manager (CreateUser user pwd r) = respond r <$> do
  createUser conn (user { u_password = makePassword (PasswordPlain pwd), u_more = (u_more user, None)})

handleUserCommand _ conn cred manager (GetUserById uid r) = respond r <$> do
  getUserById conn uid

runAuthServer :: (FromJSON a, ToJSON a, Default a) => Proxy a -> FB.Credentials -> C.Manager -> BS.ByteString -> Int -> IO ()
runAuthServer proxy cred manager url port = do
  conn <- connectPostgreSQL url
  initUserBackend conn
  runSyncServer port (handleUserCommand proxy conn cred manager)
