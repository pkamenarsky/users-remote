{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Server where

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
import           Data.Time.Clock

import           Database.PostgreSQL.Simple

import qualified Facebook                     as FB

import qualified Network.HTTP.Conduit         as C
import           Network.MessagePack.Server
import           Network.MessagePack.Client
import           Network.WebSockets.Sync

import           System.Random

import           Web.Users.Types              hiding (UserId)
import           Web.Users.Postgresql         ()

import           Web.Users.Remote.Types
import           Web.Users.Remote.Types.Shared

-- "host=localhost port=5432 dbname=postgres connect_timeout=10"
runServer :: forall a. (FromJSON a, ToJSON a, Default a) => Proxy a -> BS.ByteString -> FB.Credentials -> IO ()
runServer _ url appCredentials = do
  conn <- connectPostgreSQL url
  initUserBackend conn

  manager <- C.newManager C.tlsManagerSettings

  let getUserIdByName' :: T.Text -> Server (AsMessagePack (Maybe UserId))
      getUserIdByName' n = liftIO $ AsMessagePack <$> getUserIdByName conn n

      createUser' :: (FromJSON a, ToJSON a) => AsMessagePack (User a) -> AsMessagePack Password -> Server (AsMessagePack (Either CreateUserError UserId))
      createUser' user pwd = liftIO $ AsMessagePack <$> createUser conn (user' { u_password = getAsMessagePack pwd, u_more = (u_more user', None)})
        where user' = getAsMessagePack user

      getUserById' :: (FromJSON a, ToJSON a) => AsMessagePack UserId -> Server (AsMessagePack (Maybe (User (UserInfo a))))
      getUserById' uid = liftIO $ AsMessagePack <$> getUserById conn (getAsMessagePack uid)

      authUser' :: T.Text -> AsMessagePack PasswordPlain -> AsMessagePack NominalDiffTime -> Server (AsMessagePack (Maybe SessionId))
      authUser' name pwd t =
        liftIO $ AsMessagePack . join <$> withAuthUser conn name verifyUser createSession'
          where
            createSession' uid = createSession conn uid (getAsMessagePack t)

            -- don't allow facebook users to login with a password
            verifyUser :: User (UserInfo a) -> Bool
            verifyUser user = case u_more user of
              (_, None) -> verifyPassword (getAsMessagePack pwd) $ u_password user
              (_, FacebookInfo _) -> False

      verifySession' :: AsMessagePack SessionId -> AsMessagePack NominalDiffTime -> Server (AsMessagePack (Maybe UserId))
      verifySession' sid t = liftIO $ AsMessagePack <$> verifySession conn (getAsMessagePack sid) (getAsMessagePack t)

      facebookLoginUrl :: T.Text -> [T.Text] -> Server T.Text
      facebookLoginUrl url perms = liftIO $ FB.runFacebookT appCredentials manager $ do
        FB.getUserAccessTokenStep1 url (map (fromString . show) $ perms ++ ["email"])

      facebookLogin :: T.Text -> [(BS.ByteString, BS.ByteString)] -> AsMessagePack NominalDiffTime -> Server (AsMessagePack (Either FacebookLoginError SessionId))
      facebookLogin url args t = liftIO $ do

        -- try to fetch facebook user
        fbUser <- runResourceT $ FB.runFacebookT appCredentials manager $ do
          token <- FB.getUserAccessTokenStep2 url args
          FB.getUser "me" [] (Just token)

        AsMessagePack <$> case FB.userEmail fbUser of
          Just email -> do
            uid <- getUserIdByName conn email

            case uid of
              Just uid -> do
                sid <- createSession conn uid (getAsMessagePack t)
                return $ maybe (Left CreateSessionError) Right sid
              Nothing  -> do
                -- create random password just in case
                g <- newStdGen
                let pwd = PasswordPlain $ T.pack $ take 32 $ randomRs ('A','z') g
                uid <- createUser conn (User email email (makePassword pwd) True ((defaultValue :: a), FacebookInfo fbUser))

                case uid of
                  Left e -> return $ Left $ CreateUserError e
                  Right uid -> do
                    sid <- createSession conn uid (getAsMessagePack t)
                    return $ maybe (Left CreateSessionError) Right sid

          Nothing -> return $ Left UserEmailEmptyError

  serve 8537 [ method "getUserIdByName" getUserIdByName'
             , method "createUser" createUser'
             , method "getUserById" getUserById'
             , method "authUser" authUser'
             , method "verifySession" verifySession'
             , method "facebookLoginUrl" facebookLoginUrl
             , method "facebookLogin" facebookLogin
             ]

runUsersServer :: BS.ByteString -> IO ()
runUsersServer url = do
  conn <- connectPostgreSQL url
  initUserBackend conn

  let request (VerifySession sid r) = (r, verifySession conn sid 0)

  runSyncServer 8538 request

