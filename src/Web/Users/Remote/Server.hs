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
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString             as BS
import           Data.Int
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

import           System.Random

import           Web.Users.Types              hiding (UserId)
import           Web.Users.Postgresql         ()

type UserId = Int64

deriveJSON defaultOptions ''CreateUserError
deriveJSON defaultOptions ''NominalDiffTime
deriveJSON defaultOptions ''PasswordPlain
deriveJSON defaultOptions ''Password

deriveToJSON defaultOptions ''FB.GeoCoordinates
deriveToJSON defaultOptions ''FB.Location
deriveToJSON defaultOptions ''FB.Place
deriveToJSON defaultOptions ''FB.User

data UserProviderInfo = FacebookInfo FB.User
                      | None

deriveJSON defaultOptions ''UserProviderInfo

class Default a where
  defaultValue :: a

type UserInfo a = (a, UserProviderInfo)

data FacebookLoginError = UserEmailEmptyError
                        | CreateSessionError
                        | CreateUserError CreateUserError

deriveJSON defaultOptions ''FacebookLoginError

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

      getUserById' :: (FromJSON a, ToJSON a) => AsMessagePack UserId -> Server (AsMessagePack (Maybe (User a)))
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

getUserIdByName' :: T.Text -> Client (Maybe UserId)
getUserIdByName' a = getAsMessagePack <$> call "getUserIdByName" a

createUser' :: (FromJSON a, ToJSON a) => User a -> Client (Either CreateUserError UserId)
createUser' u = getAsMessagePack <$> call "createUser" (AsMessagePack u) (AsMessagePack $ u_password u)

getUserById' :: (FromJSON a, ToJSON a) => UserId -> Client (Maybe (User a))
getUserById' uid = getAsMessagePack <$> call "getUserById" (AsMessagePack uid)

authUser' :: T.Text -> PasswordPlain -> NominalDiffTime -> Client (Maybe SessionId)
authUser' a b c = getAsMessagePack <$> call "authUser" a (AsMessagePack b) (AsMessagePack c)

verifySession' :: SessionId -> NominalDiffTime -> Client (Maybe UserId)
verifySession' sid t = getAsMessagePack <$> call "verifySession" (AsMessagePack sid) (AsMessagePack t)

facebookLoginUrl :: T.Text -> [T.Text] -> Client T.Text
facebookLoginUrl url perms = call "facebookLoginUrl" url (AsMessagePack perms)

facebookLogin :: T.Text -> [(BS.ByteString, BS.ByteString)] -> NominalDiffTime -> Client FB.UserAccessToken
facebookLogin url args t = getAsMessagePack <$> call "facebookLogin" url args (AsMessagePack t)

testClient :: IO ()
testClient = do
  execClient "localhost" 8537 $ do
    uid <- createUser' (User "localuser" "user@gmail.com" (makePassword "pwd") True (0 :: Int))
    sid <- authUser' "localuser" "pwd" 99999999
    liftIO $ print sid
