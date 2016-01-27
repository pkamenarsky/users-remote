{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Server where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString             as BS
import           Data.Int
import           Data.MessagePack.Aeson
import           Data.Proxy
import           Data.String
import qualified Data.Text                    as T
import           Data.Time.Clock

import           Database.PostgreSQL.Simple

import qualified Facebook                     as FB

import qualified Network.HTTP.Conduit         as C
import           Network.MessagePack.Server
import           Network.MessagePack.Client

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

data UserProviderInfo = Facebook FB.User
                      | None

type UserInfo a = (a, UserProviderInfo)

-- "host=localhost port=5432 dbname=postgres connect_timeout=10"
runServer :: forall a. (FromJSON a, ToJSON a) => Proxy a -> BS.ByteString -> FB.Credentials -> IO ()
runServer _ url appCredentials = do
  conn <- connectPostgreSQL url
  initUserBackend conn

  manager <- C.newManager C.tlsManagerSettings

  let getUserIdByName' :: T.Text -> Server (AsMessagePack (Maybe UserId))
      getUserIdByName' n = liftIO $ AsMessagePack <$> getUserIdByName conn n

      createUser' :: (FromJSON a, ToJSON a) => AsMessagePack (User a) -> AsMessagePack Password -> Server (AsMessagePack (Either CreateUserError UserId))
      createUser' u pwd = liftIO $ AsMessagePack <$> createUser conn ((getAsMessagePack u) { u_password = getAsMessagePack pwd })

      getUserById' :: (FromJSON a, ToJSON a) => AsMessagePack UserId -> Server (AsMessagePack (Maybe (User a)))
      getUserById' uid = liftIO $ AsMessagePack <$> getUserById conn (getAsMessagePack uid)

      authUser' :: T.Text -> AsMessagePack PasswordPlain -> AsMessagePack NominalDiffTime -> Server (AsMessagePack (Maybe SessionId))
      authUser' a b c = liftIO $ AsMessagePack <$> authUser conn a (getAsMessagePack b) (getAsMessagePack c)

      verifySession' :: AsMessagePack SessionId -> AsMessagePack NominalDiffTime -> Server (AsMessagePack (Maybe UserId))
      verifySession' sid t = liftIO $ AsMessagePack <$> verifySession conn (getAsMessagePack sid) (getAsMessagePack t)

      fbLogin1 :: T.Text -> [T.Text] -> Server T.Text
      fbLogin1 url perms = liftIO $ FB.runFacebookT appCredentials manager $ do
        FB.getUserAccessTokenStep1 url (map (fromString . show) perms)

      fbLogin2 :: T.Text -> [(BS.ByteString, BS.ByteString)] -> Server (AsMessagePack FB.UserAccessToken)
      fbLogin2 url args = liftIO $ runResourceT $ FB.runFacebookT appCredentials manager $ do
        AsMessagePack <$> FB.getUserAccessTokenStep2 url args

      -- fbLogin3 :: T.Text -> [(BS.ByteString, BS.ByteString)] -> Server (AsMessagePack FB.UserAccessToken)
      fbLogin3 url args = liftIO $ runResourceT $ FB.runFacebookT appCredentials manager $ do
        token <- FB.getUserAccessTokenStep2 url args
        user <- FB.getUser "me" [] (Just token)
        return ()

  serve 8537 [ method "getUserIdByName" getUserIdByName'
             , method "createUser" createUser'
             , method "getUserById" getUserById'
             , method "authUser" authUser'
             , method "verifySession" verifySession'
             , method "fbLogin1" fbLogin1
             , method "fbLogin2" fbLogin2
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

fbLogin1 :: T.Text -> [T.Text] -> Client T.Text
fbLogin1 url perms = call "fbLogin1" url (AsMessagePack perms)

fbLogin2 :: T.Text -> [(BS.ByteString, BS.ByteString)] -> Client FB.UserAccessToken
fbLogin2 url args = getAsMessagePack <$> call "fbLogin2" url args

testClient :: IO ()
testClient = do
  execClient "localhost" 8537 $ do
    uid <- createUser' (User "localuser" "user@gmail.com" (makePassword "pwd") True (0 :: Int))
    sid <- authUser' "localuser" "pwd" 99999999
    liftIO $ print sid
