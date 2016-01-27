{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy        as B
import qualified Data.Binary                 as BI
import           Data.Int
import           Data.MessagePack.Aeson
import           Data.Proxy
import qualified Data.Text                    as T
import           Data.Time.Clock
import qualified Data.Vector                  as V

import           Database.PostgreSQL.Simple

import qualified Facebook                     as FB

import qualified Network.HTTP.Conduit         as C
import           Network.MessagePack.Server
import           Network.MessagePack.Client

import           Web.Users.Types              hiding (UserId)
import           Web.Users.Postgresql         ()

type UserId = Int64

deriveJSON defaultOptions ''NominalDiffTime
deriveJSON defaultOptions ''PasswordPlain
deriveJSON defaultOptions ''FB.Permission

appCredentials :: FB.Credentials
appCredentials = undefined

runServer :: forall a. (FromJSON a, ToJSON a) => Proxy a -> IO ()
runServer _ = do
  conn <- connectPostgreSQL ""
  initUserBackend conn

  manager <- C.newManager C.tlsManagerSettings

  let getUserIdByName' :: T.Text -> Server (AsMessagePack (Maybe UserId))
      getUserIdByName' n = liftIO $ AsMessagePack <$> getUserIdByName conn n

      getUserById' :: (FromJSON a, ToJSON a) => AsMessagePack UserId -> Server (AsMessagePack (Maybe (User a)))
      getUserById' uid = liftIO $ AsMessagePack <$> getUserById conn (getAsMessagePack uid)

      authUser' :: T.Text -> AsMessagePack PasswordPlain -> AsMessagePack NominalDiffTime -> Server (AsMessagePack (Maybe SessionId))
      authUser' a b c = liftIO $ AsMessagePack <$> authUser conn a (getAsMessagePack b) (getAsMessagePack c)

      fbLogin1 :: FB.RedirectUrl -> AsMessagePack [FB.Permission] -> Server T.Text
      fbLogin1 url perms = liftIO $ FB.runFacebookT appCredentials manager $ do
        FB.getUserAccessTokenStep1 url (getAsMessagePack perms)

      fbLogin2 :: FB.RedirectUrl -> [FB.Argument] -> Server (AsMessagePack FB.UserAccessToken)
      fbLogin2 url args = liftIO $ runResourceT $ FB.runFacebookT appCredentials manager $ do
        AsMessagePack <$> FB.getUserAccessTokenStep2 url args

  serve 8537 [ method "getUserIdByName" getUserIdByName'
             , method "getUserById" getUserById'
             , method "authUser" authUser'
             , method "fbLogin1" fbLogin1
             , method "fbLogin2" fbLogin2
             ]

getUserIdByName' :: T.Text -> Client (Maybe UserId)
getUserIdByName' a = getAsMessagePack <$> call "getUserIdByName" a
