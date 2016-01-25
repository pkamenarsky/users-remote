{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Users.Remote where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Data.Aeson
import qualified Data.ByteString.Lazy        as B
import qualified Data.Binary                 as BI
import           Data.Int
import           Data.MessagePack.Object
import           Data.Proxy
import qualified Data.Text                    as T
import           Data.Time.Clock
import qualified Data.Vector                  as V

import           Database.PostgreSQL.Simple

import qualified Facebook                     as FB

import qualified Network.HTTP.Conduit         as C
import           Network.MessagePack.Server

import           Web.Users.Types              hiding (UserId)
import           Web.Users.Postgresql         ()

type UserId = Int64

instance MessagePack UserId where
  toObject uid = ObjectBin (B.toStrict $ BI.encode uid)
  fromObject (ObjectBin bin) = case BI.decodeOrFail $ B.fromStrict bin of
    Right (_, _, a) -> a
    _ -> Nothing
  fromObject _ = Nothing

instance MessagePack NominalDiffTime where
  toObject = undefined
  fromObject = undefined

instance MessagePack SessionId where
  toObject = undefined
  fromObject = undefined

instance MessagePack PasswordPlain where
  toObject = undefined
  fromObject = undefined

instance MessagePack PasswordResetToken where
  toObject = undefined
  fromObject = undefined

instance MessagePack CreateUserError where
  toObject = undefined
  fromObject = undefined

instance MessagePack UpdateUserError where
  toObject = undefined
  fromObject = undefined

instance MessagePack TokenError where
  toObject = undefined
  fromObject = undefined

instance MessagePack (FB.AccessToken FB.UserKind) where
  toObject = undefined
  fromObject = undefined

instance MessagePack Password where
  toObject (PasswordHash hash) = ObjectArray $ V.fromList
    [ ObjectBool True, ObjectStr hash ]
  fromObject (ObjectArray a) = case V.toList a of
    [ ObjectBool True, ObjectStr hash ] -> Just (PasswordHash hash)
    _ -> Nothing
  fromObject _ = Nothing

instance MessagePack a => MessagePack (User a) where
  toObject User {..} = ObjectArray $ V.fromList
    [ ObjectStr u_name
    , ObjectStr u_email
    , toObject u_password
    , ObjectBool u_active
    , toObject u_more
    ]
  fromObject (ObjectArray a) = case V.toList a of
    [ ObjectStr u_name , ObjectStr u_email , u_password , ObjectBool u_active , u_more ]
      -> User <$> pure u_name <*> pure u_email <*> fromObject u_password <*> pure u_active <*> fromObject u_more
    _ -> Nothing
  fromObject _ = Nothing

runServer :: forall a. (MessagePack a, FromJSON a, ToJSON a) => Proxy a -> IO ()
runServer _ = do
  conn <- connectPostgreSQL ""
  initUserBackend conn

  manager <- C.newManager C.tlsManagerSettings

  let getUserIdByName' :: T.Text -> Server (Maybe UserId)
      getUserIdByName' = liftIO . getUserIdByName conn

      getUserById' :: (MessagePack a, FromJSON a, ToJSON a) => UserId -> Server (Maybe (User a))
      getUserById' = liftIO . getUserById conn

      authUser' :: T.Text -> PasswordPlain -> NominalDiffTime -> Server (Maybe SessionId)
      authUser' a b c = liftIO $ authUser conn a b c

      fbLogin2 :: FB.RedirectUrl -> [FB.Argument] -> Server FB.UserAccessToken
      fbLogin2 url args = liftIO $ runResourceT $ FB.runFacebookT appCredentials manager $ do
        token <- FB.getUserAccessTokenStep2 url args
        return token

  serve 8537 [ method "getUserIdByName" getUserIdByName'
             , method "getUserById" getUserById'
             , method "authUser" authUser'
             , method "fbLogin2" fbLogin2
             ]

appCredentials :: FB.Credentials
appCredentials = undefined

login :: FB.FacebookT FB.Auth IO T.Text
login = do
  url <- FB.getUserAccessTokenStep1 "" []
  return url

main = do
  manager <- C.newManager C.tlsManagerSettings
  url <- FB.runFacebookT appCredentials manager login
  print url
  return ()
