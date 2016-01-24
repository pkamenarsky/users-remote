{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Users.Remote where

import           Control.Monad.IO.Class

import           Data.Aeson
import qualified Data.ByteString.Lazy        as B
import qualified Data.Binary                 as BI
import           Data.Int
import           Data.MessagePack.Object
import           Data.Proxy
import qualified Data.Text                    as T
import qualified Data.Vector                  as V

import           Database.PostgreSQL.Simple

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

  let getUserIdByName' :: T.Text -> Server (Maybe UserId)
      getUserIdByName' = liftIO . getUserIdByName conn

      getUserById' :: (MessagePack a, FromJSON a, ToJSON a) => UserId -> Server (Maybe (User a))
      getUserById' = liftIO . getUserById conn

  serve 8537 [ method "getUserIdByName" getUserIdByName'
             , method "getUserById" getUserById'
             ]
