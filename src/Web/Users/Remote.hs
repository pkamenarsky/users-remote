{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote where

import           Control.Monad.IO.Class

import           Data.Aeson.TH
import           Data.Int
import           Data.MessagePack.Object
import qualified Data.Text as T

import           Database.PostgreSQL.Simple

import           Network.MessagePack.Server

import           Web.Users.Types
import           Web.Users.Postgresql ()

instance MessagePack Int64 where
  toObject = undefined
  fromObject = undefined

data UserData = UserData { userCredits :: Int }

instance MessagePack (User UserData) where
  toObject = undefined
  fromObject = undefined

deriveJSON defaultOptions ''UserData

runServer :: IO ()
runServer = do
  conn <- connectPostgreSQL ""
  initUserBackend conn

  let getUserIdByName' :: T.Text -> Server (Maybe Int64)
      getUserIdByName' = liftIO . getUserIdByName conn

      getUserById' :: Int64 -> Server (Maybe (User UserData))
      getUserById' = liftIO . getUserById conn

  serve 8537 [ method "getUserIdByName" getUserIdByName'
             , method "getUserById" getUserById'
             ]
