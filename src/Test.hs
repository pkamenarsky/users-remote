{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Data.Aeson
import Data.Aeson.TH
import Data.Default.Class
import Data.Proxy

import Web.Users.Types              hiding (UserId)

import Web.Users.Remote.Command
import Web.Users.Remote.Types
import Web.Users.Remote.Types.Shared

import qualified Database.PostgreSQL.Simple     as PS

data UserData = UserData { userFullName :: String, userRole :: String }

deriveJSON defaultOptions ''UserData

instance Default UserData where
  def = UserData "No name" "user"

instance OrdAccessRights UserData where
  UserData _ "admin" `cmpAccessRighs` UserData _ "admin" = EQ
  UserData _ "admin" `cmpAccessRighs` UserData _ _ = GT
  UserData _ "user" `cmpAccessRighs` UserData _ "user" = EQ
  UserData _ "user" `cmpAccessRighs` UserData _ _ = LT

test :: IO ()
test = do
  putStrLn "Testing..."

  conn <- PS.connectPostgreSQL
    "host=localhost port=5432 dbname=postgres connect_timeout=10"
  initOAuthBackend conn

  r <- handleUserCommand conn undefined undefined
    (CreateUser "user2" "user2" "password" Proxy :: UserCommand UserData UserId SessionId)
  print r
  case fromJSON r :: (Result (Either CreateUserError UserId)) of
    Success (Right uid) -> do
      r <- handleUserCommand conn undefined undefined
        (AuthUser "user2" "password" 999999999 Proxy :: UserCommand UserData UserId SessionId)
      print r
      case fromJSON r :: (Result (Maybe SessionId)) of
        Success (Just sid) -> do
          r <- handleUserCommand conn undefined undefined
            (GetUserData sid uid Proxy :: UserCommand UserData UserId SessionId)
          print r
          r <- handleUserCommand conn undefined undefined
            (UpdateUserData sid uid (UserData "NEW DATA" "NEW DATA2") Proxy :: UserCommand UserData UserId SessionId)
          print r
        _ -> return ()
      return ()
    _ -> return ()

  return ()
