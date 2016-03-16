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

data UserDataValidationError = FullNameEmpty

deriveJSON defaultOptions ''UserData
deriveJSON defaultOptions ''UserDataValidationError

test :: IO ()
test = do
  putStrLn "Testing..."

  let cmp (UserData _ "admin") (UserData _ "admin") = EQ
      cmp (UserData _ "admin") (UserData _ _      ) = GT
      cmp (UserData _ "user" ) (UserData _ "user" ) = EQ
      cmp (UserData _ "user" ) (UserData _ _      ) = LT

      cfg = Config
        { defaultUserData = UserData "No name" "user"
        , cmpAccessRights = cmp
        , validateUserData = \ud -> if null (userFullName ud)
            then Left FullNameEmpty
            else Right ()

        , fbCredentials = undefined
        , httpManager = undefined
        }

  conn <- PS.connectPostgreSQL
    "host=localhost port=5432 dbname=postgres connect_timeout=10"
  initOAuthBackend conn

  r <- handleUserCommand conn cfg
    (CreateUser "user2" "user2" "password" Proxy :: UserCommand UserData UserId SessionId UserDataValidationError)
  print r
  case fromJSON r :: (Result (Either CreateUserError UserId)) of
    Success (Right uid) -> do
      r <- handleUserCommand conn cfg
        (AuthUser "user2" "password" 999999999 Proxy :: UserCommand UserData UserId SessionId UserDataValidationError)
      print r
      case fromJSON r :: (Result (Maybe SessionId)) of
        Success (Just sid) -> do
          r <- handleUserCommand conn cfg
            (GetUserData sid uid Proxy :: UserCommand UserData UserId SessionId UserDataValidationError)
          print r
          r <- handleUserCommand conn cfg
            (UpdateUserData sid uid (UserData "NEW DATA" "NEW DATA2") Proxy :: UserCommand UserData UserId SessionId UserDataValidationError)
          print r
        _ -> return ()
      return ()
    _ -> return ()

  return ()
