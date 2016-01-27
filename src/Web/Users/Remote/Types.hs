{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Types where

import           Data.Aeson.TH
import           Data.Int
import           Data.Time.Clock

import qualified Facebook                     as FB

import           Web.Users.Types              hiding (UserId)

type UserId = Int64

deriveJSON defaultOptions ''CreateUserError
deriveJSON defaultOptions ''NominalDiffTime
deriveJSON defaultOptions ''PasswordPlain
deriveJSON defaultOptions ''Password

deriveToJSON defaultOptions ''FB.GeoCoordinates
deriveToJSON defaultOptions ''FB.Location
deriveToJSON defaultOptions ''FB.Place
deriveToJSON defaultOptions ''FB.User

class Default a where
  defaultValue :: a

data UserProviderInfo = FacebookInfo FB.User
                      | None

deriveJSON defaultOptions ''UserProviderInfo

type UserInfo a = (a, UserProviderInfo)

data FacebookLoginError = UserEmailEmptyError
                        | CreateSessionError
                        | CreateUserError CreateUserError

deriveJSON defaultOptions ''FacebookLoginError

