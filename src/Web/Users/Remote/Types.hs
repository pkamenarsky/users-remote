{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.Users.Remote.Types where

import           Data.Aeson.TH
import           Data.Int
import qualified Data.Text                              as T

import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

import qualified Facebook                               as FB

import           Web.Users.Types                        hiding (UserId)

type UserId = Int64

options = defaultOptions { allNullaryToStringTag = False }

deriving instance FromField FB.Id
deriving instance ToField FB.Id

data OAuthProviderInfo = FacebookInfo FB.UserId (Maybe T.Text)
                         deriving Show

data FacebookLoginError = UserEmailEmptyError
                        | CreateSessionError
                        | CreateUserError CreateUserError
