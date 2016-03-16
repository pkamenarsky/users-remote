{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.Users.Remote.Types where

import           Data.Aeson.TH
import           Data.Int
import qualified Data.Text                              as T

import qualified Network.HTTP.Conduit                   as C

import           Database.PostgreSQL.Simple             (Connection)
import           Database.PostgreSQL.Simple.FromField   (FromField(..))
import           Database.PostgreSQL.Simple.ToField     (ToField(..))

import qualified Facebook                               as FB

import           Web.Users.Types                        hiding (UserId)
import           Web.Users.Types                        as U

type UserId = Int64

options = defaultOptions { allNullaryToStringTag = False }

deriving instance FromField FB.Id
deriving instance ToField FB.Id

data Config udata err = Config
  { defaultUserData :: udata
  , cmpAccessRights :: udata -> udata -> Ordering
  , validateUserData :: udata -> Either err ()

  , fbCredentials :: FB.Credentials
  , httpManager :: C.Manager
  }

data OAuthProviderInfo = FacebookInfo FB.UserId (Maybe T.Text)
                         deriving Show

data CreateUserValidationError err
  = CreateUserError U.CreateUserError
  | CreateUserValidationError err

data FacebookLoginError
  = FacebookUserEmailEmptyError
  | FacebookCreateSessionError
  | FacebookCreateUserError CreateUserError
