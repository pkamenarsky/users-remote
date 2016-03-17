{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.Users.Remote.Types where

import           Data.Aeson.TH
import           Data.Int
import qualified Data.Text                              as T

import qualified Facebook                               as FB

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
  { cmpAccessRights :: udata -> udata -> Ordering
  , validateUserData :: udata -> Either err ()
  , maskUserDataFromClient :: udata -> udata
  , augmentUserDataWithFbUser :: FB.User -> udata -> udata

  , fbCredentials :: FB.Credentials
  , httpManager :: C.Manager
  }

data OAuthProviderInfo = FacebookInfo FB.UserId (Maybe T.Text)
                         deriving Show

data CreateUserValidationError err
  = CreateUserError U.CreateUserError
  | CreateUserValidationError err

data FacebookLoginError err
  = FacebookUserEmailEmptyError
  | FacebookCreateSessionError
  | FacebookCreateUserError CreateUserError
  | FacebookUserValidationError err
