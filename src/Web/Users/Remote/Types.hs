{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Types where

import           Data.Aeson.TH
import           Data.Int
import qualified Data.Text                    as T

import qualified Facebook                     as FB

import           Web.Users.Types              hiding (UserId)

type UserId = Int64

options = defaultOptions { allNullaryToStringTag = False }

data OAuthProviderInfo = FacebookInfo FB.UserId (Maybe T.Text)
                         deriving Show

data FacebookLoginError = UserEmailEmptyError
                        | CreateSessionError
                        | CreateUserError CreateUserError
