{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Types.Shared where

import           Data.Aeson.TH
import           Data.Proxy

import qualified Data.Text                    as T

import           Web.Users.Types              hiding (UserId)
import           Web.Users.Remote.Types

data UserCommand = VerifySession SessionId (Proxy (Maybe UserId))
                 | AuthUser T.Text T.Text Int (Proxy (Maybe SessionId))
                 | AuthFacebookUrl T.Text [T.Text] (Proxy T.Text)
                 | AuthFacebook T.Text [(T.Text, T.Text)] Int (Proxy (Either FacebookLoginError SessionId))
                 | Logout SessionId (Proxy ())

deriveJSON defaultOptions ''Proxy
deriveJSON defaultOptions ''UserCommand
