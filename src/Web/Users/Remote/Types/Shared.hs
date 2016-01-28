{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Types.Shared where

import           Data.Aeson.TH
import           Data.Proxy

import qualified Data.Text                    as T

import qualified Facebook                     as FB

import           Purescript.Interop

import           Web.Users.Types              hiding (UserId)
import           Web.Users.Remote.Types

data UserCommand uinfo uid sid
  = VerifySession SessionId (Proxy (Maybe uid))
  | CreateUser (User uinfo) T.Text (Proxy (Either CreateUserError uid))
  | AuthUser T.Text T.Text Int (Proxy (Maybe sid))
  | AuthFacebookUrl T.Text [T.Text] (Proxy T.Text)
  | AuthFacebook T.Text [(T.Text, T.Text)] Int (Proxy (Either FacebookLoginError sid))
  | GetUserById uid (Proxy (Maybe (User (UserInfo uinfo))))
  | Logout SessionId (Proxy ())

deriveJSON defaultOptions ''Proxy
deriveJSON defaultOptions ''UserCommand

mkExports (Just ("module Web.Users.Remote.Types.Shared where"
                , "ps/Web/Users/Remote/Types/Shared.purs"))
  [ ''CreateUserError
  , ''FacebookLoginError
  , ''UserCommand
  , ''User
  , ''Password
  , ''Proxy
  , ''SessionId
  , ''UserInfo
  , ''UserProviderInfo

  , ''FB.GeoCoordinates
  , ''FB.Location
  , ''FB.Place
  , ''FB.User
  ]
