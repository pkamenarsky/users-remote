{-# LANGUAGE TemplateHaskell #-}

module Web.Users.Remote.Types.Shared where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Proxy

import           Data.List                    (intercalate)
import qualified Data.Text                    as T
import           Data.Time.Clock

import qualified Facebook                     as FB

import           Purescript.Interop

import           Web.Users.Types              hiding (UserId)
import           Web.Users.Remote.Types

data Ok = Ok

data UserCommand udata uid sid err
  = VerifySession SessionId (Proxy (Maybe uid))
  | CreateUser T.Text T.Text T.Text (Proxy (Either (CreateUserValidationError err) uid))
  | UpdateUserData sid uid udata (Proxy Bool)
  | AuthUser T.Text T.Text Int (Proxy (Maybe sid))
  | AuthFacebookUrl T.Text [T.Text] (Proxy T.Text)
  | AuthFacebook T.Text [(T.Text, T.Text)] Int (Proxy (Either FacebookLoginError sid))
  | GetUserData sid uid (Proxy (Maybe udata))
  | Logout sid (Proxy Ok)

deriveJSON options ''Ok

deriveJSON options ''Proxy
deriveJSON options ''UserCommand

deriveJSON options ''CreateUserValidationError
deriveJSON options ''CreateUserError
deriveJSON options ''NominalDiffTime
deriveJSON options ''PasswordPlain
deriveJSON options ''Password

deriveJSON options ''FacebookLoginError

mkExports (Just ((intercalate "\n"
  [ "module Web.Users.Remote.Types.Shared where"
  , ""
  , "import Network.WebSockets.Sync.Request"
  , ""
  ])
  , ""
  , "ps/Web/Users/Remote/Types/Shared.purs"))

  [ (''CreateUserError, True)
  , (''FacebookLoginError, True)
  , (''UserCommand, True)
  , (''Password, True)
  , (''SessionId, True)
  , (''Ok, True)
  ]
