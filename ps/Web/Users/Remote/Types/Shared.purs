module Web.Users.Remote.Types.Shared where

import Network.WebSockets.Sync.Request


import Data.JSON
import Data.Either
import Data.Maybe
import Data.List (List ())
import Data.Tuple
import Data.Set (Set ())
import Control.Monad.Aff
import Prelude

type Text = String

data CreateUserValidationError err = CreateUserError CreateUserError
|
CreateUserValidationError err


instance createUserValidationErrorToJson :: (ToJSON err) =>  ToJSON (CreateUserValidationError err) where
  toJSON (CreateUserError x0) = object $
    [ "tag" .= "CreateUserError"
    , "contents" .= toJSON x0
    ]
  toJSON (CreateUserValidationError x0) = object $
    [ "tag" .= "CreateUserValidationError"
    , "contents" .= toJSON x0
    ]


instance createUserValidationErrorFromJson :: (FromJSON err) =>  FromJSON (CreateUserValidationError err) where
  parseJSON (JObject o) = do
    tag <- o .: "tag"
    case tag of
      "CreateUserError" -> do
         x0 <- o .: "contents"
         CreateUserError <$> parseJSON x0

      "CreateUserValidationError" -> do
         x0 <- o .: "contents"
         CreateUserValidationError <$> parseJSON x0



data CreateUserError  = InvalidPassword 
|
UsernameAlreadyTaken 
|
EmailAlreadyTaken 
|
UsernameAndEmailAlreadyTaken 


instance createUserErrorToJson ::  ToJSON (CreateUserError ) where
  toJSON (InvalidPassword ) = object $
    [ "tag" .= "InvalidPassword"
    , "contents" .= ([] :: Array String)
    ]
  toJSON (UsernameAlreadyTaken ) = object $
    [ "tag" .= "UsernameAlreadyTaken"
    , "contents" .= ([] :: Array String)
    ]
  toJSON (EmailAlreadyTaken ) = object $
    [ "tag" .= "EmailAlreadyTaken"
    , "contents" .= ([] :: Array String)
    ]
  toJSON (UsernameAndEmailAlreadyTaken ) = object $
    [ "tag" .= "UsernameAndEmailAlreadyTaken"
    , "contents" .= ([] :: Array String)
    ]


instance createUserErrorFromJson ::  FromJSON (CreateUserError ) where
  parseJSON (JObject o) = do
    tag <- o .: "tag"
    case tag of
      "InvalidPassword" -> do
         return InvalidPassword

      "UsernameAlreadyTaken" -> do
         return UsernameAlreadyTaken

      "EmailAlreadyTaken" -> do
         return EmailAlreadyTaken

      "UsernameAndEmailAlreadyTaken" -> do
         return UsernameAndEmailAlreadyTaken



data FacebookLoginError  = FacebookUserEmailEmptyError 
|
FacebookCreateSessionError 
|
FacebookCreateUserError CreateUserError


instance facebookLoginErrorToJson ::  ToJSON (FacebookLoginError ) where
  toJSON (FacebookUserEmailEmptyError ) = object $
    [ "tag" .= "FacebookUserEmailEmptyError"
    , "contents" .= ([] :: Array String)
    ]
  toJSON (FacebookCreateSessionError ) = object $
    [ "tag" .= "FacebookCreateSessionError"
    , "contents" .= ([] :: Array String)
    ]
  toJSON (FacebookCreateUserError x0) = object $
    [ "tag" .= "FacebookCreateUserError"
    , "contents" .= toJSON x0
    ]


instance facebookLoginErrorFromJson ::  FromJSON (FacebookLoginError ) where
  parseJSON (JObject o) = do
    tag <- o .: "tag"
    case tag of
      "FacebookUserEmailEmptyError" -> do
         return FacebookUserEmailEmptyError

      "FacebookCreateSessionError" -> do
         return FacebookCreateSessionError

      "FacebookCreateUserError" -> do
         x0 <- o .: "contents"
         FacebookCreateUserError <$> parseJSON x0



data UserCommand udata uid sid err = VerifySession SessionId (Proxy (Maybe uid))
|
CreateUser Text Text Text (Proxy ((Either (CreateUserValidationError err)) uid))
|
UpdateUserData sid uid udata (Proxy Boolean)
|
AuthUser Text Text Int (Proxy (Maybe sid))
|
AuthFacebookUrl Text (Array  Text) (Proxy Text)
|
AuthFacebook Text (Array  ((Tuple  Text) Text)) Int (Proxy ((Either FacebookLoginError) sid))
|
GetUserData sid uid (Proxy (Maybe udata))
|
Logout sid (Proxy Ok)


instance userCommandToJson :: (ToJSON udata, ToJSON uid, ToJSON sid, ToJSON err) =>  ToJSON (UserCommand udata uid sid err) where
  toJSON (VerifySession x0 x1) = object $
    [ "tag" .= "VerifySession"
    , "contents" .= [toJSON x0, toJSON x1]
    ]
  toJSON (CreateUser x0 x1 x2 x3) = object $
    [ "tag" .= "CreateUser"
    , "contents" .= [toJSON x0, toJSON x1, toJSON x2, toJSON x3]
    ]
  toJSON (UpdateUserData x0 x1 x2 x3) = object $
    [ "tag" .= "UpdateUserData"
    , "contents" .= [toJSON x0, toJSON x1, toJSON x2, toJSON x3]
    ]
  toJSON (AuthUser x0 x1 x2 x3) = object $
    [ "tag" .= "AuthUser"
    , "contents" .= [toJSON x0, toJSON x1, toJSON x2, toJSON x3]
    ]
  toJSON (AuthFacebookUrl x0 x1 x2) = object $
    [ "tag" .= "AuthFacebookUrl"
    , "contents" .= [toJSON x0, toJSON x1, toJSON x2]
    ]
  toJSON (AuthFacebook x0 x1 x2 x3) = object $
    [ "tag" .= "AuthFacebook"
    , "contents" .= [toJSON x0, toJSON x1, toJSON x2, toJSON x3]
    ]
  toJSON (GetUserData x0 x1 x2) = object $
    [ "tag" .= "GetUserData"
    , "contents" .= [toJSON x0, toJSON x1, toJSON x2]
    ]
  toJSON (Logout x0 x1) = object $
    [ "tag" .= "Logout"
    , "contents" .= [toJSON x0, toJSON x1]
    ]


instance userCommandFromJson :: (FromJSON udata, FromJSON uid, FromJSON sid, FromJSON err) =>  FromJSON (UserCommand udata uid sid err) where
  parseJSON (JObject o) = do
    tag <- o .: "tag"
    case tag of
      "VerifySession" -> do
         [x0, x1] <- o .: "contents"
         VerifySession <$> parseJSON x0 <*> parseJSON x1

      "CreateUser" -> do
         [x0, x1, x2, x3] <- o .: "contents"
         CreateUser <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2 <*> parseJSON x3

      "UpdateUserData" -> do
         [x0, x1, x2, x3] <- o .: "contents"
         UpdateUserData <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2 <*> parseJSON x3

      "AuthUser" -> do
         [x0, x1, x2, x3] <- o .: "contents"
         AuthUser <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2 <*> parseJSON x3

      "AuthFacebookUrl" -> do
         [x0, x1, x2] <- o .: "contents"
         AuthFacebookUrl <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2

      "AuthFacebook" -> do
         [x0, x1, x2, x3] <- o .: "contents"
         AuthFacebook <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2 <*> parseJSON x3

      "GetUserData" -> do
         [x0, x1, x2] <- o .: "contents"
         GetUserData <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2

      "Logout" -> do
         [x0, x1] <- o .: "contents"
         Logout <$> parseJSON x0 <*> parseJSON x1



data Password  = PasswordHash Text
|
PasswordHidden 


instance passwordToJson ::  ToJSON (Password ) where
  toJSON (PasswordHash x0) = object $
    [ "tag" .= "PasswordHash"
    , "contents" .= toJSON x0
    ]
  toJSON (PasswordHidden ) = object $
    [ "tag" .= "PasswordHidden"
    , "contents" .= ([] :: Array String)
    ]


instance passwordFromJson ::  FromJSON (Password ) where
  parseJSON (JObject o) = do
    tag <- o .: "tag"
    case tag of
      "PasswordHash" -> do
         x0 <- o .: "contents"
         PasswordHash <$> parseJSON x0

      "PasswordHidden" -> do
         return PasswordHidden



newtype SessionId  = SessionId {
  unSessionId :: Text
}

instance sessionIdToJson ::  ToJSON (SessionId ) where
  toJSON (SessionId v) = object $
    [ "tag" .= "SessionId"
    , "unSessionId" .= v.unSessionId
    ]


instance sessionIdFromJson ::  FromJSON (SessionId ) where
  parseJSON (JObject o) = do
        unSessionId <- o .: "unSessionId"
        return $ SessionId { unSessionId : unSessionId }


data Ok  = Ok 


instance okToJson ::  ToJSON (Ok ) where
  toJSON (Ok ) = object $
    [ "tag" .= "Ok"
    , "contents" .= ([] :: Array String)
    ]


instance okFromJson ::  FromJSON (Ok ) where
  parseJSON _ = do
         return Ok

