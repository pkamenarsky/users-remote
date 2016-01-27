module Web.Users.Remote.Client where

import           Data.Aeson
import qualified Data.ByteString             as BS
import           Data.MessagePack.Aeson
import qualified Data.Text                    as T
import           Data.Time.Clock

import qualified Facebook                     as FB

import           Network.MessagePack.Client

import           Web.Users.Types              hiding (UserId)
import           Web.Users.Remote.Types

getUserIdByName' :: T.Text -> Client (Maybe UserId)
getUserIdByName' a = getAsMessagePack <$> call "getUserIdByName" a

createUser' :: (FromJSON a, ToJSON a) => User a -> Client (Either CreateUserError UserId)
createUser' u = getAsMessagePack <$> call "createUser" (AsMessagePack u) (AsMessagePack $ u_password u)

getUserById' :: (FromJSON a, ToJSON a) => UserId -> Client (Maybe (User (UserInfo a)))
getUserById' uid = getAsMessagePack <$> call "getUserById" (AsMessagePack uid)

authUser' :: T.Text -> PasswordPlain -> NominalDiffTime -> Client (Maybe SessionId)
authUser' a b c = getAsMessagePack <$> call "authUser" a (AsMessagePack b) (AsMessagePack c)

verifySession' :: SessionId -> NominalDiffTime -> Client (Maybe UserId)
verifySession' sid t = getAsMessagePack <$> call "verifySession" (AsMessagePack sid) (AsMessagePack t)

facebookLoginUrl :: T.Text -> [T.Text] -> Client T.Text
facebookLoginUrl url perms = call "facebookLoginUrl" url (AsMessagePack perms)

facebookLogin :: T.Text -> [(BS.ByteString, BS.ByteString)] -> NominalDiffTime -> Client FB.UserAccessToken
facebookLogin url args t = getAsMessagePack <$> call "facebookLogin" url args (AsMessagePack t)
