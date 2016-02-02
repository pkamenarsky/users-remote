module Web.Users.Remote.Client where

import           Data.MessagePack.Aeson
import           Data.Time.Clock

import           Network.MessagePack.Client

import           Web.Users.Types              hiding (UserId)
import           Web.Users.Remote.Types
import           Web.Users.Remote.Types.Shared

verifySession' :: SessionId -> NominalDiffTime -> Client (Maybe UserId)
verifySession' sid t = getAsMessagePack <$> call "verifySession" (AsMessagePack sid) (AsMessagePack t)
