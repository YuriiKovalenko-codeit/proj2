module AuthenticatedUser where

import Data.Aeson
import Data.UUID
import GHC.Generics
import Servant.Auth.Server

newtype AuthenticatedUser = AUser
  { auID :: UUID
  }
  deriving (Show, Generic)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser
