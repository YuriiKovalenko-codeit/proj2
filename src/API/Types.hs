module API.Types where

import Data.Aeson.Types
import Data.Swagger.Schema
import Data.Text
import Data.UUID
import GHC.Generics

data LoginResponse = LoginResponse
  { userID :: UUID,
    token :: Text
  }
  deriving (Show, Generic)

instance ToJSON LoginResponse

instance FromJSON LoginResponse

instance ToSchema LoginResponse

data RegisterRequest = RegRequest
  { login :: Text,
    password :: Text
  }
  deriving (Show, Generic)

instance ToJSON RegisterRequest

instance FromJSON RegisterRequest

instance ToSchema RegisterRequest

data ProfileUpdateInfo = ProfileUpdInfo
  { newLogin :: Maybe Text,
    newPassword :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON ProfileUpdateInfo

instance FromJSON ProfileUpdateInfo

instance ToSchema ProfileUpdateInfo

data ProfileInfo = ProfileInfo
  { login :: Text,
    userID :: UUID
  }
  deriving (Show, Generic)

instance ToJSON ProfileInfo

instance FromJSON ProfileInfo

instance ToSchema ProfileInfo
