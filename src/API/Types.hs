module API.Types where

import Data.Aeson.Types
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

data RegisterRequest = RegRequest
  { login :: Text,
    password :: Text
  }
  deriving (Show, Generic)

instance ToJSON RegisterRequest

instance FromJSON RegisterRequest

data ProfileUpdateInfo = ProfileUpdInfo
  { newLogin :: Maybe Text,
    newPassword :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON ProfileUpdateInfo

instance FromJSON ProfileUpdateInfo

data ProfileInfo = ProfileInfo
  { login :: Text,
    userID :: UUID
  }
  deriving (Show, Generic)

instance ToJSON ProfileInfo

instance FromJSON ProfileInfo