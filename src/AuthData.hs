module AuthData where

import Data.Text
import Data.Text.Encoding
import GHC.Generics
import Servant.API.BasicAuth

data AuthData = AuthData
  { login :: Text,
    password :: Text
  }
  deriving (Generic, Read)

basicToAuthData :: BasicAuthData -> AuthData
basicToAuthData (BasicAuthData login pass) = AuthData (decodeUtf8 login) (decodeUtf8 pass)

authToBasicData :: AuthData -> BasicAuthData
authToBasicData (AuthData login pass) = BasicAuthData (encodeUtf8 login) (encodeUtf8 pass)
