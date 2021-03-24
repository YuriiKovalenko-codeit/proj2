module Server.Config where

import Crypto.JOSE.JWK
import Data.Aeson
import Data.Maybe
import Data.Password
import Data.Yaml
import GHC.Generics

defaultSalt :: Salt
defaultSalt = Salt "proj2salt"

defaultConfigPath :: FilePath
defaultConfigPath = "config.yaml"

newtype Config = Config
  { keyPath :: FilePath
  }
  deriving (Generic)

instance FromJSON Config

loadServerConfig :: Maybe FilePath -> IO Config
loadServerConfig mConfigPath = do
  let path = fromMaybe defaultConfigPath mConfigPath
  decodeFileEither path
    >>= either
      (error . ("Error while parsing the config file: " ++) . show)
      return

loadJWK :: FilePath -> IO JWK
loadJWK keyPath =
  eitherDecodeFileStrict' keyPath
    >>= either
      (error . ("Error while reading key file: " ++) . show)
      return
