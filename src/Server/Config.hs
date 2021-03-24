module Server.Config where

import Crypto.JOSE.JWK
import Data.Aeson
import Data.ByteString
import Data.Maybe
import Data.Password
import Data.Text
import Data.Text.Encoding
import Data.Yaml
import GHC.Generics
import Network.Wai.Handler.Warp

defaultSalt :: Salt
defaultSalt = Salt "proj2salt"

defaultConfigPath :: FilePath
defaultConfigPath = "config.yaml"

defaultPort :: Port
defaultPort = 8081

defaultPoolSize :: Int
defaultPoolSize = 4

data Config = Config
  { keyPath :: FilePath,
    port :: Port,
    dbConnectionString :: ByteString,
    dbConnPoolSize :: Int,
    hashSalt :: Salt
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config
      <$> v .: "keyPath"
      <*> v .:? "port" .!= defaultPort
      <*> fmap encodeUtf8 (v .: "dbConnectionString")
      <*> v .:? "dbConnPoolSize" .!= defaultPoolSize
      <*> fmap (fmap $ Salt . encodeUtf8) (v .:? "hashSalt") .!= defaultSalt

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
