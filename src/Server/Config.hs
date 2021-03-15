{-# LANGUAGE DeriveGeneric #-}
module Server.Config where

import GHC.Generics

import Data.Aeson
import Data.Maybe
import Data.Yaml

defaultConfigPath :: FilePath
defaultConfigPath = "config.yaml"

newtype Config = Config
    { keyPath :: FilePath
    } deriving Generic

instance FromJSON Config

loadConfig :: Maybe FilePath -> IO Config
loadConfig mConfigPath = do
    let path = fromMaybe defaultConfigPath mConfigPath
    decodeFileEither path >>= either
        (error . ("Error while parsing the config file: "++) . show)
        return
