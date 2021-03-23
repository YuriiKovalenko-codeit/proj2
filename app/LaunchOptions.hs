{-# LANGUAGE DeriveGeneric #-}

module LaunchOptions where

import Data.Text
import Options.Generic

data LaunchOptions
  = Client {username :: Text, password :: Text}
  | Server {configPath :: Maybe FilePath}
  | KeyGen {output :: FilePath}
  deriving (Generic)

instance ParseRecord LaunchOptions

parseLaunchOptions :: Text -> IO LaunchOptions
parseLaunchOptions = getRecord
