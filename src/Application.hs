{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Application where

import AuthData
import Client
import Keygen
import Log
import Server

import Prelude hiding (log)
import Options.Generic

data LaunchMode = Client { username :: String, password :: String }
                | Server { configPath :: Maybe FilePath }
                | KeyGen { output :: FilePath }
    deriving Generic

instance ParseRecord LaunchMode

run :: IO ()
run = do
    mode <- getRecord "Client/Server web app"
    (logger, cleanup) <- initLogger
    case mode of
        Client username password -> clientMain $ AuthData username password
        Server configPath -> serverMain logger configPath
        KeyGen outputPath -> keygenMain outputPath
    cleanup
