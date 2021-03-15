{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Application where

import AuthData
import Client
import Log
import Server

import Prelude hiding (log)
import Options.Generic

data LaunchMode = Client { username :: String, password :: String } | Server
    deriving Generic

instance ParseRecord LaunchMode

run :: IO ()
run = do
    mode <- getRecord "Client/Server web app"
    (logger, cleanup) <- initLogger
    pushLog logger "Log system is initialized"
    case mode of
        Client username password -> clientMain $ AuthData username password
        Server -> serverMain logger
    cleanup
