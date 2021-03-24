module Main where

import AuthData
import Client
import Keygen
import LaunchOptions
import Server

main :: IO ()
main = do
  options <- parseLaunchOptions "Client/Server web app"
  case options of
    Client username password -> clientMain $ AuthData username password
    Server configPath -> serverMain configPath
    KeyGen outputPath -> keygenMain outputPath
