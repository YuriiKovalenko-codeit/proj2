{-# LANGUAGE OverloadedStrings #-}

module Main where

import Keygen
import LaunchOptions
import Lib
import Server

main :: IO ()
main = do
  options <- parseLaunchOptions "Client/Server web app"
  case options of
    Client username password -> undefined -- $ AuthData username password
    Server configPath -> serverMain configPath
    KeyGen outputPath -> keygenMain outputPath
