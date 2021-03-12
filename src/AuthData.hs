{-# LANGUAGE DeriveGeneric #-}

module AuthData where

import GHC.Generics

data AuthData = AuthData
    { userName :: String
    , password :: String
    } deriving (Generic, Read)
