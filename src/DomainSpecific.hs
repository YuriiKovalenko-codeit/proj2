{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module DomainSpecific where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.UUID
import GHC.Generics
import Servant.Auth.Server

type UserId = UUID

newtype AuthenticatedUser = AUser
    { auID :: UserId
    } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

data RegisterRequest = RegRequest
    { login :: T.Text
    , password :: T.Text
    } deriving (Show, Generic)

instance ToJSON RegisterRequest
instance FromJSON RegisterRequest

data ProfileInfo = ProfileInfo
    { login :: T.Text
    , userID :: UserId
    } deriving (Show, Generic)

instance ToJSON ProfileInfo
instance FromJSON ProfileInfo