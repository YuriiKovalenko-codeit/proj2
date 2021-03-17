{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DomainSpecific where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Map
import Data.UUID
import GHC.Generics
import Servant.Auth.Server

data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    } deriving (Generic, Show)

instance ToJSON Position
instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
    deriving (Generic, Show)

instance ToJSON HelloMessage
instance FromJSON HelloMessage

data ClientInfo = ClientInfo
    { clientName :: String
    , clientEmail :: String
    , clientAge :: Int
    , clientInterestedIn :: [String]
    } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
    { from :: String
    , to :: String
    , subject :: String
    , body :: String
    } deriving (Generic, Show)

instance ToJSON Email
instance FromJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
    where from' = "company@example.com"
          to' = clientEmail c
          subject' = "Notification for " ++ clientName c
          body' = "Hi " ++ clientName c ++ ",\n\n"
               ++ "Since you've recently turned " ++ show (clientAge c)
               ++ ", have you checked out our latest "
               ++ intercalate ", " (clientInterestedIn c)
               ++ " products? Give us a visit!"

newtype PrivateInfo = PrivateInfo { userID :: UserId }
    deriving (Generic, Show)

instance ToJSON PrivateInfo
instance FromJSON PrivateInfo

type UserId = UUID

newtype AuthenticatedUser = AUser
    { auID :: UserId
    } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser