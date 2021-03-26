module Server.Endpoints.PublicProfile where

import API.Types
import Data.Text
import Server.DB
import Server.Helpers
import Server.Types

getPublicProfile :: Text -> HandlerT ProfileInfo
getPublicProfile = getProfileBy . UniqueLogin