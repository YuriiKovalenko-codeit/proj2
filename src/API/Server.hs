module API.Server where

import API
import AuthenticatedUser
import Data.Proxy
import Servant.Auth as SA

type API =
  API'
    (Auth '[SA.BasicAuth] AuthenticatedUser)
    (Auth '[SA.BasicAuth, SA.JWT] AuthenticatedUser)

api :: Proxy API
api = Proxy