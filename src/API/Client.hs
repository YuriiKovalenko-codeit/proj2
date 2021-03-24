module API.Client where

import API
import AuthenticatedUser
import Data.Proxy
import qualified Servant.API as SAPI
import Servant.Auth as SA
import Servant.Auth.Client

type API =
  API'
    (SAPI.BasicAuth "some-realm" AuthenticatedUser)
    (Auth '[SA.BasicAuth, Bearer] AuthenticatedUser)

api :: Proxy API
api = Proxy