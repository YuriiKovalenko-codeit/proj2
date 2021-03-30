module API.Server where

import API
import AuthenticatedUser
import Data.Proxy
import Data.Swagger
import Servant.API
import Servant.Auth as SA
import Servant.Auth.Swagger
import Servant.Swagger
import Servant.Swagger.UI

type API = SwaggerAPI :<|> BasicAPI

api :: Proxy API
api = Proxy

type BasicAPI =
  API'
    (Auth '[SA.BasicAuth] AuthenticatedUser)
    (Auth '[SA.BasicAuth, SA.JWT] AuthenticatedUser)

basicAPI :: Proxy BasicAPI
basicAPI = Proxy

type SwaggerAPI = SwaggerSchemaUI "swagger.json" "swagger-ui"

swaggerDoc :: Swagger
swaggerDoc = toSwagger basicAPI
