{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import DomainSpecific

import Data.Text
import Data.UUID
import Servant
import Servant.Auth as SA
import Servant.Auth.Server

type LoginRespWithCookies = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse

type API' loginAuth commonAuth = "profile" :> Capture "login" Text :> Get '[JSON] ProfileInfo
                            :<|> "login" :> loginAuth :> Get '[JSON] LoginRespWithCookies
                            :<|> "private" :> commonAuth :> Get '[JSON] NoContent
                            :<|> "users" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] ProfileInfo
                            :<|> "user" :> Capture "id" UUID :> Get '[JSON] ProfileInfo
                            :<|> "user" :> commonAuth :> ReqBody '[JSON] ProfileUpdateInfo :> Put '[JSON] NoContent
