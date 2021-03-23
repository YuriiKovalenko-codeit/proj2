-- TODO: Move extensions usage into package.yaml
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Text
import Data.UUID
import DomainSpecific
import Servant
import Servant.Auth as SA
import Servant.Auth.Server

-- TODO: unnescessary type parameters. Move type param values here and make complete API definition here.
type API' loginAuth commonAuth =
  "profile" :> Capture "login" Text :> Get '[JSON] ProfileInfo
    :<|> "login" :> loginAuth :> Post '[JSON] LoginResponse
    :<|> "private" :> commonAuth :> Get '[JSON] NoContent -- TODO: what is the function of this endpoint.
    :<|> "users" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] ProfileInfo -- TODO: Not a single user should be able to get other users profiles.
    :<|> "user" :> Capture "id" UUID :> Get '[JSON] ProfileInfo -- TODO: you can get UserId from AuthResult. `Capture "id" UUID` could be removed.
    :<|> "user" :> commonAuth :> ReqBody '[JSON] ProfileUpdateInfo :> Put '[JSON] NoContent
