-- TODO: Move extensions usage into package.yaml
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import API.Types
import Data.Text
import Servant.API

-- TODO: unnescessary type parameters. Move type param values here and make complete API definition here.
type API' loginAuth commonAuth =
  "profile" :> Capture "login" Text :> Get '[JSON] ProfileInfo
    :<|> "login" :> loginAuth :> Post '[JSON] LoginResponse
    :<|> "auth-test" :> commonAuth :> Get '[JSON] NoContent -- TODO: what is the function of this endpoint.
    :<|> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] ProfileInfo
    :<|> "user" :> commonAuth :> Get '[JSON] ProfileInfo
    :<|> "user" :> commonAuth :> ReqBody '[JSON] ProfileUpdateInfo :> Put '[JSON] NoContent
