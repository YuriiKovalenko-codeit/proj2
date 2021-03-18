{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import DomainSpecific

import Data.Text
import Data.UUID
import Servant

type BasicAuth' = BasicAuth "some-realm" AuthenticatedUser

type API' auth = "profile" :> Capture "login" Text :> Get '[JSON] ProfileInfo
            :<|> BasicAuth' :> "login" :> Get '[JSON] AuthenticatedUser 
            :<|> auth :> "private" :> Get '[JSON] AuthenticatedUser
            :<|> "users" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] ProfileInfo
            :<|> "user" :> Capture "id" UUID :> Get '[JSON] ProfileInfo
