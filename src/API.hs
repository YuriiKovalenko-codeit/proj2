{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import DomainSpecific

import Servant

type API' auth = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
            :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
            :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
            :<|> "private" :> auth :> Get '[JSON] PrivateInfo
