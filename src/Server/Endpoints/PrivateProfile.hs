module Server.Endpoints.PrivateProfile where

import API.Types
import AuthenticatedUser
import Control.Monad.Logger
import Control.Monad.Reader
import Servant.Auth.Server
import Servant.Server
import Server.DB
import Server.Helpers
import Server.Types

getPrivateProfile :: AuthResult AuthenticatedUser -> HandlerT ProfileInfo
getPrivateProfile (Authenticated (AUser uuid)) = do
  log <- asks _logFn
  liftIO $ log $ "Viewing private profile of uuid:" <> toLogStr (show uuid)
  getProfileBy $ UniqueUUID uuid
getPrivateProfile _ = reportHandlerError err401 "Unauthorized profile edit attempt"