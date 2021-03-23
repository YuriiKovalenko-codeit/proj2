{-# LANGUAGE OverloadedStrings #-}

module Server.Endpoints.AuthTest where

import AuthenticatedUser
import Servant.API
import Servant.Auth.Server
import Servant.Server
import Server.Helpers
import Server.Types

authTest :: AuthResult AuthenticatedUser -> HandlerT NoContent
authTest (Authenticated _) = return NoContent
authTest _ = reportHandlerError err401 "private: unauthorized access attempt"