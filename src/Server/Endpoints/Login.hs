{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Endpoints.Login where

import API.Types
import AuthenticatedUser
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Lazy as BL
import Data.Text.Encoding as T
import Servant.Auth.Server
import Servant.Server
import Server.Helpers
import Server.Types

loginUser :: JWTSettings -> AuthResult AuthenticatedUser -> HandlerT LoginResponse
loginUser jwtSettings (Authenticated user@(AUser uuid)) = do
  log <- asks _logFn
  liftIO $ log $ "Logged in as userid:" <> toLogStr (show uuid)
  liftIO (makeJWT user jwtSettings Nothing) >>= \case
    Left err -> reportHandlerError err500 "Failed to make a JWT"
    Right jwt -> return $ LoginResponse uuid $ T.decodeUtf8 $ BL.toStrict jwt
loginUser _ _ = reportHandlerError err401 "Wrong login attempt"