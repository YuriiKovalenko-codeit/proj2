{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Server where

import API
import AuthData
import AuthenticatedUser
import Control.Monad.Logger
import Control.Monad.Reader
import Crypto.JOSE
import Data.Aeson
import Data.Password
import Database.Persist
import Database.Persist.Sql
import Log
import Network.Wai.Handler.Warp
import Servant hiding (Unique)
import qualified Servant.Auth as SA
import Servant.Auth.Server
import Server.Config
import Server.DB
import Server.Endpoints.AuthTest
import Server.Endpoints.Login
import Server.Endpoints.PrivateProfile
import Server.Endpoints.PublicProfile
import Server.Endpoints.Register
import Server.Endpoints.UpdateProfile
import Server.Types

lookupUser ::
  ConnectionPool ->
  AuthData ->
  IO (Maybe AuthenticatedUser)
lookupUser connPool (AuthData username password) = flip runSqlPool connPool $ do
  mEntity <- getBy $ UniqueLogin username
  let mUser = entityVal <$> mEntity
      hashFromDB = userPasswordHash <$> mUser
  case checkPass (Pass password) <$> hashFromDB of
    Just PassCheckSucc -> return $ AUser . userUuid <$> mUser
    _ -> return Nothing

basicAuthCheck :: ConnectionPool -> BasicAuthCheck AuthenticatedUser
basicAuthCheck connPool = BasicAuthCheck $ \basicAuth ->
  maybe Unauthorized Authorized <$> lookupUser connPool (basicToAuthData basicAuth)

authCheck ::
  ConnectionPool ->
  BasicAuthData ->
  IO (AuthResult AuthenticatedUser)
authCheck connPool basicAuth =
  maybe Indefinite Authenticated <$> lookupUser connPool (basicToAuthData basicAuth)

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type LoginAuth = Auth '[SA.BasicAuth] AuthenticatedUser

type CommonAuth = Auth '[SA.BasicAuth, SA.JWT] AuthenticatedUser

type API = API' LoginAuth CommonAuth

api :: Proxy API
api = Proxy

-- TOOD: Split endpoint implementations into separate andpoints modules
server3 :: JWTSettings -> ServerT API HandlerT
server3 jwtSetting =
  getPublicProfile
    :<|> loginUser jwtSetting
    :<|> authTest
    :<|> register
    :<|> getPrivateProfile
    :<|> updateProfile

-- TODO: Split Servant Server implementation and Application creation.
mkApp :: AppContext -> IO Application
mkApp appctx = do
  jwk <- loadJWK $ keyPath $ _config appctx
  _logFn appctx $ toLogStr $ encode jwk
  let jwtCfg = (defaultJWTSettings jwk) {jwtAlg = Just ES256}
      cookieCfg = defaultCookieSettings
      connPool = _connPool appctx
      cfg = jwtCfg :. cookieCfg :. basicAuthCheck connPool :. authCheck connPool :. EmptyContext
      ctx = getCtxProxy cfg
  return $ serveWithContext api cfg $ hoistServerWithContext api ctx (`runReaderT` appctx) $ server3 jwtCfg
  where
    getCtxProxy :: Servant.Context a -> Proxy a
    getCtxProxy _ = Proxy

serverMain :: Maybe FilePath -> IO ()
serverMain mConfigPath = do
  (logger, cleanupLogger) <- initLogger
  let logFn = pushLog logger
  connPool <- createConnectionPool connectionString defaultPoolSize logFn
  flip runSqlPool connPool $ do
    runMigration migrateAll
  config <- loadServerConfig mConfigPath
  -- TODO: port should go to config
  run 8081 =<< mkApp (AppContext (pushLog logger) config connPool)
  cleanupLogger
