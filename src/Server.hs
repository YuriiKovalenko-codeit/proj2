module Server where

import API.Server
import Control.Monad.Logger
import Control.Monad.Reader
import Crypto.JOSE
import Data.Aeson
import Database.Persist.Sql
import Log
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.Auth.Server
import Server.Auth
import Server.Config
import Server.DB
import Server.Endpoints.AuthTest
import Server.Endpoints.Login
import Server.Endpoints.PrivateProfile
import Server.Endpoints.PublicProfile
import Server.Endpoints.Register
import Server.Endpoints.UpdateProfile
import Server.Middleware
import Server.Types

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
  config <- loadServerConfig mConfigPath
  connPool <- createConnectionPool (dbConnectionString config) (dbConnPoolSize config) logFn
  flip runSqlPool connPool $ do
    runMigration migrateAll
  middlewares <- mkMiddlewares
  app <- mkApp (AppContext (pushLog logger) config connPool)
  run (port config) $ chain middlewares app
  cleanupLogger
