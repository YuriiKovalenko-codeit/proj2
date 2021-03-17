{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies #-}

module Server where

import Server.Config

import API
import DomainSpecific
import Log

import Data.Aeson
import Data.ByteString as BS
import Data.Map as M
import Data.Maybe
import Data.Text.Encoding as E
import Database.Persist
import Database.Persist.Sql
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.JOSE
import Crypto.JOSE.JWK
import Network.Wai.Handler.Warp
import Servant
import qualified Servant.Auth as SA
import Servant.Auth.Server
import System.Log.FastLogger
import Server.DB
import Data.UUID.V4

lookupUser :: ConnectionPool
           -> BS.ByteString -- login
           -> BS.ByteString -- password hash
           -> IO (Maybe AuthenticatedUser)
lookupUser connPool username password = flip runSqlPool connPool $ do
    mEntity <- getBy $ UserLoginPassword (E.decodeUtf8 username) (E.decodeUtf8 password) -- TODO: these decodeUtf8 calls aren't ment to be there
    return $ AUser . userUuid . entityVal <$> mEntity

authCheck :: ConnectionPool
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck connPool (BasicAuthData username password) = maybe Indefinite Authenticated <$> lookupUser connPool username password

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
    fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type API = API' (Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser)

data AppContext = AppContext
    { _logFn :: LogStr -> IO ()
    , _config :: Config
    , _connPool :: ConnectionPool
    }

type HandlerT = ReaderT AppContext Handler

api :: Proxy API
api = Proxy

server3 :: ServerT API HandlerT
server3 = position
     :<|> hello
     :<|> marketing
     :<|> private

    where position :: Int -> Int -> HandlerT Position
          position x y = return (Position x y)

          hello :: Maybe String -> HandlerT HelloMessage
          hello mname = return . HelloMessage $ case mname of
              Nothing -> "Hello, anon"
              Just name -> "Hello, " ++ name

          marketing :: ClientInfo -> HandlerT Email
          marketing = return . emailForClient

          private :: AuthResult AuthenticatedUser -> HandlerT PrivateInfo
          private (Authenticated user) = do
              log <- asks _logFn
              liftIO $ log $ "Logged in as userid:" <> toLogStr (show (auID user))
              return . PrivateInfo $ auID user
          private _ = do
              log <- asks _logFn
              liftIO $ log "Login attempt failed"
              throwAll err401

readJWK :: FilePath -> IO JWK
readJWK keyPath = eitherDecodeFileStrict' keyPath >>= either
    (error . ("Error while reading key file: " ++) . show)
    return

mkApp :: AppContext -> IO Application
mkApp appctx = do
    jwk <- readJWK $ keyPath $ _config appctx
    _logFn appctx $ toLogStr $ encode jwk
    let jwtCfg = (defaultJWTSettings jwk) { jwtAlg = Just ES256 }
        authCfg = authCheck $ _connPool appctx
        cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
        ctx = getCtxProxy cfg
    return $ serveWithContext api cfg $ hoistServerWithContext api ctx (`runReaderT` appctx) server3

    where
        getCtxProxy :: Servant.Context a -> Proxy a
        getCtxProxy _ = Proxy

serverMain :: Logger -> Maybe FilePath -> IO ()
serverMain logger mConfigPath = do
    let logFn = pushLog logger
    connPool <- createConnectionPool connectionString defaultPoolSize logFn
    uuid <- nextRandom
    flip runSqlPool connPool $ do
        runMigration migrateAll
        -- WARNING: for test purposes only!!
        mUser <- getBy $ UserLoginPassword "user1" "11"
        when (isNothing mUser) $ do
            Database.Persist.Sql.insert $ User "user1" "11" uuid
            return ()
    config <- loadServerConfig mConfigPath
    run 8081 =<< mkApp (AppContext (pushLog logger) config connPool)
