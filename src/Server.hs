{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Server where

import Server.Config

import API
import AuthData
import DomainSpecific
import Log

import Conduit
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Crypto.JOSE
import Data.Aeson
import Data.Maybe
import Data.Password
import Data.Text as T
import Data.UUID
import Data.UUID.V4
import Database.Persist
import Database.Persist.Sql
import Network.Wai.Handler.Warp
import Servant
import qualified Servant.Auth as SA
import Servant.Auth.Server
import Server.DB

lookupUser :: ConnectionPool
           -> AuthData
           -> IO (Maybe AuthenticatedUser)
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

authCheck :: ConnectionPool
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck connPool basicAuth =
    maybe Indefinite Authenticated <$> lookupUser connPool (basicToAuthData basicAuth)

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

defaultSalt :: Salt
defaultSalt = Salt "proj2salt"

hashPassword :: T.Text -> PassHash
hashPassword password = hashPassWithSalt (Pass password) defaultSalt

runOnPool :: (MonadIO m) => ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> ReaderT AppContext m a
runOnPool action = do
    connPool <- asks _connPool
    liftIO $ runSqlPersistMPool action connPool

createUser :: (MonadIO m) => T.Text -> PassHash -> ReaderT SqlBackend m UUID
createUser login passhash = do
    uuid <- liftIO nextRandom
    insert $ User login passhash uuid
    return uuid

getUserProfileInfo :: User -> ProfileInfo
getUserProfileInfo User { userLogin = login, userUuid = userID } = ProfileInfo { .. }

server3 :: ServerT API HandlerT
server3 = getProfileByLogin
     :<|> loginUser
     :<|> private
     :<|> register
     :<|> getProfileByID

    where
        private :: AuthResult AuthenticatedUser -> HandlerT AuthenticatedUser
        private (Authenticated user) = do
            log <- asks _logFn
            liftIO $ log $ "Logged in as userid:" <> toLogStr (show (auID user))
            return . AUser $ auID user
        private _ = do
            log <- asks _logFn
            liftIO $ log "Login attempt failed"
            throwAll err401

        loginUser :: AuthenticatedUser -> HandlerT AuthenticatedUser
        loginUser user = do
            log <- asks _logFn
            liftIO $ log $ "Logged in as userid:" <> toLogStr (show (auID user))
            return . AUser $ auID user

        register :: RegisterRequest -> HandlerT ProfileInfo
        register (RegRequest login password) =
            maybe (throwAll err409) return =<< runOnPool (do
                exists <- userWithLoginExists login
                if exists
                then return Nothing
                else do
                    uuid <- createUser login (hashPassword password)
                    return $ Just $ ProfileInfo login uuid)

        getProfileBy :: Unique User -> HandlerT ProfileInfo
        getProfileBy uniqueConstr =
            maybe (throwAll err404) return =<< runOnPool (do
                mEntity <- getBy uniqueConstr
                return $ getUserProfileInfo . entityVal <$> mEntity)

        getProfileByLogin :: Text -> HandlerT ProfileInfo
        getProfileByLogin = getProfileBy . UniqueLogin

        getProfileByID :: UUID -> HandlerT ProfileInfo
        getProfileByID = getProfileBy . UniqueUUID

readJWK :: FilePath -> IO JWK
readJWK keyPath = eitherDecodeFileStrict' keyPath >>= either
    (error . ("Error while reading key file: " ++) . show)
    return

mkApp :: AppContext -> IO Application
mkApp appctx = do
    jwk <- readJWK $ keyPath $ _config appctx
    _logFn appctx $ toLogStr $ encode jwk
    let jwtCfg = (defaultJWTSettings jwk) { jwtAlg = Just ES256 }
        connPool = _connPool appctx
        cfg = jwtCfg :. defaultCookieSettings :. basicAuthCheck connPool :. authCheck connPool :. EmptyContext
        ctx = getCtxProxy cfg
    return $ serveWithContext api cfg $ hoistServerWithContext api ctx (`runReaderT` appctx) server3

    where
        getCtxProxy :: Servant.Context a -> Proxy a
        getCtxProxy _ = Proxy

serverMain :: Logger -> Maybe FilePath -> IO ()
serverMain logger mConfigPath = do
    let logFn = pushLog logger
    connPool <- createConnectionPool connectionString defaultPoolSize logFn
    flip runSqlPool connPool $ do
        runMigration migrateAll
        -- WARNING: for test purposes only!!
        mUser <- getBy $ UniqueLogin "user1"
        when (isNothing mUser) $ do
            createUser "user1" (hashPassword "11")
            return ()
    config <- loadServerConfig mConfigPath
    run 8081 =<< mkApp (AppContext (pushLog logger) config connPool)
