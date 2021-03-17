{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Server.DB where

import Server.PersistInstances

import DomainSpecific

import Data.ByteString.Char8 as BS8
import Data.Composition
import qualified Data.Text as T
import Data.UUID
import Database.Persist.Sql.Types.Internal
import Database.Persist.TH
import Database.Persist.Postgresql
import Conduit
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Database.Persist.Postgresql as P

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    login T.Text
    passwordHash T.Text
    uuid UUID
    UserLoginPassword login passwordHash
    UserUUID uuid
|]

connectionString :: ConnectionString
connectionString = "postgresql://postgres:password@localhost:5432/postgres"

defaultPoolSize :: Int
defaultPoolSize = 4

createConnectionPool :: ConnectionString
                     -> Int -- pool size
                     -> (LogStr -> IO ()) -- log function
                     -> IO ConnectionPool
createConnectionPool connStr poolSize logFn = runLoggingT (createPostgresqlPool connStr poolSize) (logFn .:: defaultLogStr)


-- WARNING next some playground code goes, will be deleted later

runPostgreSQL :: MonadUnliftIO m => ConnectionString -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runPostgreSQL connStr = runResourceT
                      . runNoLoggingT
                      . withPostgresqlConn connStr
                      . runSqlConn

runPostgreSQLPool :: MonadUnliftIO m => ConnectionString -> Int -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runPostgreSQLPool connStr poolSize = runResourceT
                                   . runNoLoggingT
                                   . withPostgresqlPool connStr poolSize
                                   . runSqlPool

withPool :: MonadUnliftIO m
         => ConnectionString
         -> Int
         -> (ConnectionPool -> NoLoggingT (ResourceT m) a)
         -> m a
withPool = runResourceT
         . runNoLoggingT
       .:. withPostgresqlPool 

test :: (ConnectionPool -> IO a) -> IO a
test actionWithPool = withPool "" 2 (liftIO . actionWithPool)

test' :: IO ConnectionPool
test' = test return
