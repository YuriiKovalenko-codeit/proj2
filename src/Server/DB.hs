{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.DB where

import Conduit
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Composition
import Data.Maybe
import Data.Password
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4
import Database.Persist.Postgresql
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH
import Server.PersistInstances

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    login T.Text
    passwordHash PassHash
    uuid UUID
    UniqueLogin login
    UniqueUUID uuid
|]

connectionString :: ConnectionString
connectionString = "postgresql://postgres:password@localhost:5432/postgres"

defaultPoolSize :: Int
defaultPoolSize = 4

createConnectionPool ::
  ConnectionString ->
  Int -> -- pool size
  (LogStr -> IO ()) -> -- log function
  IO ConnectionPool
createConnectionPool connStr poolSize logFn = runLoggingT (createPostgresqlPool connStr poolSize) (logFn .:: defaultLogStr)

userWithLoginExists ::
  MonadIO m =>
  T.Text -> -- login
  ReaderT SqlBackend m Bool
userWithLoginExists login = fmap isJust $ getBy $ UniqueLogin login

createUser :: (MonadIO m) => T.Text -> PassHash -> ReaderT SqlBackend m UUID
createUser login passhash = do
  uuid <- liftIO nextRandom
  insert $ User login passhash uuid
  return uuid
