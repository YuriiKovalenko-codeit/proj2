module Server.Helpers where

import API.Types
import Conduit
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Password
import Data.Text
import Database.Persist.Sql
import Servant.Auth.Server
import Servant.Server
import Server.Config
import Server.DB
import Server.Types

hashPassword :: Text -> PassHash
hashPassword password = hashPassWithSalt (Pass password) defaultSalt

runOnPool :: (MonadIO m) => ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> ReaderT AppContext m a
runOnPool action = do
  connPool <- asks _connPool
  liftIO $ runSqlPersistMPool action connPool

getUserProfileInfo :: User -> ProfileInfo
getUserProfileInfo User {userLogin = login, userUuid = userID} = ProfileInfo {..}

-- should be updated every time when ProfileInfo gets a new field
-- maybe there's a better way to do this
getUpdatesList :: ProfileUpdateInfo -> [Update User]
getUpdatesList ProfileUpdInfo {..} =
  Prelude.concat
    [ updateForField UserLogin newLogin,
      updateForField UserPasswordHash (hashPassword <$> newPassword)
    ]
  where
    updateForField field (Just v) = [field =. v]
    updateForField _ _ = []

getProfileBy :: Unique User -> HandlerT ProfileInfo
getProfileBy uniqueConstr =
  maybe (reportHandlerError err404 "query of non-existent user's profile") return
    =<< runOnPool
      ( do
          mEntity <- getBy uniqueConstr
          return $ getUserProfileInfo . entityVal <$> mEntity
      )

reportHandlerError :: ServerError -> LogStr -> HandlerT a
reportHandlerError err msg = do
  log <- asks _logFn
  liftIO $ log msg
  throwAll err