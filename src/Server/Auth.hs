module Server.Auth where

import AuthData
import AuthenticatedUser
import Data.Password
import Database.Persist.Sql
import Servant.API
import Servant.Auth.Server
import Servant.Server
import Server.DB

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
