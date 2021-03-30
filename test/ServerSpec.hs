module ServerSpec where

import Data.String
import qualified Data.UUID as U
import Database.Persist.Sql hiding (get)
import Helpers.DB
import Log
import Servant
import Server
import Server.Config
import Server.DB
import Server.Types
import Test.Hspec
import Test.Hspec.Wai

restoreDBHook :: (ConnectionPool, Application) -> IO (ConnectionPool, Application)
restoreDBHook (connPool, app) = restoreDB connPool >> return (connPool, app)

createPoolAndApp :: IO (ConnectionPool, Application)
createPoolAndApp = withLogFn $ \log -> do
  config <- getTestConfig
  connPool <- createConnectionPool (dbConnectionString config) (dbConnPoolSize config) log
  app <- mkApp $ AppContext log config connPool
  return (connPool, app)

serverSpec :: Spec
serverSpec = withState createPoolAndApp $
  beforeWith restoreDBHook $ do
    describe "Server" $ do
      it "responds with 200" $ do
        get "/profile/user1" `shouldRespondWith` 200
      it "responds with User" $ do
        let id = U.fromWords64 0 1
            respBody = "{\"userID\":\"" ++ show id ++ "\",\"login\":\"user1\"}"
            user1 = fromString respBody
        get "/profile/user1" `shouldRespondWith` user1