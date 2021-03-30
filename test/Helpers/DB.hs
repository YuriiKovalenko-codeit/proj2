module Helpers.DB where

import Data.Text
import Data.UUID
import Data.Word
import Database.Persist.Sql
import Helpers.Hspec
import Log
import Server.Config
import Server.DB
import Server.Helpers
import Test.Hspec

testConfigPath :: FilePath
testConfigPath = "test/config.yaml"

getTestConfig :: IO Config
getTestConfig = loadServerConfig $ Just testConfigPath

mkTestUser :: Text -> Text -> Word64 -> User
mkTestUser login pass id = User login (hashPassword pass) (fromWords64 0 id)

testUsers :: [User]
testUsers =
  [ mkTestUser "user1" "11" 1,
    mkTestUser "user2" "22" 2
  ]

createPool :: IO ConnectionPool
createPool = withLogFn $ \log -> do
  config <- getTestConfig
  createConnectionPool (dbConnectionString config) (dbConnPoolSize config) log

restoreDB :: ConnectionPool -> IO ()
restoreDB connPool = do
  flip runSqlPersistMPool connPool $ do
    runMigration migrateAll
    deleteWhere [UserLogin !=. ""]
    insertMany testUsers
  return ()

withDBAround :: SpecWith a -> SpecWith a
withDBAround = runAroundAllWait "docker-compose" ["-f", "test/docker-compose.yml", "up"] 5