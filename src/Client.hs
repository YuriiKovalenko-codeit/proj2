{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import API.Client
import API.Types
import AuthData
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Text
import Data.Text.Encoding as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API as SAPI
import Servant.Auth.Client
import Servant.Client

getProfileByLogin :<|> loginUser :<|> private :<|> register :<|> getProfileByID :<|> updateUser = client api

tokenFromText :: Text -> Token
tokenFromText = Token . T.encodeUtf8

queries :: AuthData -> ClientM ()
queries authData = do
  LoginResponse uuid token <- loginUser $ authToBasicData authData
  private $ Token $ T.encodeUtf8 token
  return ()

clientMain :: AuthData -> IO ()
clientMain authData = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM (queries authData) (mkClientEnv manager' $ BaseUrl Http "localhost" 8081 "")
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> putStrLn "Success"