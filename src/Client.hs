{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import API
import AuthData
import DomainSpecific

import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Proxy
import Data.Text.Encoding as T
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import Servant.API as SAPI
import Servant.Auth as SA
import Servant.Auth.Client

type LoginAuth = SAPI.BasicAuth "some-realm" AuthenticatedUser
type CommonAuth = Auth '[SA.BasicAuth, Bearer] AuthenticatedUser 
type API = API' LoginAuth CommonAuth

api :: Proxy API
api = Proxy

getProfileByLogin :<|> loginUser :<|> private :<|> register :<|> getProfileByID :<|> updateUser = client api

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