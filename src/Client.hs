{-# LANGUAGE DataKinds #-}

module Client where

import API
import AuthData
import DomainSpecific

import Data.ByteString.UTF8
import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import Servant.API

type API = API' (BasicAuth "test-realm" AuthenticatedUser)

api :: Proxy API
api = Proxy

position :<|> hello :<|> marketing :<|> private :<|> register = undefined--client api

queries = undefined

clientMain :: AuthData -> IO ()
clientMain authData = do
    manager' <- newManager defaultManagerSettings
    res <- runClientM (queries authData) (mkClientEnv manager' $ BaseUrl Http "localhost" 8081 "")
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "Success"