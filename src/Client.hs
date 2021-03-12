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

position :<|> hello :<|> marketing :<|> private = client api

queries :: AuthData -> ClientM (Position, HelloMessage, Email, PrivateInfo)
queries (AuthData username password) = do
    pos <- position 10 20
    message <- hello $ Just "Yurii"
    email <- marketing $ ClientInfo "Yurii" "example@example.com" 20 ["Haskell", "Lean"]
    privateInfo <- private $ BasicAuthData (fromString username) (fromString password)
    return (pos, message, email, privateInfo)

clientMain :: AuthData -> IO ()
clientMain authData = do
    manager' <- newManager defaultManagerSettings
    res <- runClientM (queries authData) (mkClientEnv manager' $ BaseUrl Http "localhost" 8081 "")
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (pos, message, email, privateInfo) -> do
            print pos
            print message
            print email
            print privateInfo