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

--type Auth' = BasicAuth "some-realm" AuthenticatedUser
type LoginAuth = SAPI.BasicAuth "some-realm" AuthenticatedUser
type CommonAuth = Auth '[SA.BasicAuth, Bearer] AuthenticatedUser 
type API = API' LoginAuth CommonAuth

api :: Proxy API
api = Proxy

--               "profile" :> Capture "login" Text :> Get '[JSON] ProfileInfo
--          :<|> "login" :> loginAuth :> Get '[JSON] AuthenticatedUser 
--          :<|> "private" :> commonAuth :> Get '[JSON] NoContent
--          :<|> "users" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] ProfileInfo
--          :<|> "user" :> Capture "id" UUID :> Get '[JSON] ProfileInfo
--          :<|> "user" :> commonAuth :> ReqBody '[JSON] ProfileUpdateInfo :> Put '[JSON] NoContent
--position :<|> hello :<|> marketing :<|> private :<|> register = undefined--client api
getProfileByLogin :<|> loginUser :<|> private :<|> register :<|> getProfileByID :<|> updateUser = client api

queries :: AuthData -> ClientM ()
queries authData = do
    (Headers (LoginResponse uuid token) _) <- loginUser $ authToBasicData authData
    private $ Token $ T.encodeUtf8 token
    return ()

clientMain :: AuthData -> IO ()
clientMain authData = do
    manager' <- newManager defaultManagerSettings
    res <- runClientM (queries authData) (mkClientEnv manager' $ BaseUrl Http "localhost" 8081 "")
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "Success"