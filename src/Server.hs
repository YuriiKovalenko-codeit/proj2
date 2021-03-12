{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Server where

import API
import DomainSpecific

import Data.Map as M
import Crypto.JOSE
import Crypto.JOSE.JWK
import Network.Wai.Handler.Warp
import Servant
import qualified Servant.Auth as SA
import Servant.Auth.Server

import System.IO
--import Data.ByteString.Base64

authCheck :: Pool Connection
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck connPool (BasicAuthData username password) = return $ maybe Indefinite Authenticated $ M.lookup (username, password) connPool

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
    fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type API = API' (Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser)

api :: Proxy API
api = Proxy

server3 :: Server API
server3 = position
     :<|> hello
     :<|> marketing
     :<|> private

    where position :: Int -> Int -> Handler Position
          position x y = return (Position x y)

          hello :: Maybe String -> Handler HelloMessage
          hello mname = return . HelloMessage $ case mname of
              Nothing -> "Hello, anon"
              Just name -> "Hello, " ++ name

          marketing :: ClientInfo -> Handler Email
          marketing = return . emailForClient

          private :: AuthResult AuthenticatedUser -> Handler PrivateInfo
          private (Authenticated user) = return . PrivateInfo $ auID user
          private _ = throwAll err401

mkApp :: Pool Connection -> IO Application
mkApp connPool = do
    myKey <- genJWK $ ECGenParam P_256
    --hPutStrLn stderr $ "Used secret is " ++ (show $ encodeBase64 secret)
    let jwtCfg = (defaultJWTSettings myKey) { jwtAlg = Just ES256 }
        authCfg = authCheck connPool
        cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
    return $ serveWithContext api cfg server3

serverMain :: IO ()
serverMain = do
    connPool <- initConnPool
    run 8081 =<< mkApp connPool
