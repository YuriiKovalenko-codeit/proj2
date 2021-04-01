module Server.Middleware where

import Network.Wai
import Network.Wai.Middleware.RequestLogger

mkMiddlewares :: IO [Middleware]
mkMiddlewares = return [logStdoutDev]

chain :: [a -> a] -> a -> a
chain = foldr1 (.)
