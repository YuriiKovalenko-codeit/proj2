module Server.Types where

import Control.Monad.Reader
import Database.Persist.Sql (ConnectionPool)
import Servant.Server
import Server.Config
import System.Log.FastLogger

data AppContext = AppContext
  { _logFn :: LogStr -> IO (),
    _config :: Config,
    _connPool :: ConnectionPool
  }

type HandlerT = ReaderT AppContext Handler
