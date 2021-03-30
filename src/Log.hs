module Log where

import Control.Monad
import System.Log.FastLogger

type Logger = TimedFastLogger

type LogFn = LogStr -> IO ()

defaultFormattedTime :: IO FormattedTime
defaultFormattedTime = join $ newTimeCache simpleTimeFormat

defaultLogType :: LogType
defaultLogType = LogStdout defaultBufSize

pushLog :: Logger -> LogFn
pushLog logger msg = logger (\time -> "[" <> toLogStr time <> "] " <> msg <> "\n")

withLogFn :: (LogFn -> IO a) -> IO a
withLogFn actionWithLogFn = withTimedFastLogger defaultFormattedTime defaultLogType (actionWithLogFn . pushLog)
