{-# LANGUAGE OverloadedStrings #-}

module Log where

import System.Log.FastLogger

type Logger = TimedFastLogger

pushLog :: Logger -> LogStr -> IO ()
pushLog logger msg = logger (\time -> "[" <> toLogStr time <> "] " <> msg <> "\n")

initLogger :: IO (Logger, IO ())
initLogger = do
  timeCache <- newTimeCache simpleTimeFormat
  newTimedFastLogger timeCache (LogStdout 1024)
