module Helpers.Hspec where

import Control.Concurrent
import System.Process
import Test.Hspec.Core.Hooks

runAroundAllWait exe args waitSec = aroundAll_ $ \spec -> do
  h <- spawnProcess exe args
  threadDelay (1000 * 1000 * waitSec)
  spec
  terminateProcess h