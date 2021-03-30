module Main (main) where

import Helpers.DB
import IntegrationSpec
import ServerSpec
import Test.Hspec

main :: IO ()
main = hspec $
  withDBAround $ do
    serverSpec
    integrationSpec
