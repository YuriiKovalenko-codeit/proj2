module Keygen where

import qualified Data.ByteString.Lazy as BL
import Crypto.JOSE.JWK
import Data.Aeson

keygenMain :: FilePath -> IO ()
keygenMain outputPath = do
    key <- genJWK $ ECGenParam P_256
    BL.writeFile outputPath $ encode key
    putStrLn $ "File '" ++ outputPath ++ "' has been successfully written"