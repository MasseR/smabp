module Main (main) where

import qualified Test.S3.Interface
import Test.Hspec (hspec)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  testContract <- maybe False (const True) <$> lookupEnv "TEST_CONTRACT"
  hspec $ do
    Test.S3.Interface.spec testContract

