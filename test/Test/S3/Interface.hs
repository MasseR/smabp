{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.S3.Interface where

import Conduit (MonadUnliftIO, liftIO, runConduit, runResourceT, (.|))
import Control.Monad (when)
import Control.Monad.Trans.Cont
import Crypto.Hash (Digest, HashAlgorithm)
import qualified Crypto.Hash as Crypto
import Crypto.Hash.Algorithms (SHA256)
import qualified Crypto.Random as Random
import qualified Data.Conduit.Combinators as C
import Data.Generics.Labels
       ()
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import S3.Interface
import System.FilePath (takeBaseName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Hedgehog

hash :: (HashAlgorithm a, MonadUnliftIO m) => FilePath -> m (Digest a)
hash path = C.withSourceFile path $ \source ->
  Crypto.hashFinalize <$> runConduit (source .| C.foldl Crypto.hashUpdate Crypto.hashInit)

withTempDir :: FilePath -> ContT r IO FilePath
withTempDir path = ContT (withSystemTempDirectory path)

-- XXX: Temporarily have the model just here
buildModel :: FilePath -> IO S3Interface
buildModel root =
  pure S3Interface
  { putFile = \path key -> do
      runResourceT $ do
        runConduit $ C.sourceFileBS path .| C.sinkFileBS (root </> T.unpack key)
      pure key
  , getFile = \key outroot -> do
      let outpath = outroot </> T.unpack key
          inpath = root </> T.unpack key
      runResourceT $ runConduit $ C.sourceFileBS inpath .| C.sinkFileBS outpath
      pure outpath
  }

data File = File { name :: Text, chunks :: Int }
  deriving Show

genFile :: Gen File
genFile = File
  <$> Gen.text (Range.linear 1 32) Gen.alphaNum
  <*> Gen.integral (Range.linear 0 8192)

data FileCommand
  = PutFile File
  | ReadFile Int
  | ReadMetadata Int
  deriving Show

genCommand :: Gen FileCommand
genCommand = Gen.choice
  [ PutFile <$> genFile
  , ReadFile <$> Gen.integral (Range.linear 0 1000)
  , ReadMetadata <$> Gen.integral (Range.linear 0 1000)
  ]


write :: FilePath -> File -> IO FilePath
write root f = do
  let path = root </> T.unpack (name f)
  runResourceT $ runConduit $ C.replicateM (chunks f) (liftIO $ Random.getRandomBytes 256) .| C.sinkFileBS path
  pure path

specGetPut :: PropertyT IO ()
specGetPut = do
  file <- forAll genFile
  (wanted, got) <- liftIO $ flip runContT pure $ do
    root <- withTempDir "test-getput-in"
    modelroot <- withTempDir "test-getput-model"
    downloadroot <- withTempDir "test-getput-download"
    liftIO $ do
      interface <- buildModel modelroot
      inpath <- write root file
      key <- putFile interface inpath (T.pack $ takeBaseName inpath)
      outpath <- getFile interface key downloadroot
      (,) <$> hash @SHA256 inpath <*> hash @SHA256 outpath
  wanted === got

data Result = Result
  { resultKey :: Text
  , resultHash :: Digest SHA256
  , resultOriginHash :: Digest SHA256
  }
  deriving (Show, Eq)

specGetPutS3 :: PropertyT IO ()
specGetPutS3 = do
  file <- forAll genFile
  annotateShow file
  (wanted, got) <- liftIO $ flip runContT pure $ do
    root <- withTempDir "test-getput-in"
    modelroot <- withTempDir "test-getput-model"
    downloadroot <- withTempDir "test-getput-download"
    liftIO $ do
      inpath <- write root file
      modelInt <- buildModel modelroot
      s3Int <- buildInterface "introitu-contract-tests"
      (,) <$> create modelInt downloadroot inpath <*> create s3Int downloadroot inpath
  wanted === got
  where
    create interface downloadroot inpath = do
      key <- putFile interface inpath (T.pack $ takeBaseName inpath)
      s3path <- getFile interface key downloadroot
      Result <$> pure key <*> hash s3path <*> hash inpath

-- Contract test for the interface

spec :: Bool -> Spec
spec testContract =
  describe "Contract test between S3 and the model" $ do
    context "Model" $ do
      it "satisfies the get-put property for content" $ hedgehog specGetPut
    when testContract $ do
      context "S3" $ do
        it "satisfies the get-put property for content" $ hedgehog specGetPutS3

