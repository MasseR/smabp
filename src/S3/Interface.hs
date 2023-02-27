{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module S3.Interface where

import Amazonka
       ( Env
       , Env'(..)
       , Region(..)
       , RequestBody(..)
       , discover
       , hashedFile
       , newEnv
       , runResourceT
       , send
       , setEndpoint
       , sinkBody
       )
import qualified Amazonka.S3 as S3
import Control.Lens (view)
import qualified Data.Conduit.Combinators as C
import Data.Generics.Labels
       ()
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeBaseName, (</>))

type Key = Text

data S3Interface = S3Interface
  { putFile :: FilePath -> Key -> IO Key
  -- ^ Upload a file from path
  , getFile :: Key -> FilePath -> IO FilePath
  -- ^ Download a file to a given root
  }

buildInterface :: Text -> IO S3Interface
buildInterface bucketName = do
  discoveredEnv <- newEnv discover
  let env = discoveredEnv
        { region = Region' "fr-par"
        , overrides = setEndpoint True "s3.fr-par.scw.cloud" 443
        }
      bucket = S3.BucketName bucketName
  pure S3Interface
    { putFile = uploadFile env bucket
    , getFile = downloadFile env bucket
    }
  where
    uploadFile :: Env -> S3.BucketName -> FilePath -> Key -> IO Key
    uploadFile env bucket path key = do
      obj <- Hashed <$> hashedFile path
      runResourceT $ do
        let putObj = S3.newPutObject bucket (S3.ObjectKey key) obj
        _ <- send env putObj
        pure key
    downloadFile :: Env -> S3.BucketName -> Key -> FilePath -> IO FilePath
    downloadFile env bucket key root = do
      let getObj = S3.newGetObject bucket (S3.ObjectKey key)
      runResourceT $ do
        x <- view #body <$> send env getObj
        let path = root </> T.unpack key
        _ <- sinkBody x (C.sinkFileBS path)
        pure path

