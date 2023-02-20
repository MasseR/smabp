{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Operations.Organize where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>), dropExtension, takeFileName)
import System.Process.Typed (proc, readProcessStdout_)
import Control.Lens ( view, strict )
import Data.Text.Strict.Lens (utf8)
import Data.Map.Strict (Map)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as M
import Data.Trace
import Amazonka (Env, chunkedFile, defaultChunkSize, runResourceT, send)
import Amazonka.S3 (BucketName, PutObjectResponse)
import qualified Amazonka.S3 as S3

data OrganizeTrace
  = Scrape (Maybe MetaData)
  | Copy FilePath FilePath
  | Remove FilePath

-- | Represents the necessary information from the audio file
data MetaData = MetaData
  { artist :: Text
  , title :: Text
  }
  deriving Show

toPath :: MetaData -> FilePath
toPath MetaData {artist, title} = T.unpack artist </> T.unpack title

type MetaMap = Map Text Text

parseMetaMap :: ByteString -> MetaMap
parseMetaMap = foldMap parseLine . T.lines . view (strict . utf8)
  where
    parseLine :: Text -> MetaMap
    parseLine (T.splitOn ":" -> (key:value)) =
      M.singleton (T.toLower (T.strip key)) (T.strip (T.intercalate ":" value))
    parseLine _ = M.empty

scrapeMetaData :: FilePath -> IO (Maybe MetaData)
scrapeMetaData path = do
  results <- parseMetaMap <$> readProcessStdout_ cmd
  print results
  pure (MetaData <$> M.lookup "artist" results <*> M.lookup "title" results)
  where
    cmd = proc "exiftool" [path]

organize :: Trace IO OrganizeTrace -> Env -> BucketName -> FilePath -> IO FilePath
organize trace env bucket path = do
  meta <- scrapeMetaData path
  runTrace trace (Scrape meta)
  let newPath = maybe (dropExtension path) toPath meta
      target = "audiobooks" </> newPath </> takeFileName path
  -- Something wrong with the uploads, the binary data is not the same there as
  -- it is in here. Chunk, endianness or something?
  obj <- uploadImage env bucket path target
  print obj
  runTrace trace (Copy path target)
  -- removeFile path >> runTrace trace (Remove path)
  pure target

uploadImage :: Env -> BucketName -> FilePath -> FilePath -> IO PutObjectResponse
uploadImage env bucket source target = do
  obj <- chunkedFile defaultChunkSize source
  runResourceT $ do
    let putObj = S3.newPutObject bucket key obj
        key = S3.ObjectKey (T.pack target)
    send env putObj
