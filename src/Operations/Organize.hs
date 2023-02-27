{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Operations.Organize where

import Amazonka (Env, chunkedFile, defaultChunkSize, runResourceT, send)
import Amazonka.S3 (BucketName, PutObjectResponse)
import qualified Amazonka.S3 as S3
import Control.Lens (strict, view)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Strict.Lens (utf8)
import Data.Trace
import S3.Interface
import System.FilePath (dropExtension, takeFileName, (</>))
import System.Process.Typed (proc, readProcessStdout_)

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

organize :: Trace IO OrganizeTrace -> S3Interface -> FilePath -> IO FilePath
organize trace interface path = do
  meta <- scrapeMetaData path
  runTrace trace (Scrape meta)
  let newPath = maybe (dropExtension path) toPath meta
      target = "audiobooks" </> newPath </> takeFileName path
  -- Something wrong with the uploads, the binary data is not the same there as
  -- it is in here. Chunk, endianness or something?
  key <- putFile interface path (T.pack target)
  print key
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
