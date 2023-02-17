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
import System.Directory (copyFile, createDirectoryIfMissing, removeFile)
import Data.Trace

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

organize :: Trace IO OrganizeTrace -> FilePath -> FilePath -> IO FilePath
organize trace audioFolder path = do
  meta <- scrapeMetaData path
  runTrace trace (Scrape meta)
  let newPath = audioFolder </> maybe (dropExtension path) toPath meta
      file = newPath </> takeFileName path
  createDirectoryIfMissing True newPath
  copyFile path file >> runTrace trace (Copy path file)
  removeFile path >> runTrace trace (Remove path)
  pure file
