{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MyLib (someFunc) where

import Options.Generic (getRecord)
import Command
    ( Command(Command, inboxFolder, bucketName) )
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Data.Foldable (for_)
import Operations.Inaudible (deDRM, InaudibleTrace(..))
import Operations.Organize (organize, OrganizeTrace(..))
import qualified Data.Text.IO as TI
import Data.Trace ( Trace(..) )
import Data.Text (Text)
import Data.Functor.Contravariant (contramap)
import qualified Data.Text as T
import Amazonka (newEnv, Region(..), setEndpoint, region, overrides)
import Amazonka.Auth (discover)
import qualified Amazonka.S3 as S3
import Data.Key (getKey)

logger :: Trace IO Text
logger = Trace TI.putStrLn

data OrganizeMsg
  = Organized FilePath FilePath
  | Inaudible FilePath InaudibleTrace
  | Organize FilePath OrganizeTrace

formatOrganizeMsg :: OrganizeMsg -> Text
formatOrganizeMsg (Organized s str) = T.pack $ s <> " was moved to " <> str
formatOrganizeMsg (Inaudible s DeDRM) = T.pack $ s <> " was cleaned of DRM successfully"
formatOrganizeMsg (Organize s (Scrape md)) = T.pack $ s <> " was scraped into " <> show md
formatOrganizeMsg (Organize _ (Copy from to)) = T.pack $ from <> " copied to " <> to
formatOrganizeMsg (Organize _ (Remove str)) = T.pack $ str <> " was removed"

getInputFiles :: FilePath -> IO [FilePath]
getInputFiles inbox =
  map (inbox </>) . filter ((==) ".aax" . takeExtension) <$> listDirectory inbox

someFunc :: IO ()
someFunc = do
  Command{..} <- getRecord "smabp"
  discoveredEnv <- newEnv discover
  decryptKey <- getKey
  let env = discoveredEnv
        { region = Region' "fr-par"
        , overrides = setEndpoint True "https://s3.fr-par.scw.cloud" 443
        }
      bucket = S3.BucketName bucketName
  let trace = contramap formatOrganizeMsg logger
  inputFiles <- getInputFiles inboxFolder
  for_ inputFiles $ \file -> do
    newFile <- deDRM (contramap (Inaudible file) trace) decryptKey file >>=
      organize (contramap (Organize file) trace) env bucket
    runTrace trace (Organized file newFile)
