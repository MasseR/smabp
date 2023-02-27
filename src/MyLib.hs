{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module MyLib (someFunc) where

import Amazonka (Region(..), newEnv, overrides, region, setEndpoint)
import Amazonka.Auth (discover)
import qualified Amazonka.S3 as S3
import Command (Command(Command, bucketName, inboxFolder))
import Data.Foldable (for_)
import Data.Functor.Contravariant (contramap)
import Data.Key (getKey)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Trace (Trace(..))
import Operations.Inaudible (InaudibleTrace(..), deDRM)
import Operations.Organize (OrganizeTrace(..), organize)
import Options.Generic (getRecord)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

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
        , overrides = setEndpoint True "s3.fr-par.scw.cloud" 443
        }
      bucket = S3.BucketName bucketName
  let trace = contramap formatOrganizeMsg logger
  inputFiles <- getInputFiles inboxFolder
  for_ inputFiles $ \file -> do
    newFile <- deDRM (contramap (Inaudible file) trace) decryptKey file >>=
      organize (contramap (Organize file) trace) env bucket
    runTrace trace (Organized file newFile)
