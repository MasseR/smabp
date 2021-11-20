module Operations.Inaudible where

import System.Process.Typed (proc, runProcess_)
import qualified Data.Text as T
import System.FilePath (replaceExtension)
import Data.Key (Key(..))
import Data.Trace

data InaudibleTrace =
  DeDRM

asKey :: Key -> String
asKey (Key txt) = T.unpack txt

deDRM :: Trace IO InaudibleTrace -> Key -> FilePath -> IO FilePath
deDRM trace key aax = m4b <$ (runProcess_ cmd >> runTrace trace DeDRM)
  where
    m4b :: FilePath
    m4b = replaceExtension aax "m4b"
    cmd = proc "ffmpeg" ["-activation_bytes", asKey key, "-i", aax, "-c", "copy", m4b]
