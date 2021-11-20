{-# LANGUAGE DeriveGeneric #-}
module Command where

import GHC.Generics (Generic)
import Options.Generic (ParseRecord)
import Data.Key (Key)

data Command = Command
  { decryptKey :: Key
  , audioFolder :: FilePath
  , inboxFolder :: FilePath
  }
  deriving (Show, Generic)

instance ParseRecord Command
