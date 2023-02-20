{-# LANGUAGE DeriveGeneric #-}
module Command where

import GHC.Generics (Generic)
import Options.Generic (ParseRecord)
import Data.Text (Text)

data Command = Command
  { bucketName :: Text
  , inboxFolder :: FilePath
  }
  deriving (Show, Generic)

instance ParseRecord Command
