{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Key where

import Data.Text (Text)
import Options.Generic (ParseFields(..), ParseField(..), ParseRecord(..))

newtype Key = Key Text
  deriving Show

instance ParseField Key where
  metavar _ = "KEY"
  parseField h m c d = Key <$> parseField h m c d
  readField = Key <$> readField


instance ParseFields Key

instance ParseRecord Key where
  parseRecord = Key <$> parseRecord
