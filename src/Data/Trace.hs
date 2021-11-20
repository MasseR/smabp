{-# LANGUAGE DerivingVia #-}
module Data.Trace where

import Data.Functor.Contravariant
import Data.Monoid

newtype Trace m a = Trace { runTrace :: a -> m () }
  deriving Contravariant via Op (m ())
  deriving (Semigroup, Monoid) via Op (Ap m ()) a
