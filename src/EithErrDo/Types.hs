{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}

module EithErrDo.Types
  ( IOEither, Error(..), ViaShow(..), ViaException(..)
  ) where

import           Control.Exception (Exception (displayException))
import           Data.Text
import           EitherDo.Core     (IOEither)
import           Prelude           (Show, show)

{-|

This module defines the lightweight `Error` class used across the edo API,
together with two small adapters that let you reuse existing instances:

* 'ViaShow'      — lift any @Show e@ to @Error e@
* 'ViaException' — lift any @Exception e@ to @Error e@

Typical usage with your own error type:

@
data Err = ParseErr Text | NotFound Text | Bug Text
  deriving (Eq, Show)

deriving via (ViaShow Err) instance Error Err
@

Or, if you already have an exception type:

@
newtype Boom = Boom Text
  deriving (Show)
instance Exception Boom

deriving via (ViaException Boom) instance Error Boom
@

With these in place, all edo combinators that require @Error e@ will work
with your custom errors, while keeping display/formatting concerns decoupled.
-}

class Error e where
  displayError :: e -> Text

-- | 'ViaShow'      — lift any @Show e@ to @Error e@
--
-- @since 0.1.0
--
newtype ViaShow e = ViaShow e
instance Show e => Error (ViaShow e) where
  displayError (ViaShow e) = pack (show e)

-- | 'ViaException' — lift any @Exception e@ to @Error e@
--
-- @since 0.1.0
--
newtype ViaException e = ViaException e
instance Exception e => Error (ViaException e) where
  displayError (ViaException e) = pack (displayException e)
