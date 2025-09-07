{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}
{-# OPTIONS_HADDOCK not-home   #-}

module EitherDo.Branch
  ( whenE
  , unlessE
  , tapE
  ) where

import qualified Prelude as P
import           Prelude ( Either(..), IO, Bool(..)
                         , not
                         )
import EitherDo.Core 

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EitherDo.Edo as E
-- >>> import Data.IORef
-- >>> import qualified Control.Exception as X
-- >>> :set -XQualifiedDo


-- | Run the action when the predicate is 'True'; otherwise succeed with @()@.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.whenE True (E.ok "ignored" :: E.IOEither String String)
-- Right ()
-- >>> E.whenE True (E.bad "returned" :: E.IOEither String String)
-- Left "returned"
-- >>> E.whenE False (E.bad "ignored" ::  E.IOEither String String)
-- Right ()
whenE :: Bool -> IOEither e a -> IOEither e ()
whenE True  m = m >> ok ()
whenE False _ = ok ()

-- | Run the action when the predicate is 'False'; otherwise succeed with @()@.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.unlessE False (E.ok 1 :: E.IOEither String Int)
-- Right ()
-- >>> E.unlessE False (E.bad "error" :: E.IOEither String Int)
-- Left "error"
-- >>> E.unlessE True (E.bad "error" :: E.IOEither String Int)
-- Right ()
unlessE :: Bool -> IOEither e a -> IOEither e ()
unlessE b = whenE (not b)

-- | On success, perform an 'IO' side effect with the value, then return it.
-- @since 0.1.0
-- Useful for logging/metrics without breaking the flow.
--
-- ==== __Examples__
-- >>> let side _ = pure () :: IO () -- stand-in for logging
-- >>> E.tapE side (E.ok 7 :: E.IOEither String Int)
-- Right 7
-- >>> E.tapE side (E.bad "error" :: E.IOEither String Int)
-- Left "error"
tapE :: (a -> IO ()) -> IOEither e a -> IOEither e a
tapE f m = do
  r <- m
  case r of
    Left e  -> bad e
    Right a -> f a P.>> ok a
