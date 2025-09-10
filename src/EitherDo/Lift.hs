{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}
{-# OPTIONS_HADDOCK not-home   #-}

module EitherDo.Lift
  ( hoistEither
  , fromMaybeE
  , guardE
  ) where

import           Prelude ( Either(..), Maybe(..), Bool(..)
                         , maybe , pure
                         )
import EitherDo.Core

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EitherDo.Edo as E
-- >>> import Data.IORef
-- >>> import qualified Control.Exception as X
-- >>> :set -XQualifiedDo

-- | Lift a pure 'Either' into 'IOEither'.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.hoistEither (Right (7 :: Int))
-- Right 7
-- >>> E.hoistEither (Left "err" :: Either String Int)
-- Left "err"
hoistEither :: Either e a -> IOEither e a
hoistEither = pure

-- | Convert a 'Maybe' to 'IOEither', providing an error for 'Nothing'.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.fromMaybeE "none" (Just 'x')
-- Right 'x'
-- >>> E.fromMaybeE "none" (Nothing :: Maybe Int)
-- Left "none"
fromMaybeE :: e -> Maybe a -> IOEither e a
fromMaybeE e = maybe (bad e) ok

-- | Succeed with @()@ when predicate holds; otherwise fail with the error.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.guardE True "err"
-- Right ()
-- >>> E.guardE False "err"
-- Left "err"
guardE :: Bool -> e -> IOEither e ()
guardE b e = if b then ok () else bad e
