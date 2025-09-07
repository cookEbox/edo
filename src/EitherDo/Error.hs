{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}
{-# OPTIONS_HADDOCK not-home   #-}

module EitherDo.Error
  ( mapErrorE
  , recoverE
  , orElse
  ) where

import qualified Prelude as P
import           Prelude ( Either(..)
                         , const, either, pure
                         , (.), (<$>)
                         )
import EitherDo.Core

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EitherDo.Edo as E
-- >>> import Data.IORef
-- >>> import qualified Control.Exception as X
-- >>> :set -XQualifiedDo


-- | Map the error type of an 'IOEither'.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.mapErrorE length (E.bad "boom" :: E.IOEither String Int)
-- Left 4
-- >>> E.mapErrorE length (E.ok 5 :: E.IOEither String Int)
-- Right 5
mapErrorE :: (e -> e') -> IOEither e a -> IOEither e' a
mapErrorE f io = either (Left . f) Right <$> io

-- | Handle a failure by providing a recovery action.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.recoverE (E.bad "error") (\_ -> E.ok "yes")
-- Right "yes"
-- >>> E.recoverE (E.ok "ok") (\_ -> E.ok "yes")
-- Right "ok"
recoverE :: IOEither e a -> (e -> IOEither e a) -> IOEither e a
recoverE m handler = m P.>>= either handler (pure . Right)

-- | Try the first computation; if it fails, run the alternative.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.orElse (E.bad "x") (E.ok "alt")
-- Right "alt"
-- >>> E.orElse (E.ok "ok") (E.bad "never")
-- Right "ok"
orElse :: IOEither e a -> IOEither e a -> IOEither e a
orElse m alt = recoverE m (const alt)
