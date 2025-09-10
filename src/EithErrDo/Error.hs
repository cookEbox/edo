{-# LANGUAGE NoImplicitPrelude #-}

module EithErrDo.Error
  ( mapErrorE, recoverE, orElse
  ) where

import EithErrDo.Types   (IOEither, Error(..)) 
import qualified EitherDo.Error   as Er ( mapErrorE, recoverE, orElse )

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EithErrDo.Edo as E
-- >>> import Data.Text (Text, toUpper)
-- >>> import qualified Data.Text as T
-- >>> :set -XQualifiedDo
-- >>> :set -XOverloadedStrings
-- >>> :set -XDerivingVia
--
-- A tiny error type for all examples.
-- We derive Show and then use `ViaShow` to satisfy `E.Error`.
-- >>> :{
-- data Err = ParseErr Text | NotFound Text | Bug Text
--   deriving (Eq, Show)
-- deriving via (E.ViaShow Err) instance E.Error Err
-- :}

-- | Map the error type of an 'IOEither'.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.mapErrorE (\(ParseErr t) -> ParseErr (toUpper t)) (E.bad (ParseErr "boom") :: E.IOEither Err Int)
-- Left (ParseErr "BOOM")
-- >>> E.mapErrorE (\(ParseErr t) -> ParseErr (toUpper t)) (E.ok 5 :: E.IOEither Err Int)
-- Right 5
mapErrorE :: (Error e, Error e') => forall a. (e -> e') -> IOEither e a -> IOEither e' a
mapErrorE = Er.mapErrorE

-- | Handle a failure by providing a recovery action.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.recoverE (E.bad (Bug "error")) (\_ -> E.ok "yes") :: E.IOEither Err String
-- Right "yes"
-- >>> E.recoverE (E.ok "ok") (\_ -> E.ok "yes") :: E.IOEither Err String
-- Right "ok"
recoverE :: Error e => forall a. IOEither e a -> (e -> IOEither e a) -> IOEither e a
recoverE = Er.recoverE

-- | Try the first computation; if it fails, run the alternative.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.orElse (E.bad (Bug "x")) (E.ok "alt") :: E.IOEither Err String
-- Right "alt"
-- >>> E.orElse (E.ok "ok") (E.bad (ParseErr "never")) :: E.IOEither Err String
-- Right "ok"
orElse :: Error e => forall a. IOEither e a -> IOEither e a -> IOEither e a
orElse = Er.orElse
