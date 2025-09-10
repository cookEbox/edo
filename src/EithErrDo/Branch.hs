{-# LANGUAGE NoImplicitPrelude #-}

module EithErrDo.Branch
  ( whenE, unlessE, tapE
  ) where

import EithErrDo.Types   (IOEither, Error(..)) 
import qualified EitherDo.Branch  as Br ( whenE, unlessE, tapE )
import Prelude (IO, Bool(..))

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EithErrDo.Edo as E
-- >>> import Data.Text (Text)
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

-- | Run the action when the predicate is 'True'; otherwise succeed with @()@.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.whenE True (E.ok "ignored" :: E.IOEither Err String)
-- Right ()
-- >>> E.whenE True (E.bad (ParseErr "returned") :: E.IOEither Err String)
-- Left (ParseErr "returned")
-- >>> E.whenE False (E.bad (NotFound "ignored") ::  E.IOEither Err String)
-- Right ()
whenE :: Error e => forall a. Bool -> IOEither e a -> IOEither e ()
whenE = Br.whenE

-- | Run the action when the predicate is 'False'; otherwise succeed with @()@.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.unlessE False (E.ok 1 :: E.IOEither Err Int)
-- Right ()
-- >>> E.unlessE False (E.bad (Bug "error") :: E.IOEither Err Int)
-- Left (Bug "error")
-- >>> E.unlessE True (E.bad (ParseErr "error") :: E.IOEither Err Int)
-- Right ()
unlessE :: Error e => forall a. Bool -> IOEither e a -> IOEither e ()
unlessE = Br.unlessE

-- | On success, perform an 'IO' side effect with the value, then return it.
-- @since 0.1.0
-- Useful for logging/metrics without breaking the flow.
--
-- ==== __Examples__
-- >>> let side _ = pure () :: IO () -- stand-in for logging
-- >>> E.tapE side (E.ok 7 :: E.IOEither Err Int)
-- Right 7
-- >>> E.tapE side (E.bad (NotFound "error") :: E.IOEither Err Int)
-- Left (NotFound "error")
tapE :: Error e => forall a. (a -> IO ()) -> IOEither e a -> IOEither e a
tapE = Br.tapE
