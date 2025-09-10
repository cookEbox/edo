{-# LANGUAGE NoImplicitPrelude #-}

module EithErrDo.Except
  ( tryE, tryAnyE
  ) where

import EithErrDo.Types   (IOEither, Error(..)) 
import qualified EitherDo.Except  as Ex ( tryE, tryAnyE )
import Prelude (IO)
import Control.Exception (Exception, SomeException)

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EithErrDo.Edo as E
-- >>> import Data.Text (Text, toUpper)
-- >>> import qualified Data.Text as T
-- >>> import Data.IORef
-- >>> import qualified Control.Exception as X
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

-- | Catch a specific exception from an 'IO' action and map it to your error.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.tryE (\(_ :: X.IOException) -> (Bug "io")) (pure "ok")
-- Right "ok"
--
-- >>> E.tryE (\(_ :: X.IOException) -> (Bug "io")) (X.throwIO (userError "nope") :: IO ())
-- Left (Bug "io")
--
-- Works smoothly inside an E.do block:
---
-- ==== __Examples__
-- >>> E.do n <- E.tryE (\(_ :: X.IOException) -> (Bug t"io")) (pure (3 :: Int))
-- ...      E.ok (n + 1)
-- Right 4
tryE :: Error e => forall ex a. Exception ex => (ex -> e) -> IO a -> IOEither e a
tryE = Ex.tryE

-- | Catch any exception (use with care) and map to your error type.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.tryAnyE (const (NotFound "boom")) (pure (99 :: Int))
-- Right 99
--
-- >>> E.tryAnyE (const (NotFound "boom")) (X.throwIO (userError "oh no") :: IO Int)
-- Left (NotFound "boom")
--
-- Equivalent to 'tryE' specialized to 'SomeException':
---
-- ==== __Examples__
-- >>> let f = const (Bug "x") :: X.SomeException -> String
-- >>> E.tryAnyE f (pure (1 :: Int))
-- Right 1
-- >>> E.tryE f (pure (1 :: Int))
-- Right 1
tryAnyE :: Error e => forall a. (SomeException -> e) -> IO a -> IOEither e a
tryAnyE = Ex.tryAnyE
