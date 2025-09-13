{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}

{-|
Module      : EithErrDo.Edo
Description : Qualified do-notation and helpers for IO (Either e a) where e is contstrained as an Error
License     : BSD-3-Clause
Maintainer  : nj.cooke@outlook.com
Stability   : experimental
Portability : portable

Write ergonomic code in the @IO (Either e a)@ style using @QualifiedDo@.  

Instead of juggling `ExceptT e IO` or nested case-expressions, you write
straight-line code and let failures short-circuit automatically.

Here is a complete example:

@
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}

import qualified EithErrDo.Edo as E
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readEither)

data Err = ParseErr T.Text | NotFound T.Text
  deriving (Eq, Show)
deriving via (E.ViaShow Err) instance E.Error Err

type IOEither e a = E.IOEither e a
hoistMaybe :: E.Error e => e -> Maybe a -> IOEither e a
hoistMaybe e m = E.hoistEither id (maybe (Left e) Right m)

-- Pipeline A: guaranteed success (uses a constant "123")
okPipeline :: IOEither Err Int
okPipeline = E.do
  s <- E.ok "123"
  E.hoistEither (ParseErr . T.pack) (readEither s)

-- Pipeline B: likely failure if PORT is not set
badPipeline :: IOEither Err Int
badPipeline = E.do
  m <- E.ok =<< lookupEnv "PORT"
  s <- hoistMaybe (NotFound "PORT") m
  E.hoistEither (ParseErr . T.pack) (readEither s)

main :: IO ()
main = do
  r1 <- okPipeline
  putStrLn $ case r1 of
    Left e  -> "OK pipeline error (unexpected): " <> show e
    Right n -> "OK pipeline result: " <> show n

  r2 <- badPipeline
  putStrLn $ case r2 of
    Left e  -> "PORT pipeline error (expected if PORT unset): " <> show e
    Right n -> "PORT pipeline result: " <> show n
@

The above shows:

  * defining a custom error type (`Err`) and deriving `Error` via 'Show'
  * using @QualifiedDo@ with `E.do`
  * combining IO actions and error checks without extra boilerplate
  * clean short-circuiting on the first error

@since 0.1.0
-}

module EithErrDo.Edo
  ( -- * Types
    IOEither, Error(..), ViaException(..), ViaShow(..)
    -- * Core
  , (>>=), (=<<), (>>), return, ok, bad, (|>), onRight
    -- * Lifting & conversions
  , hoistEither, fromMaybeE, guardE
    -- * Error mapping & recovery
  , mapErrorE, recoverE, orElse
    -- * Branching / side effects
  , whenE, unlessE, tapE
    -- * Collecting results
  , sequenceE, traverseE, forE, sequenceE_, traverseE_, forE_
    -- * Resource-safety
  , finallyE, bracketE
    -- * Exceptions interop
  , tryE, tryAnyE
    -- * Kleisli
  , (>=>), (<=<)
  ) where

import EithErrDo.Core    ( return, ok, bad, onRight, (>>=), (=<<), (>>), (|>) ) 
import EithErrDo.Lift    ( hoistEither, fromMaybeE, guardE )
import EithErrDo.Error   ( mapErrorE, recoverE, orElse )
import EithErrDo.Branch  ( whenE, unlessE, tapE )
import EithErrDo.Collect ( sequenceE, traverseE, forE, sequenceE_, traverseE_, forE_ )
import EithErrDo.Res     ( finallyE, bracketE )
import EithErrDo.Except  ( tryE, tryAnyE )
import EithErrDo.Kleisli ( (>=>), (<=<) )
import EithErrDo.Types   ( IOEither, Error(..), ViaException(..), ViaShow(..) )

