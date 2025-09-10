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
{-# LANGUAGE QualifiedDo, OverloadedStrings, DerivingVia #-}

import qualified EithErrDo.Edo as E
import Data.Text (Text)
import System.Environment (lookupEnv)

-- A small error type with 'Error' instance via 'Show'
data Err = ParseErr Text | NotFound Text
  deriving (Eq, Show)
deriving via (E.ViaShow Err) instance E.Error Err

-- A config reader in the 'IOEither Err' style
readPort :: E.IOEither Err Int
readPort = E.do
  m <- E.hoistEither (maybe (Left (NotFound "PORT")) Right)
         =<< E.ok =<< lookupEnv "PORT"
  n <- E.hoistEither ParseErr (readEither m)
  E.ok n

-- Example program using the API
main :: IO ()
main = do
  r <- readPort
  case r of
    Left e  -> putStrLn ("config error: " <> show e)
    Right n -> putStrLn ("starting on port " <> show n)
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

