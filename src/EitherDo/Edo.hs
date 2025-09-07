{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}

{-|
Module      : EitherDo.Edo
Description : Qualified do-notation and helpers for IO (Either e a)
License     : BSD-3-Clause
Maintainer  : nj.cooke@outlook.com
Stability   : experimental
Portability : portable

Write ergonomic code in the @IO (Either e a)@ style using @QualifiedDo@:

@
import qualified EitherDo.Edo as E

action :: E.IOEither Text ()
action = E.do
  n  <- readConfigE          -- IO (Either e Int)
  _  <- E.traverseE_ doOne [1..n]
  E.ok ()
@

@since 0.1.0
-}

module EitherDo.Edo
  ( -- * Core
    IOEither, (>>=), (=<<), (>>), return, ok, bad, (|>), onRight
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

import EitherDo.Core    ( IOEither, (>>=), (=<<), (>>), return, ok, bad, (|>), onRight )
import EitherDo.Lift    ( hoistEither, fromMaybeE, guardE )
import EitherDo.Error   ( mapErrorE, recoverE, orElse )
import EitherDo.Branch  ( whenE, unlessE, tapE )
import EitherDo.Collect ( sequenceE, traverseE, forE, sequenceE_, traverseE_, forE_ )
import EitherDo.Res     ( finallyE, bracketE )
import EitherDo.Except  ( tryE, tryAnyE )
import EitherDo.Kleisli ( (>=>), (<=<) )

