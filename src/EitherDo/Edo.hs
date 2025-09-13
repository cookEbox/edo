{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}

{-|
Module      : EitherDo.Edo
Description : Qualified do-notation and helpers for IO (Either e a)
License     : BSD-3-Clause
Maintainer  : nj.cooke@outlook.com
Stability   : experimental
Portability : portable

For more examples please go to https://github.com/cookEbox/edo
Write ergonomic code in the @IO (Either e a)@ style using @QualifiedDo@:

@
{-# LANGUAGE QualifiedDo #-}

import qualified EitherDo.Edo as E

fetchConfig :: E.IOEither String Int
fetchConfig = pure (Right 3) -- "OK"
-- fetchConfig = pure (Right 5) -- "ERROR: too big"

doOne :: Int -> E.IOEither String ()
doOne n = if n < 5 then E.ok () else E.bad "too big"

pipeline :: E.IOEither String ()
pipeline = E.do
  n <- fetchConfig
  _ <- E.traverseE_ doOne [1..n]
  E.ok ()

main :: IO ()
main = do
  putStrLn "Running pipeline..."
  r <- pipeline
  -- Normal IO do-block interleaving with E.do pipeline
  case r of
    Left err -> putStrLn ("ERROR: " ++ err)
    Right () -> putStrLn "OK"
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

