{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}
{-# OPTIONS_HADDOCK not-home   #-}

module EitherDo.Except
  ( tryE
  , tryAnyE
  ) where

import           Control.Exception (Exception, SomeException)
import qualified Control.Exception as X
import           EitherDo.Core
import           Prelude           (Either (..), IO)

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EitherDo.Edo as E
-- >>> import Data.IORef
-- >>> import qualified Control.Exception as X
-- >>> :set -XQualifiedDo


-- | Catch a specific exception from an 'IO' action and map it to your error.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.tryE (\(_ :: X.IOException) -> "io") (pure "ok")
-- Right "ok"
--
-- >>> E.tryE (\(_ :: X.IOException) -> "io") (X.throwIO (userError "nope") :: IO ())
-- Left "io"
--
-- Works smoothly inside an E.do block:
---
-- ==== __Examples__
-- >>> E.do n <- E.tryE (\(_ :: X.IOException) -> "io") (pure (3 :: Int))
-- ...      E.ok (n + 1)
-- Right 4
tryE :: Exception ex => (ex -> e) -> IO a -> IOEither e a
tryE toErr io = do
  r <- X.try io
  case r of
    Left ex -> bad (toErr ex)
    Right a -> ok a

-- | Catch any exception (use with care) and map to your error type.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.tryAnyE (const "boom") (pure (99 :: Int))
-- Right 99
--
-- >>> E.tryAnyE (const "boom") (X.throwIO (userError "oh no") :: IO Int)
-- Left "boom"
--
-- Equivalent to 'tryE' specialized to 'SomeException':
---
-- ==== __Examples__
-- >>> let f = const "x" :: X.SomeException -> String
-- >>> E.tryAnyE f (pure (1 :: Int))
-- Right 1
-- >>> E.tryE f (pure (1 :: Int))
-- Right 1
tryAnyE :: (SomeException -> e) -> IO a -> IOEither e a
tryAnyE = tryE
