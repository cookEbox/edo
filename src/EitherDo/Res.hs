{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}
{-# OPTIONS_HADDOCK not-home   #-}

module EitherDo.Res (finallyE, bracketE) where

import           Prelude ( Either(..), IO)
import qualified Control.Exception as X
import EitherDo.Core

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EitherDo.Edo as E
-- >>> import Data.IORef
-- >>> import qualified Control.Exception as X
-- >>> :set -XQualifiedDo


-- | Ensure a finaliser runs after the computation (success or failure).
-- @since 0.1.0
--
-- Runs the finalizer even on 'Left', and preserves the original result.
---
-- ==== __Examples__
-- >>> let testFinallyRight = do
-- ...       r   <- newIORef ([] :: [String])
-- ...       res <- E.finallyE (E.ok "X") (modifyIORef' r ("fin":))
-- ...       logs <- readIORef r
-- ...       pure (res, logs)
-- >>> testFinallyRight
-- (Right "X",["fin"])
--
-- >>> let testFinallyLeft = do
-- ...       r   <- newIORef ([] :: [String])
-- ...       res <- E.finallyE (E.bad "e" :: E.IOEither String ()) (modifyIORef' r ("fin":))
-- ...       logs <- readIORef r
-- ...       pure (res, logs)
-- >>> testFinallyLeft
-- (Left "e",["fin"])
finallyE :: IOEither e a -> IO () -> IOEither e a
finallyE = X.finally

-- | Acquire / use / release with @IOEither@ semantics.
-- @since 0.1.0
--
-- If acquisition fails, 'release' is not called. If use runs, 'release' runs
-- afterwards regardless of success or failure.
--
-- Successful acquire + use ⇒ release runs once:
---
-- ==== __Examples__
-- >>> let testBracketOK = do
-- ...       logRef <- newIORef ([] :: [String])
-- ...       let acquire   = modifyIORef' logRef ("acq":) >> E.ok "R"
-- ...           release a = modifyIORef' logRef (("rel:"<>a):)
-- ...           use _     = modifyIORef' logRef ("use":) >> E.ok "done"
-- ...       res  <- E.bracketE acquire release use
-- ...       logs <- readIORef logRef
-- ...       pure (res, logs)
-- >>> testBracketOK
-- (Right "done",["rel:R","use","acq"])
--
-- Successful acquire + failing use ⇒ release still runs:
--
-- >>> let testBracketUseFail = do
-- ...       logRef <- newIORef ([] :: [String])
-- ...       let acquire   = modifyIORef' logRef ("acq":) >> E.ok "R"
-- ...           release a = modifyIORef' logRef (("rel:"<>a):)
-- ...           use _     = modifyIORef' logRef ("useBad":) >> E.bad "e"
-- ...       res  <- E.bracketE acquire release use
-- ...       logs <- readIORef logRef
-- ...       pure (res, logs)
-- >>> testBracketUseFail
-- (Left "e",["rel:R","useBad","acq"])
--
-- Failing acquire ⇒ release is not called:
--
-- >>> let testBracketAcquireFail = do
-- ...       logRef <- newIORef ([] :: [String])
-- ...       let acquire   = modifyIORef' logRef ("acqFail":) >> E.bad "fail"
-- ...           release a = modifyIORef' logRef (("rel:"<>a):)
-- ...           use _     = modifyIORef' logRef ("use?":) >> E.ok ()
-- ...       res  <- E.bracketE acquire release use
-- ...       logs <- readIORef logRef
-- ...       pure (res, logs)
-- >>> testBracketAcquireFail
-- (Left "fail",["acqFail"])
bracketE
  :: IOEither e a        -- ^ acquire
  -> (a -> IO ())        -- ^ release
  -> (a -> IOEither e b) -- ^ use
  -> IOEither e b
bracketE acquire release use = do
  ar <- acquire
  case ar of
    Left e  -> bad e
    Right a -> X.finally (use a) (release a)
