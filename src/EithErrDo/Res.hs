{-# LANGUAGE NoImplicitPrelude #-}

module EithErrDo.Res
  ( finallyE, bracketE
  ) where

import qualified EitherDo.Res    as Re (bracketE, finallyE)
import           EithErrDo.Types (Error (..), IOEither)
import           Prelude         (IO)

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EithErrDo.Edo as E
-- >>> import Data.Text (Text, toUpper)
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

-- | Ensure a finaliser runs after the computation (success or failure).
-- @since 0.1.0
--
-- Runs the finalizer even on 'Left', and preserves the original result.
---
-- ==== __Examples__
-- >>> let testFinallyRight = do
-- ...       r   <- newIORef ([] :: [String])
-- ...       res <- E.finallyE (E.ok "X" :: E.IOEither Err ()) (modifyIORef' r ("fin":))
-- ...       logs <- readIORef r
-- ...       pure (res, logs)
-- >>> testFinallyRight
-- (Right "X",["fin"])
--
-- >>> let testFinallyLeft = do
-- ...       r   <- newIORef ([] :: [String])
-- ...       res <- E.finallyE (E.bad (Bug "e") :: E.IOEither Err ()) (modifyIORef' r ("fin":))
-- ...       logs <- readIORef r
-- ...       pure (res, logs)
-- >>> testFinallyLeft
-- (Left (Bug "e"),["fin"])
finallyE :: Error e => forall a. IOEither e a -> IO () -> IOEither e a
finallyE = Re.finallyE

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
-- ...           use _     = modifyIORef' logRef ("useBad":) >> E.bad (ParseErr "e")
-- ...       res  <- E.bracketE acquire release use
-- ...       logs <- readIORef logRef
-- ...       pure (res, logs)
-- >>> testBracketUseFail
-- (Left (ParseErr "e"),["rel:R","useBad","acq"])
--
-- Failing acquire ⇒ release is not called:
--
-- >>> let testBracketAcquireFail = do
-- ...       logRef <- newIORef ([] :: [String])
-- ...       let acquire   = modifyIORef' logRef ("acqFail":) >> E.bad (Bug "fail")
-- ...           release a = modifyIORef' logRef (("rel:"<>a):)
-- ...           use _     = modifyIORef' logRef ("use?":) >> E.ok ()
-- ...       res  <- E.bracketE acquire release use
-- ...       logs <- readIORef logRef
-- ...       pure (res, logs)
-- >>> testBracketAcquireFail
-- (Left (Bug "fail"),["acqFail"])
bracketE :: Error e => forall a b. IOEither e a -> (a -> IO ()) -> (a -> IOEither e b) -> IOEither e b
bracketE = Re.bracketE
