{-# LANGUAGE NoImplicitPrelude #-}

module EithErrDo.Lift
  ( hoistEither, fromMaybeE, guardE
  ) where

import EithErrDo.Types   (IOEither, Error) 
import qualified EitherDo.Lift    as Li ( hoistEither, fromMaybeE, guardE )
import Prelude (Either(..), Maybe(..), Bool(..), ($))
import Data.Bifunctor (first)

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

-- | Lift a pure 'Either' into 'IOEither' you must provide something of the kind Error e.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.hoistEither Bug (Right (7 :: Int))
-- Right 7
-- >>> E.hoistEither Bug (Left "err" :: Either Text Int)
-- Left (Bug "err")
hoistEither :: Error e => forall a. (e' -> e) -> Either e' a -> IOEither e a
hoistEither err eith = Li.hoistEither $ first err eith

-- | Convert a 'Maybe' to 'IOEither', providing an error for 'Nothing'.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.fromMaybeE (Bug "none") (Just 'x')
-- Right 'x'
-- >>> E.fromMaybeE (Bug "none") (Nothing :: Maybe Int)
-- Left (Bug "none")
fromMaybeE :: Error e => forall a. e -> Maybe a -> IOEither e a
fromMaybeE = Li.fromMaybeE

-- | Succeed with @()@ when predicate holds; otherwise fail with the error.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.guardE True (ParseErr "err")
-- Right ()
-- >>> E.guardE False (NotFound "err")
-- Left (NotFound "err")
guardE :: Error e => Bool -> e -> IOEither e ()
guardE = Li.guardE
