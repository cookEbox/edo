{-# LANGUAGE NoImplicitPrelude #-}

module EithErrDo.Kleisli
  ( (>=>), (<=<)
  ) where

import EithErrDo.Types (IOEither, Error(..)) 
import qualified EitherDo.Kleisli as Kl ( (>=>), (<=<) )

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

-- | Kleisli composition for @a -> IOEither e b@.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f x = if x > 0 then E.ok (x+1) else E.bad (ParseErr "neg") :: E.IOEither Err Int
-- >>> let g y = if even y then E.ok (show y) else E.bad (Bug "odd") :: E.IOEither Err String
-- >>> (f E.>=> g) 3
-- Right "4"
-- >>> (f E.>=> g) 2
-- Left (Bug "odd")
-- >>> (f E.>=> g) (-1)
-- Left (ParseErr "neg")
(>=>) :: Error e => forall a b c. (a -> IOEither e b) -> (b -> IOEither e c) -> a -> IOEither e c
(>=>) = (Kl.>=>)

-- | Reverse Kleisli composition.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f x = if x > 0 then E.ok (x+1) else E.bad (ParseErr "neg") :: E.IOEither Err Int
-- >>> let g y = if even y then E.ok (show y) else E.bad (Bug "odd") :: E.IOEither Err String
-- >>> (g E.<=< f) 3
-- Right "4"
-- >>> (g E.<=< f) 2
-- Left (Bug "odd")
-- >>> (g E.<=< f) (-1)
-- Left (ParseErr "neg")
(<=<) :: Error e => forall b c a. (b -> IOEither e c) -> (a -> IOEither e b) -> a -> IOEither e c
(<=<) = (Kl.<=<)
