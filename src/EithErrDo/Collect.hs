{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}

module EithErrDo.Collect
  ( sequenceE, traverseE, forE, sequenceE_, traverseE_, forE_
  ) where

import EithErrDo.Types   (IOEither, Error(..)) 
import qualified EitherDo.Collect as Cl ( sequenceE, traverseE, forE, sequenceE_, traverseE_, forE_ )

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EithErrDo.Edo as E
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text as T
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

-- | Sequence a list of @IOEither@, short-circuiting on the first error.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.sequenceE [E.ok 1, E.ok 2, E.ok 3 :: E.IOEither Err Int]
-- Right [1,2,3]
-- >>> E.sequenceE [E.ok 1, E.bad (Bug "error1"), E.ok 2, E.bad (NotFound "error2") :: E.IOEither Err Int]
-- Left (Bug "error1")
sequenceE :: Error e => forall a. [IOEither e a] -> IOEither e [a]
sequenceE = Cl.sequenceE

-- | Traverse with an @IOEither@ effect, short-circuiting on error.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f n = if n > 0 then E.ok (n*2) else E.bad (Bug "error") :: E.IOEither Err Int
-- >>> E.traverseE f [1,2,3]
-- Right [2,4,6]
-- >>> E.traverseE f [1,0,2]
-- Left (Bug "error")
traverseE :: Error e => forall x a. (x -> IOEither e a) -> [x] -> IOEither e [a]
traverseE = Cl.traverseE

-- | Flipped 'traverseE' for pipeline style.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f n = if n > 0 then E.ok (n*2) else E.bad (NotFound "error") :: E.IOEither Err Int
-- >>> E.forE [1,2,3] f
-- Right [2,4,6]
-- >>> E.forE [1,0,2] f
-- Left (NotFound "error")
forE :: Error e => forall x a. [x] -> (x -> IOEither e a) -> IOEither e [a]
forE = Cl.forE

-- | Like 'sequenceE' but discards results.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.sequenceE_ [E.ok 1, E.ok 2, E.ok 3 :: E.IOEither Err Int]
-- Right ()
-- >>> E.sequenceE_ [E.ok 1, E.bad (ParseErr "error1"), E.ok 2, E.bad (Bug "error2") :: E.IOEither Err Int]
-- Left (ParseErr "error1")
sequenceE_ :: Error e => forall a. [IOEither e a] -> IOEither e ()
sequenceE_ = Cl.sequenceE_

-- | Flipped 'traverseE_' for pipeline style.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f n = if n > 0 then E.ok (n*2) else E.bad (Bug "error") :: E.IOEither Err Int
-- >>> E.forE_ [1,2,3] f
-- Right ()
-- >>> E.forE_ [1,0,2] f
-- Left (Bug "error")
traverseE_ :: Error e => forall x a. (x -> IOEither e a) -> [x] -> IOEither e ()
traverseE_ = Cl.traverseE_

-- | Flipped 'traverseE_' for pipeline style.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f n = if n > 0 then E.ok (n*2) else E.bad (ParseErr "error") :: E.IOEither Err Int
-- >>> E.forE_ [1,2,3] f
-- Right ()
-- >>> E.forE_ [1,0,2] f
-- Left (ParseErr "error")
forE_ :: Error e => forall x a. [x] -> (x -> IOEither e a) -> IOEither e ()
forE_ = Cl.forE_
