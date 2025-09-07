{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}
{-# OPTIONS_HADDOCK not-home   #-}

module EitherDo.Collect
  ( sequenceE
  , traverseE
  , forE
  , sequenceE_
  , traverseE_
  , forE_
  ) where

import           Prelude ( const, flip, foldr, (<$>))
import EitherDo.Core

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EitherDo.Edo as E
-- >>> import Data.IORef
-- >>> import qualified Control.Exception as X
-- >>> :set -XQualifiedDo


-- | Sequence a list of @IOEither@, short-circuiting on the first error.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.sequenceE [E.ok 1, E.ok 2, E.ok 3 :: E.IOEither String Int]
-- Right [1,2,3]
-- >>> E.sequenceE [E.ok 1, E.bad "error1", E.ok 2, E.bad "error2" :: E.IOEither String Int]
-- Left "error1"
sequenceE :: [IOEither e a] -> IOEither e [a]
sequenceE []     = ok []
sequenceE (m:ms) = m >>= \a -> sequenceE ms >>= \as -> ok (a:as)

-- | Traverse with an @IOEither@ effect, short-circuiting on error.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f n = if n > 0 then E.ok (n*2) else E.bad "error" :: E.IOEither String Int
-- >>> E.traverseE f [1,2,3]
-- Right [2,4,6]
-- >>> E.traverseE f [1,0,2]
-- Left "error"
traverseE :: (x -> IOEither e a) -> [x] -> IOEither e [a]
traverseE f xs = sequenceE (f <$> xs)

-- | Flipped 'traverseE' for pipeline style.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f n = if n > 0 then E.ok (n*2) else E.bad "error" :: E.IOEither String Int
-- >>> E.forE [1,2,3] f
-- Right [2,4,6]
-- >>> E.forE [1,0,2] f
-- Left "error"
forE :: [x] -> (x -> IOEither e a) -> IOEither e [a]
forE = flip traverseE

-- | Like 'sequenceE' but discards results.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.sequenceE_ [E.ok 1, E.ok 2, E.ok 3 :: E.IOEither String Int]
-- Right ()
-- >>> E.sequenceE_ [E.ok 1, E.bad "error1", E.ok 2, E.bad "error2" :: E.IOEither String Int]
-- Left "error1"
sequenceE_ :: [IOEither e a] -> IOEither e ()
sequenceE_ = foldr (\m acc -> m >>= const acc) (ok ())

-- | Like 'traverseE' but discards results.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f n = if n > 0 then E.ok (n*2) else E.bad "error" :: E.IOEither String Int
-- >>> E.traverseE_ f [1,2,3]
-- Right ()
-- >>> E.traverseE_ f [1,0,2]
-- Left "error"
traverseE_ :: (x -> IOEither e a) -> [x] -> IOEither e ()
traverseE_ f xs = sequenceE_ (f <$> xs)

-- | Flipped 'traverseE_' for pipeline style.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f n = if n > 0 then E.ok (n*2) else E.bad "error" :: E.IOEither String Int
-- >>> E.forE_ [1,2,3] f
-- Right ()
-- >>> E.forE_ [1,0,2] f
-- Left "error"
forE_ :: [x] -> (x -> IOEither e a) -> IOEither e ()
forE_ = flip traverseE_
