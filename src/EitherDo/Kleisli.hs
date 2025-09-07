{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}
{-# OPTIONS_HADDOCK not-home   #-}

module EitherDo.Kleisli ((>=>), (<=<)) where

import           EitherDo.Core
import           Prelude       (flip)

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EitherDo.Edo as E
-- >>> import Data.IORef
-- >>> import qualified Control.Exception as X
-- >>> :set -XQualifiedDo


-- | Kleisli composition for @a -> IOEither e b@.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f x = if x > 0 then E.ok (x+1) else E.bad "neg" :: E.IOEither String Int
-- >>> let g y = if even y then E.ok (show y) else E.bad "odd" :: E.IOEither String String
-- >>> (f E.>=> g) 3
-- Right "4"
-- >>> (f E.>=> g) 2
-- Left "odd"
-- >>> (f E.>=> g) (-1)
-- Left "neg"
(>=>) :: (a -> IOEither e b) -> (b -> IOEither e c) -> a -> IOEither e c
(f >=> g) a = f a >>= g
infixr 1 >=>  -- same fixity as base

-- | Reverse Kleisli composition.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f x = if x > 0 then E.ok (x+1) else E.bad "neg" :: E.IOEither String Int
-- >>> let g y = if even y then E.ok (show y) else E.bad "odd" :: E.IOEither String String
-- >>> (g E.<=< f) 3
-- Right "4"
-- >>> (g E.<=< f) 2
-- Left "odd"
-- >>> (g E.<=< f) (-1)
-- Left "neg"
(<=<) :: (b -> IOEither e c) -> (a -> IOEither e b) -> a -> IOEither e c
(<=<) = flip (>=>)
infixr 1 <=<
