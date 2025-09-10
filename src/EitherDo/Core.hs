{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}
{-# OPTIONS_HADDOCK not-home   #-}

module EitherDo.Core
  ( IOEither
  , (>>=)
  , (=<<)
  , (>>)
  , return
  , ok
  , bad
  , (|>)
  , onRight
  ) where

import qualified Prelude as P
import           Prelude ( Either(..), IO 
                         , const, either, flip
                         , (.)
                         , pure
                         )

-- $setup
-- >>> import Prelude hiding ((>>=), return, (>>), (>=>))
-- >>> import qualified EitherDo.Edo as E
-- >>> import Data.IORef
-- >>> import qualified Control.Exception as X
-- >>> :set -XQualifiedDo


-- | Convenient synonym for the common pattern @IO (Either e a)@.
-- @since 0.1.0
--
-- >>> import qualified EitherDo.Edo as E
-- >>> :set -XQualifiedDo
-- >>> (E.ok True :: E.IOEither String Bool)
-- Right True
type IOEither e a = IO (Either e a)

-- | Qualified-@do@ bind for @IOEither@.
--
-- Lets you write @E.do@ blocks that short-circuit on @Left@.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.do x <- E.ok (2 :: Int); E.ok (x + 1)
-- Right 3
-- >>> E.do _ <- E.bad "boom" :: E.IOEither String Int; E.ok (99 :: Int)
-- Left "boom"
(>>=) :: IOEither e a -> (a -> IOEither e b) -> IOEither e b
(>>=) = onRight

-- | Reverse bind
--
-- @since 0.1.0
--
(=<<) :: (a -> IOEither e b) -> IOEither e a -> IOEither e b
(=<<) = flip (>>=)

-- | Sequencing for @IOEither@; discards the first result.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.do E.ok () >> E.ok "x" :: E.IOEither String String
-- Right "x"
-- >>> E.do E.bad "e" >> E.ok "x" :: E.IOEither String String
-- Left "e"
(>>) :: IOEither e a -> IOEither e b -> IOEither e b
m1 >> m2 = m1 >>= const m2

-- | Pipeline sugar: same behavior as >>=.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f x = if x > 0 then E.ok (x*2) else E.bad "neg" :: E.IOEither String Int
-- >>> (E.ok 3 :: E.IOEither String Int) E.|> f
-- Right 6
-- >>> (E.ok 0 :: E.IOEither String Int) E.|> f
-- Left "neg"
(|>) :: IOEither e a -> (a -> IOEither e b) -> IOEither e b
(|>) = onRight
infixr 0 |>

-- | Pipe the @Right@ value into a continuation; if @Left@, propagate it.
-- @since 0.1.0
--
-- Synonym of '(>>=)' specialized to readability in pipelines.
onRight :: IOEither e a -> (a -> IOEither e b) -> IOEither e b
onRight io f = io P.>>= either (pure . Left) f

-- | Lift a pure value into @Right@.
-- @since 0.1.0
--
return :: a -> IOEither e a
return = pure . Right

-- | Alias for 'return'.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.ok "hi" :: E.IOEither String String
-- Right "hi"
ok :: a -> IOEither e a
ok = return

-- | Construct a failed computation.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.bad "nope" :: E.IOEither String Int
-- Left "nope"
bad :: e -> IOEither e a
bad = pure . Left
