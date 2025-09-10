{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}

module EithErrDo.Core
  ( (>>=), (=<<), (>>), (|>)
  , return, ok, bad, onRight
  ) where

import qualified EitherDo.Core   as Cr (bad, ok, onRight, return, (=<<), (>>),
                                        (>>=), (|>))
import           EithErrDo.Types

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

-- | Qualified-@do@ bind for @IOEither@.
--
-- Lets you write @E.do@ blocks that short-circuit on @Left@.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.do x <- E.ok (2 :: Int) :: E.IOEither Err Int; E.ok (x + 1)
-- Right 3
-- >>> E.do _ <- E.bad (Bug "boom") :: E.IOEither Err Int; E.ok (99 :: Int)
-- Left (Bug "boom")
(>>=) :: Error e => forall a b. IOEither e a -> (a -> IOEither e b) -> IOEither e b
(>>=) = (Cr.>>=)

-- | Reverse bind
--
-- @since 0.1.0
--
(=<<) :: Error e => forall a b. (a -> IOEither e b) -> IOEither e a -> IOEither e b
(=<<) = (Cr.=<<)

-- | Sequencing for @IOEither@; discards the first result.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.do E.ok () >> E.ok "x" :: E.IOEither Err String
-- Right "x"
-- >>> E.do E.bad (NotFound "e") >> E.ok "x" :: E.IOEither Err String
-- Left (NotFound "e")
(>>) :: Error e => forall a b. IOEither e a -> IOEither e b -> IOEither e b
(>>) = (Cr.>>)

-- | Pipeline sugar: same behavior as >>=.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> let f x = if x > 0 then E.ok (x*2) else E.bad (NotFound "neg") :: E.IOEither Err Int
-- >>> (E.ok 3 :: E.IOEither Err Int) E.|> f
-- Right 6
-- >>> (E.ok 0 :: E.IOEither Err Int) E.|> f
-- Left (NotFound "neg")
(|>) :: Error e => forall a b. IOEither e a -> (a -> IOEither e b) -> IOEither e b
(|>) = (Cr.|>)

-- | Pipe the @Right@ value into a continuation; if @Left@, propagate it.
-- @since 0.1.0
--
-- Synonym of '(>>=)' specialized to readability in pipelines.
onRight :: Error e => forall a b. IOEither e a -> (a -> IOEither e b) -> IOEither e b
onRight = Cr.onRight

-- | Lift a pure value into @Right@.
-- @since 0.1.0
--
return :: Error e => forall a. a -> IOEither e a
return = Cr.return

-- | Alias for 'return'.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.ok "hi" :: E.IOEither Err String
-- Right "hi"
ok :: Error e => forall a. a -> IOEither e a
ok = Cr.ok

-- | Construct a failed computation.
-- @since 0.1.0
--
-- ==== __Examples__
-- >>> E.bad (ParseErr "nope") :: E.IOEither Err Int
-- Left (ParseErr "nope")
bad :: Error e => forall a. e -> IOEither e a
bad = Cr.bad
