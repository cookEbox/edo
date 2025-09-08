# edo

⚠️ **Work in progress — not yet released on Hackage/Stackage** ⚠️

**edo** provides a lightweight `QualifiedDo` syntax and helper functions
for working with the common pattern `IO (Either e a)`.

It aims to make writing error-aware `IO` code ergonomic without introducing
a full monad transformer stack.

## Example

```haskell
{-# LANGUAGE QualifiedDo #-}

import qualified EitherDo.Edo as E

main :: IO ()
main = do
  r <- E.do
    n <- readConfigE             -- :: IO (Either Text Int)
    _ <- E.traverseE_ doOne [1..n]
    E.ok "done!"
  print r
