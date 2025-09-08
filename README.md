# edo — Qualified do-notation for `IO (Either e a)`

> Write ergonomic, composable code in the `IO (Either e a)` style—without a transformer stack. Bring your own error type, keep `IO` at the base, and use `QualifiedDo` to get sleek `E.do` blocks.

---

## Why edo?

Many Haskell codebases want a single, explicit error channel and `IO` as the base monad. The common answer is `ExceptT e IO a`. That works—but it also introduces a transformer, instances, and a little mental overhead.

**edo** takes a simpler route:

- **Keep it concrete.** The effect you run is *exactly* `IO (Either e a)`.
- **Use combinators, not a stack.** Handle `Left`/`Right` with small, predictable helpers.
- **Enjoy clean syntax.** With `QualifiedDo`, you can write `E.do` blocks that short-circuit on `Left` like you’d expect from `ExceptT`, but stay inside plain `IO (Either e a)`.

At the heart is the type alias:

```haskell
type IOEither e a = IO (Either e a)
```

This makes signatures shorter and emphasises that **`IOEither` is just a synonym** for the raw form—no wrappers, no hidden cost.

---

## Quick taste

```haskell
{-# LANGUAGE QualifiedDo #-}

import qualified EitherDo.Edo as E

data Err = MissingConfig | ParseFail | DbError String

fetchConfig :: E.IOEither Err Int
fetchConfig = pure (Right 3)

doOne :: Int -> E.IOEither Err ()
doOne n = if n < 5 then E.ok () else E.bad (DbError "too big")

pipeline :: E.IOEither Err ()
pipeline = E.do
  n <- fetchConfig
  _ <- E.traverseE_ doOne [1..n]
  E.ok ()
```

---

## Installing

**Cabal**

```cabal
build-depends:
    base ^>= 4.18
  , edo  ^>= 0.1
```

**Nix flakes (example)**

```nix
hp = pkgs.haskellPackages;
edoPkg = hp.callCabal2nix "edo" (fetchGit { url = "https://github.com/your/edo"; }) {};
```

---

## Development

If you’re hacking on edo locally, you can use the flake to generate and browse documentation via Hoogle:

```bash
nix run '.#hoogle'
```

This will build Haddock docs, generate a Hoogle database, and launch a local server where you can explore the library API.

---

## Contributing

### Running tests

Tests are written as **doctests**, meaning that the examples in the Haddocks double as executable test cases.

You can run them with:

```bash
cabal test
```

or directly:

```bash
cabal repl --repl-options="-fno-warn-unused-do-bind" edo
cabal repl edo
:! doctest src
```

If you’re using Nix:

```bash
nix develop
cabal test
```

Any example you see in the documentation is also validated automatically.

---

## API tour (selected)

Import qualified:

```haskell
import qualified EitherDo.Edo as E
```

### Constructors & binders
- `type IOEither e a = IO (Either e a)`
- `E.ok   :: a -> IOEither e a`
- `E.bad  :: e -> IOEither e a`
- `(>>=), (=<<), (>>), return` – `QualifiedDo`-friendly re-exports for `E.do`

### Lifting & conversions
- `E.hoistEither :: Either e a -> IOEither e a`
- `E.fromMaybeE  :: e -> Maybe a -> IOEither e a`
- `E.guardE      :: e -> Bool -> IOEither e ()`

…and more (see Haddocks for the full set).

---

## License

BSD-3-Clause
