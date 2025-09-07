# edo — Qualified do-notation for `IO (Either e a)`

> Write ergonomic, composable code in the `IO (Either e a)` style—without a transformer stack. Bring your own error type, keep `IO` at the base, and use `QualifiedDo` to get sleek `E.do` blocks.

---

## Why edo?

Many Haskell codebases want a single, explicit error channel and `IO` as the base monad. The common answer is `ExceptT e IO a`. That works—but it also introduces a transformer, instances, and a little mental overhead.

**edo** takes a simpler route:

- **Keep it concrete.** The effect you run is *exactly* `IO (Either e a)`.
- **Use combinators, not a stack.** Handle `Left`/`Right` with small, predictable helpers.
- **Enjoy clean syntax.** With `QualifiedDo`, you can write `E.do` blocks that short-circuit on `Left` like you’d expect from `ExceptT`, but stay inside plain `IO (Either e a)`.

If you like the clarity of `Either` and the explicitness of `IO`, edo gives you a tidy middle-ground: *transformer-like ergonomics* without the transformer.

---

## Quick taste

```haskell
{-# LANGUAGE QualifiedDo #-}

import qualified EitherDo.Edo as E

-- Choose an error type that fits your domain
data Err = MissingConfig | ParseFail | DbError String

fetchConfig :: IO (Either Err Int)
fetchConfig = pure (Right 3)

doOne :: Int -> IO (Either Err ())
doOne n = if n < 5 then E.ok () else E.bad (DbError "too big")

pipeline :: IO (Either Err ())
pipeline = E.do
  n <- fetchConfig           -- :: IO (Either Err Int)
  _ <- E.traverseE_ doOne [1..n]
  E.ok ()
```

### What’s going on?
- `E.do` gives you familiar `do`-notation that short-circuits on the first `Left`.
- `E.ok` / `E.bad` are constructors for `Right` / `Left` inside `IO`.
- `E.traverseE_` runs a list of `IO (Either e a)` actions, stopping on the first error.

Result type is **exactly** `IO (Either Err ())`—no newtype and no transformer.

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
# overlay snippet
hp = pkgs.haskellPackages;
edoPkg = hp.callCabal2nix "edo" (fetchGit { url = "https://github.com/your/edo"; }) {};
```

(Adjust to however you pin Haskell deps in your flake.)

---

## Core ideas

- **Explicit effects**: errors are data (`e`), effects are `IO`.
- **Local reasoning**: you can read any type signature and see the control flow surface.
- **Combinator-first**: recovery, mapping, branching, and lifting helpers are plain functions.
- **Interop-friendly**: it’s just `IO (Either e a)`—easy to consume/produce at boundaries.

---

## API tour (selected)

Import qualified:

```haskell
import qualified EitherDo.Edo as E
```

### Constructors & binders
- `type IOEither e a = IO (Either e a)`
- `E.ok   :: a   -> IOEither e a` – produce a successful result
- `E.bad  :: e   -> IOEither e a` – produce an error
- `(>>=), (=<<), (>>), return` – `QualifiedDo`-friendly re-exports for `E.do`

### Lifting & conversions
- `E.hoistEither :: Either e a -> IOEither e a`
- `E.fromMaybeE  :: e -> Maybe a -> IOEither e a`
- `E.guardE      :: e -> Bool -> IOEither e ()`

### Error mapping & recovery
- `E.mapErrorE :: (e -> e') -> IOEither e a -> IOEither e' a`
- `E.recoverE  :: IOEither e a -> (e -> IOEither e a) -> IOEither e a`
- `E.orElse    :: IOEither e a -> IOEither e a -> IOEither e a`

### Branching / side effects
- `(|>)     :: a -> (a -> b) -> b` – forward-pipe helper (ergonomic, optional)
- `E.onRight :: IOEither e a -> (a -> IO ()) -> IO ()` – run an `IO` callback on success
- `E.traverseE_ :: (x -> IOEither e a) -> [x] -> IOEither e ()` – stop-on-first-error traversal

> Note: The above list highlights common entry points. See Haddocks for the full surface.

---

## Using `QualifiedDo`

Enable the language extension and qualify your import:

```haskell
{-# LANGUAGE QualifiedDo #-}
import qualified EitherDo.Edo as E
```

Now you can write `E.do` wherever the surrounding type is `IO (Either e a)`:

```haskell
foo :: IO (Either Text Int)
foo = E.do
  a <- parseSomething
  b <- queryDb a
  E.ok (a + b)
```

Short-circuiting works the way you expect: any bound action that returns `Left e` exits the block early with that error.

---

## When to choose **edo** vs **ExceptT**

**Pick edo if…**
- Your base is *always* `IO` and you don’t need to stack effects.
- You want explicit, concrete types at boundaries (`IO (Either e a)` is ideal for APIs).
- You prefer small, local combinators over a general transformer stack.

**Pick ExceptT if…**
- You do have (or may later need) a transformer stack (`ReaderT`, `StateT`, etc.).
- You rely on generic `mtl`-style constraints (`MonadError e m`, etc.).
- You want typeclass-driven polymorphism over the effect shape.

**Bridging**
- Converting is trivial: `runExceptT :: ExceptT e IO a -> IO (Either e a)` and `ExceptT :: IO (Either e a) -> ExceptT e IO a`.
- In other words, **using edo is not a one-way door**. Start explicit; lift later if needed.

---

## Motivating examples

### 1) Early-exit validation + IO

```haskell
validate :: Text -> Either Err UserId
validate = ...

lookupUser :: UserId -> IO (Either Err User)
lookupUser = ...

fetch :: Text -> IO (Either Err User)
fetch raw = E.do
  uid <- E.hoistEither (validate raw)
  user <- lookupUser uid
  E.ok user
```

### 2) Map and recover errors

```haskell
load :: FilePath -> IO (Either Err ByteString)
load = ...

loadStrict :: FilePath -> IO (Either Text ByteString)
loadStrict fp =
  E.mapErrorE explain (load fp)
  where
    explain :: Err -> Text
    explain = ...

loadOrDefault :: FilePath -> IO (Either Err ByteString)
loadOrDefault fp =
  E.recoverE (load fp) $ \e ->
    case e of
      MissingConfig -> E.ok mempty
      _             -> E.bad e
```

### 3) Traversal that aborts on first failure

```haskell
migrateAll :: [Step] -> IO (Either Err ())
migrateAll steps = E.traverseE_ run steps
  where
    run :: Step -> IO (Either Err ())
    run = ...
```

---

## Design notes

- **Predictability** over abstraction: the library aims to be transparent and obvious in how it composes.
- **No orphan instances or magic**: functionality is delivered via plain functions & operators; you opt in explicitly.
- **Small surface**: a curated set of helpers to cover 80% of use-cases for `IO (Either e a)`.

---

## Performance & lawfulness

- The main payoff is *clarity of control flow* and cheap interop at boundaries.
- Semantics follow those of `Either` and `IO`. Short-circuiting in `E.do` mirrors `EitherT`/`ExceptT` behavior.
- There’s no performance penalty for a transformer layer because there isn’t one; results are already in the target representation.

(For real workloads, benchmark your specific code paths.)

---

## Interop patterns

- **Pure Either** ➜ `IOEither`: `E.hoistEither`
- **Maybe** ➜ `IOEither`: `E.fromMaybeE err maybeVal`
- **Guarding**: `E.guardE err predicate` fails with `err` when `predicate` is `False`.
- **Callbacks**: `E.onRight action (\a -> ...)` for fire-and-forget side effects on success.

---

## FAQ

**Is this a replacement for `ExceptT`?**

*Sometimes.* For apps that want explicit `IO (Either e a)` and don’t need a stack, edo can feel lighter and clearer. For polymorphic code over `MonadError` or multi-effect stacks, `ExceptT` remains a great fit.

**Why `QualifiedDo`?**

It makes the common path look nice while keeping the types concrete. You can still use all combinators without `E.do`.

**Can I mix edo with other styles?**

Yes. Use edo at the edges, `ExceptT` in modules that prefer it, and convert at the boundaries. The representations are isomorphic over `IO`.

---

## Minimal example project

```haskell
{-# LANGUAGE QualifiedDo #-}
module Main where

import qualified EitherDo.Edo as E

main :: IO () = do
  r <- program
  case r of
    Left  e -> putStrLn ("Error: " <> show e)
    Right _ -> putStrLn "OK"

program :: IO (Either String ())
program = E.do
  E.guardE "precond failed" True
  E.ok ()
```

---

## License

BSD-3-Clause

---

## Acknowledgements

Thanks to the Haskell community for the great building blocks we stand on: `Either`, `IO`, and the `QualifiedDo` extension that makes this style pleasant.

