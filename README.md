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

## Three modules: EitherDo, EithErrDo, and ReadErrDo

The library provides **three** entry points:

- **`EitherDo`** — the base module, easiest for new users who just want ergonomic `IO (Either e a)` code.  
- **`EithErrDo`** — a stricter variant that encourages using a dedicated `Error` type class for error management (with adapters `ViaShow` / `ViaException`).  
- **`ReadErrDo`** — a tiny Reader-style layer that threads a read-only config through your `IO (Either e a)` programs **without transformers** (`ask`, `local`, `view`, `run/load`) while keeping the same error model.

👉 New users may start with `EitherDo`, but if you plan to use this library for **serious error handling**, we recommend moving to `EithErrDo`. If you also need a shared configuration, reach for **`ReadErrDo`**.

---

## Quick taste (EitherDo)

```haskell
{-# LANGUAGE QualifiedDo #-}

import qualified EitherDo.Edo as E

-- Keep it simple: use String directly for errors
fetchConfig :: E.IOEither String Int
fetchConfig = pure (Right 3)

doOne :: Int -> E.IOEither String ()
doOne n = if n < 5 then E.ok () else E.bad "too big"

pipeline :: E.IOEither String ()
pipeline = E.do
  n <- fetchConfig
  _ <- E.traverseE_ doOne [1..n]
  E.ok ()

main :: IO ()
main = do
  putStrLn "Running pipeline..."
  r <- pipeline
  -- Normal IO do-block interleaving with E.do pipeline
  case r of
    Left err -> putStrLn ("ERROR: " ++ err)
    Right () -> putStrLn "OK"
```

---

## Quick taste (EithErrDo)

```haskell
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified EithErrDo.Edo as E
import Control.Exception (IOException, try)
import qualified Data.Text as T

-- Domain error type that can also carry real exceptions
data MyError
  = MissingConfig
  | ParseFail
  | DbTooBig
  | IoErr (E.ViaException IOException)     -- <— adapter for IOException
  deriving stock (Show, Eq)
  -- Reuse Show for pretty display of *domain* errors:
  deriving (E.Error) via (E.ViaShow MyError)

type IOEither e a = E.IOEither e a

-- Step 1: fetch some config (domain-level success)
fetchConfig :: IOEither MyError Int
fetchConfig = E.ok 7

-- Step 2: do a domain-level check that can fail
ensureSmall :: Int -> IOEither MyError ()
ensureSmall n = if n < 5 then E.ok () else E.bad DbTooBig

-- Step 3: perform an IO action that can throw (we catch and adapt)
readCfgFile :: FilePath -> IOEither MyError T.Text
readCfgFile fp = do
  r <- try @IOException (readFile fp)     -- IO (Either IOException String)
  case r of
    Left ioe  -> E.bad (IoErr (E.ViaException ioe))  -- <— wrap exception coherently
    Right str -> E.ok (T.pack str)

-- Step 4: another pure/domain step (won't run if an earlier Left occurs)
parseCfg :: T.Text -> IOEither MyError Int
parseCfg t =
  case reads (T.unpack t) of
    [(n, "")] -> E.ok n
    _         -> E.bad ParseFail

-- The pipeline: the *first* Left short-circuits the rest.
-- Try changing the filename to an existing file to see later steps run.
pipeline :: IOEither MyError Int
pipeline = E.do
  n  <- fetchConfig              -- OK: 7
  _  <- ensureSmall n            -- Left DbTooBig (since 7 >= 5) ⇒ short-circuit
  t  <- readCfgFile "missing.txt" -- This step is SKIPPED if DbTooBig already occurred
  x  <- parseCfg t                -- Also skipped if earlier Left
  E.ok x

-- End-of-world handling: collect the first error or the final result
main :: IO ()
main = do
  r <- pipeline
  case r of
    Left e  -> T.putStrLn ("ERROR: " <> E.displayError e)
    Right x -> T.putStrLn ("OK: value = " <> T.pack (show x))
```

**What this buys you (chaining and first-failure wins):**
- Each step returns `IO (Either MyError a)`; the qualified `E.do` chains them.
- The **first `Left`** (domain error like `DbTooBig` *or* wrapped exception like `IoErr …`)
  **short-circuits** the rest, so later steps don’t run.
- At the end, you get a single `Either MyError result` to handle in one place.
- `ViaException` and `ViaShow` are the glue: they let you mix domain errors and caught exceptions
  **without writing bespoke `Error` instances** or risking orphan/overlapping instances.

> With plain **EitherDo**, you can still chain `IO (Either e a)`, but you’d have to write
> your own `Error` instances (or skip a unifying class). `EithErrDo`’s adapters let you
> drop in existing `Exception`/`Show` types and keep everything coherent and consistent.

---

## Quick taste (ReadErrDo)

A tiny Reader-style layer to thread a read-only config through your `IO (Either e a)` program—no transformers required.

```haskell
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified ReadErrDo.Edo as R
import qualified EithErrDo.Edo as E
import qualified Data.Text as T

-- Define an application error and derive Error via Show
data AppErr = MissingVar T.Text | BadPort T.Text | DbErr T.Text
  deriving (Eq, Show)
deriving via (E.ViaShow AppErr) instance E.Error AppErr

-- Read-only configuration threaded through the program
data Config = Config { fallbackPort :: Int, defaultUser :: T.Text }

-- Pure parse that we can lift later
parsePort :: String -> Either AppErr Int
parsePort s =
  case reads s of
    [(n,"")] | n > 0 && n < 65536 -> Right n
    _                             -> Left (BadPort (T.pack s))

-- A simple IOEither action we’ll call from ReadErrDo
greet :: T.Text -> E.IOEither AppErr T.Text
greet who = E.ok (T.concat ["Hello, ", who, "!"])

-- The program: ask for config, try to read PORT (simulated), recover to fallback
program :: R.ReadErrDo Config AppErr T.Text
program = do
  cfg <- R.ask
  let fallback = fallbackPort cfg
      who      = defaultUser  cfg

  mEnv <- R.liftIO (pure (Nothing :: Maybe String))  -- simulate unset PORT

  port <- R.recover (const (pure fallback)) $ do
    portStr <- R.liftIOEither (E.fromMaybeE (MissingVar "PORT") mEnv)
    R.liftEither (parsePort portStr)

  _ <- R.liftIOEither (E.ok port) -- pretend to use the port
  R.liftIOEither (greet who)

main :: IO ()
main = do
  let cfg = Config { fallbackPort = 8080, defaultUser = "Nick" }
  r <- R.load cfg program
  case r of
    Left  e -> putStrLn ("ERROR: " <> T.unpack (E.displayError e))
    Right t -> putStrLn (T.unpack t)
```

**Why ReadErrDo?**
- Keep effects **concrete** (`cfg -> IO (Either e a)` under the hood).
- Get **Reader conveniences** (`ask`, `local`, `view`) with your existing `IOEither` flow.
- **Interop** seamlessly with `EithErrDo` helpers and `Error` adapters.

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
