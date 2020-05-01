# plzwrk

Yet another Haskell front-end framework.

## Hello world

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag (p__)

main :: IO ()
main = do
  browser <- asteriusBrowser
  plzwrk'_ (p__ "Hello world!") browser
```

See it live.

## Kitchen sink

Check out the code [here](./kitchen-sink/Main.hs).

See it live.

## Installation

Add `plzwrk` to the `build-depends` stanza of your `.cabal` file.

Also, add `plzwrk-X.Y.Z.?` to the `extra-deps` list of your `stack.yaml` file if you're using stack.

## Making a webpage

`plzwrk` uses [Asterius](https://github.com/tweag/asterius) as a backend to output to Web Assembly. Compiling an application using `plzwrk` application is no different than compiling an application using `ahc-cabal` and `ahc-dist` as described in the [Asterius documentation](https://asterius.netlify.app) with **one caveat**. You **must** use `-f plzwrk-enable-asterius` when running `ahc-cabal`.

A minimal flow is below, mostly copied from the asterius documentation:

```bash
username@hostname:~/project$ docker run --rm -it -v $(pwd):/project -w /project terrorjack/asterius
asterius@hostname:/project$ ahc-link --input-hs main.hs
asterius@hostname:/project$ ahc-cabal new-install -f plzwrk-enable-asterius --installdir <my-install-dir> <my-executable-name>
asterius@hostname:/project$ cd <my-install-dir> && 
```

## Documentation

The main documentation for `plzwrk` is on hackage. The four importable modules are:

- `Web.Frameworks.Plzwrk` for the basic functions
- `Web.Frameworks.Plzwrk.Tag` for helper functions to make takes like `input` or `br`.
- `Web.Frameworks.Plzwrk.MockJSVal` to use a mock browser.
- `Web.Frameworks.Plzwrk.Asterius` to use a bindings for a real browser courtesy of [Asterius](https://github.com/tweag/asterius).

## Testing your code

Plzwrk comes with a mock browser that can act as a drop-in replacement for your browser. Use this in your tests.

```haskell
import Web.Framework.Plzwrk.MockJSVal

main :: IO ()
    browser <- makeMockBrowser
    print "Now I'm using the mock browser."
```

## Using

Plzwrk should be considered experimental. It is unfit for production and the syntax will change frequently, often in non-backward-compatible ways. There is a [changelog](ChangeLog.md).

## Contributing

Thanks for your interest in contributing!

In general, you'll be able to work on `plzwrk` with `ghc` and not have to invoke `ahc-cabal`. To make sure you didn't break anything, run `stack test`. In general, try to write new tests for non-trivial changes or features.

If you compile the project with `ahc-cabal`, it will intermittently fail at the linking phase because `ahc-cabal` struggles with multiple executables due to a race condition in its parallel processing. If this happens, just run `ahc-cabal` again.