# Changelog for plzwrk

## 0.0.0.8

- Fixes a bug in the HSX parser that rejected certain valid text nodes

## 0.0.0.7

- Removes spurious dependencies for faster build.

## 0.0.0.6

- Adds `#el` mixin for element lists.

## 0.0.0.5

- Adds bounds for cabal packages
- Explicity declares `Control.Monad.Fail` in `HSX.hs` to allow automated haddock builds.

## 0.0.0.4

- Adds `hsx` and `hsx'` for `jsx`-like manipulation.

## 0.0.0.3

- Adds bindings for `fetch`
- Simplifies `Browserful` API to only contain primitives and uses utility functions to build on top of the primitives.

## 0.0.0.2

- Adds server side rendering via `toHTML` and `plzwrkSSR`.
- Adds more documentation.

## 0.0.0.1

- Adds util functions for attribute creation like `wClass`, `wId` etc.

## 0.0.0.0

- Initial release.
