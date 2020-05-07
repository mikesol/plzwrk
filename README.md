# plzwrk

A Haskell front-end framework.

## Hello world

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Web.Framework.Plzwrk
import Web.Framework.Plzwrk.Asterius

main :: IO ()
main = do
  browser <- asteriusBrowser
  plzwrk'_ [hsx|<p>Hello world!</p>|] browser
```

See it [live](https://plzwrk-hello-world.surge.sh).

## Kitchen sink

Check out the code [here](./kitchen-sink/Main.hs).

See it [live](https://plzwrk-kitchen-sink.surge.sh).

## Installation

Add `plzwrk` to the `build-depends` stanza of your `.cabal` file.

Also, add `plzwrk-X.Y.Z.?` to the `extra-deps` list of your `stack.yaml` file if you're using stack.

## Making a webpage

`plzwrk` uses [Asterius](https://github.com/tweag/asterius) as its backend for web development. Compiling an application using `plzwrk` is no different than compiling an application using `ahc-cabal` and `ahc-dist` as described in the [Asterius documentation](https://asterius.netlify.app) with **one caveat**. You **must** use `--constraint "plzwrk +plzwrk-enable-asterius"` when running `ahc-cabal`.

A minimal flow is shown below, mostly copied from the asterius documentation. It assumes that you have a cabal-buildable project in the `pwd`. Note the use of the `--constraint "plzwrk +plzwrk-enable-asterius"` flag in the `ahc-cabal` step.

```bash
username@hostname:~/my-dir$ docker run --rm -it -v $(pwd):/project -w /project terrorjack/asterius
asterius@hostname:/project$ ahc-cabal v2-update
asterius@hostname:/project$ ahc-cabal new-install --constraint "plzwrk +plzwrk-enable-asterius" --installdir <inst-dir> <exec-name>
asterius@hostname:/project$ cd <inst-dir> && ahc-dist --input-exe <exec-name> --browser --bundle
```

## Documentation

The main documentation for `plzwrk` is on [hackage](https://hackage.haskell.org/package/plzwrk). The four importable modules are:

- `Web.Frameworks.Plzwrk` for the basic functions
- `Web.Frameworks.Plzwrk.Tag` for helper functions to make takes like `input` or `br` if you are not using `hsx`.
- `Web.Frameworks.Plzwrk.MockJSVal` to use a mock browser.
- `Web.Frameworks.Plzwrk.Asterius` to use a bindings for a real browser courtesy of [Asterius](https://github.com/tweag/asterius).

## Design

`plzwrk` is inspired by [redux](https://redux.js.org/) for its state management. The main idea is that you have a HTML-creation function that is composed, via `<*>`, with getters from a state.

```haskell

-- State
data MyState = MkMyState { _name :: Text, age :: Int, _tags :: [Text] }

-- Function hydrating a DOM with elementse from the state
makeP = (\name age -> [hsx'|<p>#t{concat [name, " is the name and ", show age, " is my age."]}#</p>|] <$> _name <*> _age

-- The same function using functional tags instead of hsx
makeP = (\name age -> p'__ concat [name, " is the name and ", show age, " is my age."]) <$> _name <*> _age
```

HTML-creation functions can be nested, allowing for powerful abstractions.

```haskell
nested = div_ (take 10 $ repeat makeP)
```

### HSX

`hsx` is not unlike `jsx`. The main difference is that instead of using just `{}`, `hsx` uses three different varieties of `#{}#`

- `#e{}#` for an element
- `#t{}#` for a text node or text attribute
- `#c{}#` for a callback attribute

### Hydrating with a state

HTML-creation functions use an apostrophe after the tag name (ie `div'`) if they accept arguments from a state and no apostrophe (ie `div`) if they don't. The same is true of `hsx`, ie `[hsx|<br />|]` versus `(s -> [hsx'|<br />|])`. Additionally, HTML-creation functions for tags that do not have any attributes (class, style etc) are marked with a trailing underscore (`div_ [p__ "hello"]`), and tags that only accept text are marked with two trailing underscores (`p__ "hello"`).

### Event handlers

Event handlers take two arguments - an opaque pointer to the event and the current state - and return a new state (which could just be the original state) in the `IO` monad. For example, if the state is an integer, a valid event handler could be:

```
eh :: opq -> Int -> IO Int
eh _ i = pure $ i + 1
dom = [hsx|<button click=#c{eh}#>Click here</button>|]
```

To handle events (ie extract values from input events, etc) you can use one of the functions exported by `Web.Framework.Plzwrk`. Please see the [hackage documentation](https://hackage.haskell.org/package/plzwrk) for more information.

> If you are using the Asterius backend, callback functions are still quite fragile and subject to breakage. The less third-party libraries you use in them, the better. For example, avoid using `Data.Text` and `aeson` if possible.

## Server side rendering

Plzwrk supports server side rendering. To do this, you have to compile your site twice:
- once using `ahc-cabal` using the procedure above to create any JavaScript you need (ie event handlers), and
- once using plain old `cabal` to create the inital HTML.

When compiling using `ahc-cabal`, make sure to use the `plzwrkSSR` family of functions. These functions will look for pre-existing elements in the DOM and attach event listeners to them instead of creating elements from scratch. Additionally, if the static website needs to be initialized with data (ie using the result of an HTTP response made on the server), you'll need to pass these values dynamically to the function that calls `plzwrkSSR`. You can do this using the `foreign export` syntax as described in the [Asterius documentation](https://asterius.netlify.app/jsffi.html#jsffi-static-exports).

When compiling with `cabal`, you'll likely be using it to output an HTML document or build a server that serves your website as `text/html`. Regardless of the approach, you should use `toHTML` to create the part of the initial DOM controlled by plzwrk.  Also, in your HTML, make sure to include a link to the script(s) produced by `ahc-dist` and, if needed, make sure to call your exported functions.

## Testing your code

Plzwrk comes with a mock browser that can act as a drop-in replacement for your browser. Use this in your tests.

```haskell
import Web.Framework.Plzwrk.MockJSVal

main :: IO ()
    browser <- makeMockBrowser
    print "Now I'm using the mock browser."
```

## When to use

Plzwrk may be a good fit if you enjoy the benefits of programming in Haskell and would like to create a web app.

Plzwrk is experimental. It is unfit for production and the syntax will change frequently, often in non-backward-compatible ways. We will try to document all of these changes in the [changelog](ChangeLog.md).

Some alternatives to `plzwrk`:

- [Elm](https://elm-lang.org/), a delightful language for reliable web apps.
- [Purescript react basic](https://github.com/lumihq/purescript-react-basic), an opinionated set of bindings to the React library, optimizing for the most basic use cases.

## Contributing

Thanks for your interest in contributing! If you have a bug or feature request, please file an [issue](https://github.com/meeshkan/plzwrk/issues), or if you'd like to hack at the code base, please propose a [pull request](https://github.com/meeshkan/plzwrk/issues).
