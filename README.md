# plzwrk

Yet another Haskell front-end framework.

## Hello world

```haskell
import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag (p__)

main :: IO ()
main = do
  browser <- asteriusBrowser
  plzwrk'_ (p__ "Hello world!") browser
```

See it [live](https://plzwrk-hello-world.surge.sh).

## Kitchen sink

Check out the code [here](./kitchen-sink/Main.hs).

See it [live](https://plzwrk-kitchen-sink.surge.sh).

## Installation

Add `plzwrk` to the `build-depends` stanza of your `.cabal` file.

Also, add `plzwrk-X.Y.Z.?` to the `extra-deps` list of your `stack.yaml` file if you're using stack.

## Making a webpage

`plzwrk` uses [Asterius](https://github.com/tweag/asterius) as its backend for web development. Compiling an application using `plzwrk` is no different than compiling an application using `ahc-cabal` and `ahc-dist` as described in the [Asterius documentation](https://asterius.netlify.app) with **one caveat**. You **must** use `-f plzwrk-enable-asterius` when running `ahc-cabal`.

A minimal flow is shown below, mostly copied from the asterius documentation. It assumes that you have a cabal-buildable project in the root directory. Note the use of the `-f plzwrk-enable-asterius` flag in the `ahc-cabal` step.

```bash
username@hostname:~/my-dir$ docker run --rm -it -v $(pwd):/project -w /project terrorjack/asterius
asterius@hostname:/project$ ahc-cabal new-install -f plzwrk-enable-asterius --installdir <inst-dir> <exec-name>
asterius@hostname:/project$ cd <inst-dir> && ahc-dist --input-exe <exec-name> --browser --bundle
```

## Documentation

The main documentation for `plzwrk` is on [hackage](https://https://hackage.haskell.org/package/plzwrk). The four importable modules are:

- `Web.Frameworks.Plzwrk` for the basic functions
- `Web.Frameworks.Plzwrk.Tag` for helper functions to make takes like `input` or `br`.
- `Web.Frameworks.Plzwrk.MockJSVal` to use a mock browser.
- `Web.Frameworks.Plzwrk.Asterius` to use a bindings for a real browser courtesy of [Asterius](https://github.com/tweag/asterius).

## Design

`plzwrk` is inspired by [redux](https://redux.js.org/) for its state management. The main idea is that you have a HTML-creation function that accepts one or more variables from a state that is composed, via applicative functors, with getters from a state.

```haskell

-- State
data MyState = MkMyState { _name :: Text, age :: Int, _tags :: [Text] }

-- Function hydrating a DOM with elementse from the state
makeP = (\name age -> p'__ concat [name, " is the name and ", show age, " is my age."]) <$> _name <*> _age
```

HTML-creatino functions can be nested, allowing for powerful abstractions.

```haskell
nested = div_ (take 10 $ repeat makeP)
```

The convention is that HTML-creation functions use an apostrophe after the tag name (ie `div'`) if they accept arguments and no apostrophe (ie `div`) if they don't. Additionally, tags that do not have any attributes (class, style etc) are marked with a trailing underscore (`div_ [p__ "hello"]`), and tags that only accept text are marked with two trailing underscores (`p__ "hello"`).

The HTML-creation function itself should be pure with type `(s -> Node s opq)`, where `s` is the type of the state and `opq` is the type of the opaque pointer used to represent a JavaScript value.  `opq` will rarely need to be provided manually and is induced from the compiler based on the `Browserful` being used (ie `asteriusBrowser`).

Event handlers take two arguments - an opaque pointer to the event and the current state - and must return a new state (which could just be the original state) in the `IO` monad. For example, if the state is an integer, a valid event handler could be:

```
eh :: opq -> Int -> IO Int
eh _ i = pure $ i + 1
```

To handle events (ie extract values from input events, etc) you can use one of the functions exported by `Web.Framework.Plzwrk`. Please see the [hackage documentation](https://hackage.haskell.org/package/plzwrk) for more information.

> If you are using the Asterius backend, callback functions are still quite fragile and subject to breakage. The less third-party libraries you use in them, the better. For example, avoid using `Data.Text` and `aeson` if possible.

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

Thanks for your interest in contributing! Pull requests are welcome :)

In general, most development can be done without touching the bindings to Asterius. If you compile the project with `ahc-cabal`, it will intermittently fail at the linking phase because `ahc-cabal` struggles with multiple executables due to a race condition in its parallel processing. If this happens, just run `ahc-cabal` again.