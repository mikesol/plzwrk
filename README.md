# plzwrk

[![Chat on Gitter](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/meeshkan/community)

A Haskell front-end framework.

Available as a Hackage package: [`plzwrk`](https://hackage.haskell.org/package/plzwrk)

📖 Looking for an overview? [Read our announcement blog post](https://meeshkan.com/blog/introducing-plzwrk/).

## In this document:

* [When to use `plzwrk`](#when-to-use-plzwrk)
* [Examples using `plzwrk`](examples-using-plzwrk)
  * [Hello world](#hello-world)
  * [Aphorism machine](#aphorism-machine)
* [Making a webpage](#making-a-webpage)
* [Documentation](#documentation)
* [Design of `plzwrk`](#design-of-plzwrk)
  * [PWX](#pwx)
  * [Hydrating with a state](#hydrating-with-a-state)
  * [Event handlers](#event-handlers)
* [Server-side rendering](#server-side-rendering)
* [Testing your code](#testing-your-code)
* [Contributing](#contributing)
  * [Local development](#local-development)

## When to use `plzwrk`

`plzwrk` may be a good fit if you enjoy the benefits of programming in Haskell and you'd like to create a web app.

⚠️ Warning: `plzwrk` is experimental. It is unfit for production and the syntax will change frequently, often in non-backward-compatible ways. We will try to document all of these changes in the [changelog](ChangeLog.md).

Some alternatives to `plzwrk`:

- [Elm](https://elm-lang.org/), a delightful language for reliable web apps.
- [Purescript react basic](https://github.com/lumihq/purescript-react-basic), an opinionated set of bindings to the React library, optimizing for the most basic use cases.

## Examples using `plzwrk`

<!-- TODO: Update when PR#5 is merged -->

### Hello world

An example web page that says 'Hello world!'

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Web.Framework.Plzwrk
import Web.Framework.Plzwrk.Asterius

main :: IO ()
main = do
  browser <- asteriusBrowser
  plzwrk'_ [pwx|<p>Hello world!</p>|] browser
```

[See the Hello World example live](https://plzwrk-hello-world.surge.sh).

### Aphorism machine

An Aphorism Machine that spits out and hides universal truths on demand.

[Check out the source code in the `kitchen-sink` directory](./kitchen-sink/Main.hs). Or [see the Aphorism Machine live](https://plzwrk-kitchen-sink.surge.sh).

## Making a webpage

`plzwrk` uses [Asterius](https://github.com/tweag/asterius) as its backend for web development. 

A minimal flow is shown below. It assumes that you have a file called `Main.hs` in the present working directory with a function `main :: IO ()` inside of it, not unlike in our [hello world example](#hello-world).

```bash
username@hostname:~/my-dir$ docker run --rm -it -v $(pwd):/project -w /project meeshkan/plzwrk
asterius@hostname:/project$ ahc-link --input-hs Main.hs --browser --bundle
```

If you're using `ahc-cabal`, compiling an application using `plzwrk` is no different than compiling an application as described in the [Asterius documentation](https://asterius.netlify.app) with **one caveat**. You **must** use `--constraint "plzwrk +plzwrk-enable-asterius"` when running `ahc-cabal`.

## Documentation

The main documentation for `plzwrk` is on [Hackage](https://hackage.haskell.org/package/plzwrk). 

The four importable modules are:

- [`Web.Frameworks.Plzwrk`](https://hackage.haskell.org/package/plzwrk-0.0.0.9/docs/Web-Framework-Plzwrk.html) for the basic functions
- [`Web.Frameworks.Plzwrk.Tag`](https://hackage.haskell.org/package/plzwrk-0.0.0.9/docs/Web-Framework-Plzwrk-Tag.html) for helper functions to make takes like `input` or `br` if you are not using `pwx`.
- [`Web.Frameworks.Plzwrk.MockJSVal`](https://hackage.haskell.org/package/plzwrk-0.0.0.9/docs/Web-Framework-Plzwrk-MockJSVal.html) to use a mock browser.
- [`Web.Frameworks.Plzwrk.Asterius`](https://hackage.haskell.org/package/plzwrk-0.0.0.9/docs/Web-Framework-Plzwrk-Asterius.html) to use a bindings for a real browser courtesy of [Asterius](https://github.com/tweag/asterius).

## Design of `plzwrk`

`plzwrk` is inspired by [Redux](https://redux.js.org/) for its state management. The main idea is that you have an HTML-creation function that is composed, via `<*>`, with getters from a state.

```haskell
-- State
data MyState = MkMyState { _name :: Text, age :: Int, _tags :: [Text] }

-- Function hydrating a DOM with elementse from the state
makeP = (\name age ->
  [pwx'|<p>#t{concat [name, " is the name and ", show age, " is my age."]}#</p>|])
    <$> _name
    <*> _age

-- The same function using functional tags instead of pwx
makeP = (\name age ->
    p'__ concat [name, " is the name and ", show age, " is my age."])
      <$> _name
      <*> _age
```

HTML-creation functions can be nested, allowing for powerful abstractions:

```haskell
nested = div_ (take 10 $ repeat makeP)
```

### PWX

`pwx` is similar to [`jsx`](https://reactjs.org/docs/introducing-jsx.html). The main difference is that instead of only using `{}`, `pwx` uses four different varieties of `#{}#`:

- `#e{}#` for a single element.
- `#el{}#` for a list of elements.
- `#t{}#` for a single piece of text, either as a node in the body of an element or as a text attribute.
- `#c{}#` for a callback attribute.

### Hydrating with a state

HTML-creation functions use an apostrophe after the tag name (ie `div'`) if they accept arguments from a state and no apostrophe (ie `div`) if they don't. The same is true of `pwx`, ie `[pwx|<br />|]` versus `(s -> [pwx'|<br />|])`. 

Additionally, HTML-creation functions for tags that don't have any attributes (class, style, etc) are marked with a trailing underscore (ie `div_ [p__ "hello"]`), and tags that only accept text are marked with two trailing underscores (ie `p__ "hello"`).

### Event handlers

Event handlers take two arguments - an opaque pointer to the event and the current state. Then, it returns a new state (which could also be the original state) in the `IO` monad. 

For example, if the state is an integer, a valid event handler could be:

```haskell
eh :: opq -> Int -> IO Int
eh _ i = pure $ i + 1
dom = [pwx|<button click=#c{eh}#>Click here</button>|]
```

To handle events, you can use one of the functions exported by `Web.Framework.Plzwrk`. This could be useful to extract values from input events, for instance. Please see the [Hackage documentation](https://hackage.haskell.org/package/plzwrk) for more information.

## Server-side rendering

`plzwrk` supports server-side rendering. To do this, you have to compile your site twice:
- Once using `ahc-cabal`. This uses the [procedure outlined in the last section](#design-of-plzwrk) to create any JavaScript you need (ie event handlers), and
- Once using plain old `cabal` to create the inital HTML.

When compiling using `ahc-cabal`, make sure to use the [`plzwrkSSR`](https://hackage.haskell.org/package/plzwrk-0.0.0.9/docs/Web-Framework-Plzwrk.html#v:plzwrkSSR) family of functions. These functions will look for pre-existing elements in the DOM and attach event listeners to them instead of creating elements from scratch. 

There may also be times that the static website needs to be initialized with data (ie using the result of an HTTP response made on the server). In this case, you'll need to pass these values dynamically to the function that calls `plzwrkSSR`. You can do this using the `foreign export` syntax as described in the [Asterius documentation](https://asterius.netlify.app/jsffi.html#jsffi-static-exports).

When compiling with `cabal`, you'll likely be using it to output an HTML document or build a server that serves your website as `text/html`. Regardless of the approach, you should use [`toHTML`](https://hackage.haskell.org/package/plzwrk-0.0.0.9/docs/Web-Framework-Plzwrk.html#v:toHTML) to create the part of the initial DOM controlled by `plzwrk`. In your HTML, make sure to include a link to the script(s) produced by `ahc-dist`. Also, if needed, make sure to call your exported functions.

## Testing your code

`plzwrk` comes with a mock browser that can act as a drop-in replacement for your browser. 

You can use this in your tests:

```haskell
import Web.Framework.Plzwrk.MockJSVal

main :: IO ()
    browser <- makeMockBrowser
    print "Now I'm using the mock browser."
```

## Contributing

Thanks for your interest in contributing! If you have a bug or feature request, please file an [issue](https://github.com/meeshkan/plzwrk/issues). Or if you'd like to hack at the code base, open a [pull request](https://github.com/meeshkan/plzwrk/issues).

Please note that this project is governed by the [Meeshkan Community Code of Conduct](https://github.com/meeshkan/code-of-conduct). By participating, you agree to abide by its terms.

### Local development

1. Clone this repository: `git clone https://github.com/meeshkan/plzwrk.git`
1. Move into the directory: `cd plzwrk`
1. Set up your local environment: You can use this guide from [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) for reference.
