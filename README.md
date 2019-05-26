# Servant Reason

[![Build Status](https://travis-ci.org/abarbu/servant-reason.svg)](https://travis-ci.org/abarbu/servant-reason)
<img src="https://cdn.svgporn.com/logos/reasonml.svg" alt="reason" height="20"/>
<img src="https://www.haskell.org/img/haskell-logo.svg" alt="reason" height="20"/>

Automatically derive bindings for Servant APIs in Reason.  Originally build by
converting [servant-elm](http://hackage.haskell.org/package/servant-elm) to
Reason. Types are generated using
[reason-export](https://github.com/abarbu/reason-export).

More docs on [Hackage](http://hackage.haskell.org/package/servant-reason).

A full example of how to integrate Servant and Reason is available at
[servant-reason-example](https://github.com/abarbu/servant-reason-example).

## Usage

Run the tests if you want a concrete example. They will build a _test-cache
directory that contains a sample Reason repository for a servant API. test also
contain example output files for different kinds of bindings.

Usage is simple and automatic. If you have an API like the one below

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Reason          (Spec (Spec), specsToDir, toReasonDecoderSource,
                               toReasonTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Reason  (ReasonType, Proxy (Proxy), defReasonImports,
                               generateReasonForAPI)

data Book = Book
    { name :: String
    } deriving (Generic)

instance ReasonType Book

type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book
```

You can expose your API to reason with:

```haskell
module Main where

import Data.Proxy
import Reason
import           Data.Text hiding (intercalate, map)
import Db

main :: IO ()
main = do
  let code = defReasonImports
             : toReasonTypeSource    (Proxy :: Proxy Book)
             : toReasonDecoderSource (Proxy :: Proxy Book)
             : generateReasonForAPI  (Proxy :: Proxy BooksApi))
  writeFile "Api.re" $ intercalate "\n\n" $ map unpack code
```

That's it. You can now include `Api.re` in a Reason project.

## Reason setup

The generated code needs access to two Reason libraries
[@glennsl/bs-json](https://github.com/glennsl/bs-json) and
[bs-fetch](https://github.com/reasonml-community/bs-fetch). Get the latest
install instructions from the upstream repos, but at the time of writing these
were:

```sh
npm install --save @glennsl/bs-json
npm install --save bs-fetch
```

Then add `@glennsl/bs-json` and `bs-fetch` to `bs-dependencies` in your `bsconfig.json`:
```js
{
  ...
  "bs-dependencies": ["@glennsl/bs-json", "bs-fetch"]
}
```
