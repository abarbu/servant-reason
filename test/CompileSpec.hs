{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module CompileSpec where

import           Test.Hspec
import           Test.Mockery.Directory

import           Control.Exception
import           Control.Monad                (when)
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Reason                          (toReasonDecoderSource,
                                               toReasonEncoderSource,
                                               toReasonTypeSource)
import           Servant.API                  (NoContent)
import           Servant.Reason
import           System.Directory             (canonicalizePath,
                                               createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               getCurrentDirectory, removeFile,
                                               setCurrentDirectory)
import           System.Process

import Common (Book, testApi)

main :: IO ()
main =
  hspec spec

spec :: Test.Hspec.Spec
spec = do
  describe "generateReasonForAPI" $ do
    it "creates compilable javascript" $ do
      inTempReasonDir $ do
        let generated =
              T.intercalate "\n\n" $
                defReasonImports :
                [ toReasonTypeSource (Proxy :: Proxy NoContent)
                , toReasonTypeSource (Proxy :: Proxy Book)
                , toReasonDecoderSource (Proxy :: Proxy Book)
                , toReasonEncoderSource (Proxy :: Proxy Book)
                ] ++
                generateReasonForAPI testApi
        T.writeFile "src/Api.re" generated
        callCommand "yarn build"

inTempReasonDir :: IO a -> IO a
inTempReasonDir action = do
  cacheExists <- doesDirectoryExist "_test-cache"
  when (not cacheExists)
    createCache
  cacheSrcExists <- doesDirectoryExist "_test-cache/src"
  when (not cacheSrcExists) $ do
    createDirectoryIfMissing True "_test-cache/src"
  cacheDir <- canonicalizePath "_test-cache"
  inTempDirectory $ do
    callCommand ("cp -r " ++ cacheDir ++ "/* .")
    action

createCache :: IO ()
createCache = do
  createDirectoryIfMissing True "_test-cache"
  withCurrentDirectory "_test-cache" $ do
    writeFile "package.json" $ unindent $ [i|
{
  "name": "json-test-project",
  "version": "0.1.0",
  "scripts": {
    "build": "bsb -make-world",
    "start": "bsb -make-world -w",
    "clean": "bsb -clean-world"
  },
  "keywords": [
    "BuckleScript"
  ],
  "author": "",
  "license": "MIT",
  "devDependencies": {
    "bs-platform": "^5.0.0"
  },
  "dependencies": {
    "@glennsl/bs-json": "^3.0.0",
    "bs-fetch": "^0.3.1"
  }
}
    |]
    writeFile "bsconfig.json" $ unindent $ [i|
{
  "name": "json-test-project",
  "version": "0.1.0",
  "sources": {
    "dir" : "src",
    "subdirs" : true
  },
  "package-specs": {
    "module": "commonjs",
    "in-source": true
  },
  "suffix": ".bs.js",
  "bs-dependencies": [
      "bs-fetch",
      "@glennsl/bs-json"
  ],
  "warnings": {
    "error" : "+101"
  },
  "namespace": true,
  "refmt": 3
}
    |]
    callCommand "yarn install"
    compileDependencies

compileDependencies :: IO ()
compileDependencies = do
  pure ()

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket enter recover (const action)
  where
    enter = do
      original <- getCurrentDirectory
      setCurrentDirectory dir
      return original
    recover = setCurrentDirectory
