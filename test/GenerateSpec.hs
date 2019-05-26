{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module GenerateSpec where

import           Control.Monad             (zipWithM_)
import qualified Data.Algorithm.Diff       as Diff
import qualified Data.Algorithm.DiffOutput as Diff
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Servant.API               ((:>), Get, JSON)
import           Servant.Reason
import           Reason
import           Test.Hspec                (Spec, describe, hspec, it, runIO)
import           Test.HUnit                (Assertion, assertBool)

import           Common                    (testApi, Book)


main :: IO ()
main = hspec spec

spec :: Test.Hspec.Spec
spec = do
    runIO $ putStrLn $ T.unpack $ toReasonTypeSourceWith defaultOptions (Proxy :: Proxy Book)
    runIO $ putStrLn $ T.unpack $ toReasonDecoderSourceWith defaultOptions (Proxy :: Proxy Book)
    runIO $ putStrLn $ T.unpack $ toReasonEncoderSourceWith defaultOptions (Proxy :: Proxy Book)
    describe "encoding a simple api" $
        do it "does it" $
               do expected <-
                      mapM
                          (\(fpath,header) -> do
                               source <- T.readFile fpath
                               return (fpath, header, source))
                          [ ( "test/reason-sources/getOneSource.re", "")
                          , ( "test/reason-sources/postTwoSource.re", "")
                          , ( "test/reason-sources/getBooksByIdSource.re", "open BookType;\n")
                          , ( "test/reason-sources/getBooksByTitleSource.re", "open BookType;\n")
                          , ( "test/reason-sources/getBooksSource.re", "open BookType;\n")
                          , ( "test/reason-sources/postBooksSource.re", "open BookType;\n")
                          , ( "test/reason-sources/getNothingSource.re", "open BookType;\n")
                          , ( "test/reason-sources/putNothingSource.re", "open BookType;\n")
                          , ( "test/reason-sources/getWithaheaderSource.re", "open BookType;\n")
                          , ( "test/reason-sources/getWitharesponseheaderSource.re", "open BookType;\n")]
                  let generated = map (<> "\n") (generateReasonForAPI testApi)
                  generated `itemsShouldBe` expected
           it "with dynamic URLs" $
               do expected <-
                      mapM
                          (\(fpath,header) -> do
                               source <- T.readFile fpath
                               return (fpath, header, source))
                          [( "test/reason-sources/getOneWithDynamicUrlSource.re", "open BookType;\n")]
                  let generated =
                          map
                              (<> "\n")
                              (generateReasonForAPIWith
                                   (defReasonOptions
                                    { urlPrefix = Dynamic
                                    })
                                   (Proxy :: Proxy ("one" :> Get '[JSON] Int)))
                  generated `itemsShouldBe` expected

itemsShouldBe :: [Text] -> [(String, Text, Text)] -> IO ()
itemsShouldBe actual expected =
    zipWithM_
        shouldBeDiff
        (actual ++ replicate (length expected - length actual) mempty)
        (expected ++ replicate (length actual - length expected) mempty)

shouldBeDiff :: Text -> (String, Text, Text) -> Assertion
shouldBeDiff a (fpath,header,b) =
    assertBool
        ("< generated\n" <> "> " <> fpath <> "\n" <>
         Diff.ppDiff
             (Diff.getGroupedDiff
                  (lines (T.unpack (header <> a)))
                  (lines (T.unpack b))))
        (header <> a == b)
