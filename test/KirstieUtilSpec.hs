{-# LANGUAGE OverloadedStrings #-}

module KirstieUtilSpec where

import           Test.Hspec

import           Control.Exception
import           Control.Monad
import           Data.Text (Text)
import           Data.List (sort, nub)
import           Data.Time.Clock
import           Database.MongoDB hiding (sort)

import Web.Kirstie.Model
import Web.Kirstie.Util

import Util

spec :: Spec
spec = do
  describe "Web.Kirstie.Util.parseArgs" $ do
    it "parse arguments" $ do
      let args   = ["foo", "--bar", "--baz", "qux", "-quux", "foobar"
                   , "foobaz"]
          expect = [ ("quux",["foobaz","foobar"]), ("baz",["qux"]), ("bar",[])
                   , ("no label",["foo"]) ]
      parseArgs args `shouldBe` expect


  describe "Web.Kirstie.Util.articleToTArticle" $ do
    it "Article" $ do
      let article = minimal { articleTags = ["foo", "bar baz"] }
          tArticle = TArticle { title = minimal
                              , aid   = "0"
                              , pubdate = TPubdate { date  = "1858-11-17"
                                                   , year  = "1858"
                                                   , month = "Nov"
                                                   , day   = "17" }
                              , tags    = [ TTag "foo" "foo"
                                          , TTag "bar baz" "bar%20baz" ]
                              , content = minimal
                              , lastmod = "1858-11-17T00:00:00Z" }
      articleToTArticle article `shouldBe` tArticle


  describe "Web.Kirstie.Util.safeRead" $ do
    it "readable string" $ do
      (safeRead "400" :: Maybe Int) `shouldBe` Just 400

    it "non readable string" $ do
      (safeRead "foo" :: Maybe Int) `shouldBe` Nothing

    it "mixed string" $ do
      (safeRead "4oo" :: Maybe Int) `shouldBe` Just 4


  describe "Web.Kirstie.Util.strErr" $ do
    it "Left Error" $ do
      let e = Left $ userError "foo" :: Either IOError String
      strError e `shouldBe` Left "user error (foo)"

    it "Right" $ do
      let e = Right "foo" :: Either String String
      strError e `shouldBe` Right "foo"


  describe "Web.Kirstie.Util.filenameEncode" $ do
    it "no necessary to encode" $ do
      filenameEncode "foo bar" `shouldBe` "foo bar"

    it "'\\0'" $ do
      filenameEncode "foo\0bar" `shouldBe` "foo-bar"

    it "'\NUL'" $ do
      filenameEncode "foo\NULbar" `shouldBe` "foo-bar"

    it "'/'" $ do
      filenameEncode "foo/bar" `shouldBe` "foo-bar"


  describe "Web.Kirstie.Util.rfcTime" $ do
    it "1858-11-17 00:00:00 UTC" $ do
      rfcTime minimal `shouldBe` "1858-11-17T00:00:00Z"
