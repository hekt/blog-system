{-# LANGUAGE OverloadedStrings #-}

module KirstieModelSpec where

import           Test.Hspec

import           Control.Exception
import           Control.Monad
import           Data.Text (Text)
import           Data.List (sort, nub)
import           Data.Time.Clock
import           Database.MongoDB hiding (sort)

import Web.Kirstie.Model

import Util

spec :: Spec
spec = do
  describe "Web.Kirstie.Model.minimal" $ do
    it "Int" $ do
      (minimal :: Int) `shouldBe` 0
    it "Integer" $ do
      (minimal :: Integer) `shouldBe` 0
    it "Text" $ do
      (minimal :: Text) `shouldBe` ""
    it "List" $ do
      (minimal :: [Int]) `shouldBe` []
    it "String (same as List Char)" $ do
      (minimal :: String) `shouldBe` ""
    it "Bool" $ do
      (minimal :: Bool) `shouldBe` False
    it "UTCtime" $ do
      show (minimal :: UTCTime) `shouldBe` "1858-11-17 00:00:00 UTC"
    it "Maybe" $ do
      (minimal :: Maybe Int) `shouldBe` Nothing


  describe "Web.Kirstie.Model.articleToTArticle" $ do
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
