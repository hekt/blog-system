{-# LANGUAGE OverloadedStrings #-}

module KirstieHooksAmazonAssociatesSpec where

import           Test.Hspec
import qualified Data.Text as T

import Web.Kirstie.Model
import Web.Kirstie.Hooks.AmazonAssociates

import Util

spec :: Spec
spec = do
  describe "Web.Kirstie.Hooks.AmazonAssociates" $ do
    let asin = "4274068854"
        name = "hoge"
        body = T.concat [ "<p>foo</p>"
                        , "[amazon]", name, ":", asin, "[/amazon]"
                        , "<p>bar</p>" ]
        url = T.concat [ "http://www.amazon.co.jp/dp/"
                       , asin, "?tag=hekt-22" ]
        img = T.concat [ "http://images-jp.amazon.com/images/P/"
                       , asin, ".09.LZZZZZZZ.jpg" ]
        expectBody = T.concat [ "<p>foo</p>"
                              , "<dl class=\"amazon-associates\">"
                              , "<dt class=\"image\">商品画像</dt>"
                              , "<dd class=\"image\">"
                              , "<a href=\"", url, "\">"
                              , "<img src=\"",  img, "\">"
                              , "</a>"
                              , "</dd>"
                              , "<dt class=\"name\">商品名</dt>"
                              , "<dd class=\"name\">"
                              , "<a href=\"", url, "\">", name, "</a>"
                              , "</dd>"
                              , "</dl>"
                              , "<p>bar</p>"]
    it "include asin tag" $ onTestSpace $ \conf pipe path -> do
      let articles = [minimal {articleContent = body}]
          expect   = [minimal {articleContent = expectBody}]
      amazonAssociates conf articles `shouldReturn` expect

    it "no asin tag" $ onTestSpace $ \conf pipe path -> do
      let articles = [minimal] :: [Article]
      amazonAssociates conf articles `shouldReturn` articles

    it "mixed" $ onTestSpace $ \conf pipe path -> do
      let articles = [minimal, minimal {articleContent = body}]
          expect   = [minimal, minimal {articleContent = expectBody}]
      amazonAssociates conf articles `shouldReturn` expect
