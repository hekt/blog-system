{-# LANGUAGE OverloadedStrings #-}

module KirstieHooksRelatedPostsSpec where

import           Test.Hspec
import           Data.List (sort)
import           Data.Bson (at)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Database.MongoDB hiding (sort)
import           System.Directory
import           System.FilePath

import Web.Kirstie.Model
import Web.Kirstie.DB
import Web.Kirstie.Hooks.RelatedPosts

import Util

spec :: Spec
spec = do
  describe "Web.Kirstie.Hooks.RealtedPosts" $ do
    it "test" $ onTestSpace $ \conf pipe path -> do
      let articles  = buildArticles [ (1, ["foo", "bar", "baz"])
                                    , (2, ["foo", "bar"])
                                    , (3, ["foo"])
                                    , (4, ["baz"])
                                    , (5, ["qux"])]
          expect    = [ (1, [2,4,3])
                      , (2, [1,3])
                      , (3, [2,1])
                      , (4, [1])
                      , (5, []) ]
      saveArticlesToDB conf articles
      relatedPosts conf articles
      result <- access pipe master (databaseName conf) $
                rest =<< find (select [] "related_posts")
      fmap (sort . docs2pairs) result `shouldBe` Right (sort expect)

    it "empty database" $ onTestSpace $ \conf pipe path -> do
      relatedPosts conf []
      result <- access pipe master (databaseName conf) $
                findOne (select [] "related_posts")
      result `shouldBe` Right Nothing

docs2pairs :: [Document] -> [(Int, [Int])]
docs2pairs = map f
    where f d = ("id" `at` d, "relateds" `at` d)

buildArticles :: [(ArticleId, [T.Text])] -> [Article]
buildArticles = map f
    where f (i, ts) = minimal {articleIdNum = i, articleTags = ts}
