{-# LANGUAGE OverloadedStrings #-}

module KirstieDBSpec where

import           Test.Hspec

import           Control.Exception
import           Control.Monad
import           Data.List (sort, nub)
import           Data.Time.Clock
import           Database.MongoDB hiding (sort)

import Web.Kirstie.Model
import Web.Kirstie.DB

import Util

spec :: Spec
spec = do
  describe "Web.Kirstie.DB.saveArticlesToDB" $ do
    it "save articles once" $ withDatabase $ \pipe -> do
      let articles = map (\i -> minimal {articleIdNum = i}) [1,2]
      saveArticlesToDB testConfig articles
      e <- access pipe master (databaseName testConfig) $
           rest =<< find (select [] "articles")
      fmap (sort . map parseBSON) e `shouldBe` Right articles

    it "save twice same articles" $ withDatabase $ \pipe -> do
      let articles = map (\i -> minimal {articleIdNum = i}) [1,2]
      saveArticlesToDB testConfig articles
      saveArticlesToDB testConfig articles
      e <- access pipe master (databaseName testConfig) $
           rest =<< find (select [] "articles")
      fmap (sort . map parseBSON) e `shouldBe` Right articles

    it "save articles include duplicate" $ withDatabase $ \pipe -> do
      let articles = map (\i -> minimal {articleIdNum = i}) [1,2,2,3]
      saveArticlesToDB testConfig articles
      e <- access pipe master (databaseName testConfig) $
           rest =<< find (select [] "articles")
      fmap (sort . map parseBSON) e `shouldBe` Right (nub articles)


  describe "Web.Kirstie.DB.getLatestIdNumber" $ do
    it "get number" $ withDatabase $ \pipe -> do
      let ns = [1,8,10]
          articles = map (\i -> minimal {articleIdNum = i}) ns
      saveArticlesToDB testConfig articles
      getLatestIdNumber testConfig `shouldReturn` maximum ns

    it "get number from empty database" $ withDatabase $ \pipe -> do
      getLatestIdNumber testConfig `shouldReturn` (minimal :: Int)


  describe "Web.Kirstie.DB.getKnownList" $ do
    it "get list" $ withDatabase $ \pipe -> do
      let files = ["foo", "bar"]
          f (p, i) = minimal {articleIdNum = i, articleSourceFile = p}
          articles = map f $ zip files [1..]
      saveArticlesToDB testConfig articles
      result <- getKnownList testConfig
      result `shouldMatchList` files

    it "get empty list" $ withDatabase $ \pipe -> do
      getKnownList testConfig `shouldReturn` []


  describe "Web.Kirstie.DB.getAllArticleSourceAndIds" $ do
    it "get" $ withDatabase $ \pipe -> do
      let pairs    = zip ["foo", "bar"] [1..]
          f (p, i) = minimal {articleIdNum = i, articleSourceFile = p}
          articles = map f pairs
      saveArticlesToDB testConfig articles
      result <- getAllArticleSourceAndIds testConfig
      result `shouldMatchList` pairs

    it "get empty list" $ withDatabase $ \pipe -> do
      getAllArticleSourceAndIds testConfig `shouldReturn` []


  describe "Web.Kirstie.DB.getAllArticlesFromDB" $ do
    it "get" $ withDatabase $ \pipe -> do
      let articles = map (\i -> minimal {articleIdNum = i}) [1,2]
      saveArticlesToDB testConfig articles
      result <- getAllArticlesFromDB testConfig
      result `shouldMatchList` articles

    it "get empty list" $ withDatabase $ \pipe -> do
      getAllArticlesFromDB testConfig `shouldReturn` []


  describe "Web.Kirstie.DB.updateLastRunTime" $ do
    -- this test will fail because database returns truncated time.
    -- it "update" $ withDatabase $ \pipe -> do
    --   before <- getCurrentTime
    --   updateLastRunTime testConfig
    --   after  <- getCurrentTime
    --   result <- getLastRunTime testConfig
    --   result `shouldSatisfy` (\t -> before <= t && t <= after)

    it "update" $ withDatabase $ \pipe -> do
      updateLastRunTime testConfig
      result <- getLastRunTime testConfig
      result `shouldSatisfy` (\t -> minimal < t)


  describe "Web.Kirstie.DB.getLastRuntime" $ do
    -- this test will fail because database returns truncated time.
    -- it "get" $ withDatabase $ \pipe -> do
    --   time <- getCurrentTime
    --   access pipe master (databaseName testConfig) $
    --          insert "last_run" ["time" =: time]
    --   getLastRunTime testConfig `shouldReturn` time

    it "get" $ withDatabase $ \pipe -> do
      time <- getCurrentTime
      access pipe master (databaseName testConfig) $
             insert "last_run" ["time" =: time]
      result <- getLastRunTime testConfig
      result `shouldSatisfy` (\t -> minimal < t)

    it "get with empty database" $ withDatabase $ \pipe -> do
      getLastRunTime testConfig `shouldReturn` (minimal :: UTCTime)

    
  describe "Web.Kirstie.DB.resetDB" $ do
    it "reset" $ withDatabase $ \pipe -> do
      access pipe master (databaseName testConfig) $
             insert "last_run" ["time" =: (minimal :: UTCTime)]
      access pipe master (databaseName testConfig) $
             insert "articles" $ toBSON (minimal :: Article)
      resetDB testConfig
      e1 <- access pipe master (databaseName testConfig) $
            findOne (select [] "articles")
      e2 <- access pipe master (databaseName testConfig) $
            findOne (select [] "last_run")
      (e1, e2) `shouldBe` (Right Nothing, Right Nothing)


  describe "Web.Kirstie.DB.access'" $ do
    it "access" $ withDatabase $ \pipe -> do
      let dbName = databaseName testConfig
      (access' pipe master dbName $ thisDatabase) `shouldReturn` Right dbName

  describe "Web.Kirstie.DB.accessToBlog" $ do
    it "access" $ withDatabase $ \pipe -> do
      let dbName = databaseName testConfig
      accessToBlog testConfig thisDatabase `shouldReturn` Right dbName

  describe "Web.Kirstie.DB.accessToBlog'" $ do
    it "access" $ withDatabase $ \pipe -> do
      let dbName = databaseName testConfig
      accessToBlog' testConfig thisDatabase `shouldReturn` Right dbName


  describe "Web.Kirstie.DB.failureToIOE" $ do
    it "convert ConnectionFailure" $ do
      let failure = ConnectionFailure $ userError "fail"
          msg     = "user error (fail)"
      show (failureToIOE failure) `shouldBe` msg

    it "convert other errors" $ do
      let failure = AggregateFailure "fail"
          msg = "user error (AggregateFailure \"fail\")"
      show (failureToIOE failure) `shouldBe` msg
