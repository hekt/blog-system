{-# LANGUAGE OverloadedStrings #-}

module KirstieHooksXmlSitemapSpec where

import           Test.Hspec
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           System.Directory
import           System.FilePath

import Web.Kirstie.Model
import Web.Kirstie.DB
import Web.Kirstie.Hooks.XmlSitemap

import Util

spec :: Spec
spec = do
  describe "Web.Kirstie.Hooks.XmlSitemap" $ do
    let template = BL.concat [ "{{blog.url}}", "{{blog.lastmod}}"
                             , "{{#articles}}"
                             , "{{aid}}", "{{lastmod}}"
                             , "{{/articles}}" ]
    it "generate" $ onTestSpace $ \conf pipe path -> do
      let expect   = BS.concat [ "http://www.example.com/"
                               , "2014-04-15T17:12:00Z"
                               , "1", "2000-12-03T04:56:07Z"
                               , "2", "2014-04-15T17:12:00Z"
                               , "3", "1858-11-17T00:00:00Z" ]
          articles = map (\(i, t) -> minimal { articleIdNum = i
                                             , articleLastModified = t }) $
                     zip [1..] $ map read [ "2000-12-03 04:56:07"
                                          , "2014-04-15 17:12:00"
                                          , "1858-11-17 00:00:00" ]
          tempDir = templateDirectory conf
          htmlDir = htmlDirectory conf
      createDirectoryIfMissing True tempDir
      createDirectoryIfMissing True htmlDir
      saveArticlesToDB conf articles
      BL.writeFile (tempDir </> "xml-sitemap.xml") template
      xmlSitemap conf []
      BS.readFile (htmlDir </> "sitemap.xml") `shouldReturn` expect

    it "empty database" $ onTestSpace $ \conf pipe path -> do
      let expect = BS.concat [ "http://www.example.com/" 
                             , "1858-11-17T00:00:00Z" ]
          tempDir = templateDirectory conf
          htmlDir = htmlDirectory conf
      createDirectoryIfMissing True tempDir
      createDirectoryIfMissing True htmlDir
      BL.writeFile (tempDir </> "xml-sitemap.xml") template
      xmlSitemap conf []
      BS.readFile (htmlDir </> "sitemap.xml") `shouldReturn` expect
