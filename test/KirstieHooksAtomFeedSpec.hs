{-# LANGUAGE OverloadedStrings #-}

module KirstieHooksAtomFeedSpec where

import           Test.Hspec
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           System.Directory
import           System.FilePath

import Web.Kirstie.Model
import Web.Kirstie.DB
import Web.Kirstie.Hooks.AtomFeed

import Util

spec :: Spec
spec = do
  describe "Web.Kirstie.Hooks.XmlSitemap" $ do

    it "all values" $ onTestSpace $ \conf pipe path -> do
      let template = BL.concat [ "{{blog.url}}", "{{blog.lastmod}}"
                               , "{{#articles}}"
                               , "{{aid}}", "{{lastmod}}"
                               , "{{& title}}", "{{& content}}"
                               , "{{#tags}}"
                               , "{{tag}}", "{{encoded_tag}}"
                               , "{{/tags}}"
                               , "{{/articles}}" ]
          expect   = BS.concat [ "http://www.example.com/"
                               , "1858-11-17T00:00:00Z"
                               , "0", "1858-11-17T00:00:00Z"
                               , "hoge", "<p>fuga</p>"
                               , "foo", "foo", "bar baz", "bar%20baz" ]
          articles = [ minimal { articleTitle = "hoge"
                               , articleContent = "<p>fuga</p>"
                               , articleTags = ["foo", "bar baz"] } ]
          tempFile = templateDirectory conf </> "atom-feed.xml"
          htmlFile = htmlDirectory conf </> "atom.xml"
      saveArticlesToDB conf articles
      createDirectories conf
      BL.writeFile tempFile template
      atomFeed conf []
      BS.readFile htmlFile `shouldReturn` expect

    it "empty database" $ onTestSpace $ \conf pipe path -> do
      let template = BL.concat ["{{blog.url}}", "{{blog.lastmod}}"]
          expect   = BS.concat [ "http://www.example.com/" 
                               , "1858-11-17T00:00:00Z" ]
          tempFile = templateDirectory conf </> "atom-feed.xml"
          htmlFile = htmlDirectory conf </> "atom.xml"
      createDirectories conf
      BL.writeFile tempFile template
      atomFeed conf []
      BS.readFile htmlFile `shouldReturn` expect

    it "blog's last mod" $ onTestSpace $ \conf pipe path -> do
      let template = "{{blog.lastmod}}"
          time     = read "2000-01-01 00:00:00"
          articles = [minimal, minimal {articleLastModified = time}]
          expect   = "2000-01-01T00:00:00Z"
          tempFile = templateDirectory conf </> "atom-feed.xml"
          htmlFile = htmlDirectory conf </> "atom.xml"
      saveArticlesToDB conf articles
      createDirectories conf
      BL.writeFile tempFile template
      atomFeed conf []
      BS.readFile htmlFile `shouldReturn` expect

    it "more than 10 articles" $ onTestSpace $ \conf pip path -> do
      let template = "{{#articles}}{{aid}}{{/articles}}"
          articles = map (\i -> minimal {articleIdNum = i}) [1..15]
          tempFile = templateDirectory conf </> "atom-feed.xml"
          htmlFile = htmlDirectory conf </> "atom.xml"
      saveArticlesToDB conf articles
      createDirectories conf
      BL.writeFile tempFile template
      atomFeed conf []
      BS.readFile htmlFile `shouldReturn` "12345678910"
          
createDirectories :: Configure -> IO ()
createDirectories conf = do
  createDirectoryIfMissing True $ templateDirectory conf
  createDirectoryIfMissing True $ htmlDirectory conf
