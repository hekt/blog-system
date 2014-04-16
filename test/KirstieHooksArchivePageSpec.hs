{-# LANGUAGE OverloadedStrings #-}

module KirstieHooksArchivePageSpec where

import           Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           System.Directory
import           System.FilePath
import           System.Posix.Files

import Web.Kirstie.Model
import Web.Kirstie.DB
import Web.Kirstie.Hooks.ArchivePage

import Util

spec :: Spec
spec = do
  describe "Web.Kirstie.Hooks.ArchivePage" $ do
    it "all values" $ onTestSpace $ \conf pipe path -> do
      let template = BL.concat [ "{{config_data.blogUrl}}"
                               , "{{#articles}}"
                               , "{{aid}}", "{{title}}"
                               , "{{pubdate.date}}", "{{pubdate.year}}"
                               , "{{pubdate.month}}", "{{pubdate.day}}"
                               , "{{#tags}}"
                               , "{{tag}}", "{{encoded_tag}}"
                               , "{{/tags}}"
                               , "{{/articles}}" ]
          articles = [ minimal { articleTitle = "hoge"
                               , articleTags  = ["foo", "bar"] } ]
          expect   = BS.concat [ "http://www.example.com/"
                               , "0", "hoge"
                               , "1858-11-17", "1858", "Nov", "17"
                               , "foo", "foo", "bar", "bar" ]
          tempFile = templateDirectory conf </> "archive-page.html"
          htmlFile = htmlDirectory conf </> "archive.html"

      saveArticlesToDB conf articles
      createDirectories conf
      BL.writeFile tempFile template
      archivePage conf articles
      BS.readFile htmlFile `shouldReturn` expect

    it "empty database" $ onTestSpace $ \conf pipe path -> do
      let expect   = "http://www.example.com/"
          template = BL.concat [ "{{config_data.blogUrl}}"
                               , "{{#articles}}", "foo", "{{/articles}}" ]
          tempFile = templateDirectory conf </> "archive-page.html"
          htmlFile = htmlDirectory conf </> "archive.html"

      createDirectories conf
      BL.writeFile tempFile template
      archivePage conf []
      BS.readFile htmlFile `shouldReturn` expect

    it "all articles" $ onTestSpace $ \conf pipe path -> do
      let template = BL.concat ["{{#articles}}", "0", "{{/articles}}"]
          articles = map (\i -> minimal {articleIdNum = i}) [1..20]
          expect   = BS.concat $ replicate (length articles) "0"
          tempFile = templateDirectory conf </> "archive-page.html"
          htmlFile = htmlDirectory conf </> "archive.html"

      saveArticlesToDB conf articles
      createDirectories conf
      BL.writeFile tempFile template
      archivePage conf articles
      BS.readFile htmlFile `shouldReturn` expect

createDirectories :: Configure -> IO ()
createDirectories conf = do
  createDirectoryIfMissing False $ templateDirectory conf
  createDirectoryIfMissing False $ htmlDirectory conf
