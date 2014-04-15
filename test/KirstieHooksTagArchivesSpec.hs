{-# LANGUAGE OverloadedStrings #-}

module KirstieHooksTagArchivesSpec where

import           Test.Hspec
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           System.Directory
import           System.FilePath

import Web.Kirstie.Model
import Web.Kirstie.DB
import Web.Kirstie.Hooks.TagArchives

import Util

spec :: Spec
spec = do
  describe "Web.Kirstie.Hooks.TagArchives" $ do
    it "all tag archives generated" $ onTestSpace $ \conf pipe path -> do
      let articles =
              [ minimal {articleIdNum = 1, articleTags = ["foo"]}
              , minimal {articleIdNum = 2, articleTags = ["bar", "baz"]} ]
          expect   = [".", "..", "foo.html", "bar.html", "baz.html"]
          tempFile = templateDirectory conf </> "tag-archives.html"
      createDirectories conf
      writeFile tempFile ""
      saveArticlesToDB conf articles
      tagArchives conf articles
      result <- getDirectoryContents $ htmlDirectory conf </> "tags"
      result `shouldMatchList` expect

    it "filename escape" $ onTestSpace $ \conf pipe path -> do
      let articles = [ minimal {articleTags = ["foo/bar\0baz\NULqux"]}]
          expect   = [".", "..", "foo-bar-baz-qux.html"]
          tempFile = templateDirectory conf </> "tag-archives.html"
      createDirectories conf
      writeFile tempFile ""
      saveArticlesToDB conf articles
      tagArchives conf articles
      result <- getDirectoryContents $ htmlDirectory conf </> "tags"
      result `shouldMatchList` expect

    it "all values" $ onTestSpace $ \conf pipe path -> do
      let articles = [ minimal { articleTitle   = "hoge"
                               , articleTags    = ["foo"] } ]
          template = BL.concat [ "{{config_data.blogUrl}}"
                               , "{{tag_name}}"
                               , "{{#articles}}"
                               , "{{aid}}", "{{title}}"
                               , "{{pubdate.date}}", "{{pubdate.year}}"
                               , "{{pubdate.month}}", "{{pubdate.day}}"
                               , "{{#tags}}"
                               , "{{tag}}", "{{encoded_tag}}"
                               , "{{/tags}}"
                               , "{{/articles}}"]
          expect   = BS.concat [ "http://www.example.com/", "foo"
                               , "0", "hoge"
                               , "1858-11-17", "1858", "Nov", "17"
                               , "foo", "foo"]
      createDirectories conf
      BL.writeFile (templateDirectory conf </> "tag-archives.html") template
      saveArticlesToDB conf articles
      tagArchives conf articles
      BS.readFile (htmlDirectory conf </> "tags/foo.html")
            `shouldReturn` expect

createDirectories :: Configure -> IO ()
createDirectories conf = do
  createDirectoryIfMissing True $ templateDirectory conf
  createDirectoryIfMissing True $ htmlDirectory conf </> "tags"
