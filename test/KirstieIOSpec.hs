{-# LANGUAGE OverloadedStrings #-}

module KirstieIOSpec where

import           Test.Hspec

import           Control.Monad
import           Control.Exception
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Calendar
import           Database.MongoDB
import           Foreign.C.Types
import           System.Directory
import           System.FilePath
import           System.Posix.Temp
import           System.Posix.Files

import Web.Kirstie.Model
import Web.Kirstie.IO

testDirectoryName :: String
testDirectoryName = "kirstie"

testConfig :: Configure
testConfig = Configure
             { blogUrl           = "http://www.example.com/"
             , templateDirectory = "template_directory"
             , sourceDirectory   = "source_directory"
             , htmlDirectory     = "output_directory"
             , databaseName      = "KirstieTest"
             , databaseHost      = "localhost"
             }

spec :: Spec
spec = do
  describe "Web.Kirstie.IO.getConf" $ do
    it "get valid file" $ onTemporaryDirectory $ \path -> do
      let name = "test_config_success_1.yaml"
          body = BL.intercalate "\n" 
                 [ "blog_url: http://www.example.com/"
                 , "template_directory: template_directory"
                 , "source_directory: source_directory"
                 , "html_directory: output_directory"
                 , "database_name: KirstieTest"
                 , "database_host: localhost" ]
      BL.writeFile name body
      getConf name `shouldReturn` Right testConfig

    it "get invalid file" $ onTemporaryDirectory $ \path -> do
      let name   = "test_config_failure_1.yaml"
          body   = "foo: bar"
          errMsg = unwords [ "yaml parse error: AesonException \"key"
                         , "\\\"blog_url\\\" not present\"" ]
      BL.writeFile name body
      getConf name `shouldReturn` Left errMsg

    it "get not exist file" $ onTemporaryDirectory $ \path -> do
      getConf "not_exist_file" `shouldThrow` anyIOException


  describe "Web.Kirstie.IO.getArticleFromFile" $ do
    it "get valid file" $ onTemporaryDirectory $ \path -> do
      let name  = "test_article_success.md"
          name' = path </> name
          body  = BL.intercalate "\n" 
                  [ "---"
                  , "title: hoge"
                  , "tags: foo, bar"
                  , "pubdate: " `mappend` (BL.pack $ show (minimal :: Day))
                  , "---"
                  , "fuga" ]
      BL.writeFile name body
      lastMod <- getLastModified name
      let aid  = 100
          article = Article { articleTitle = "hoge" 
                            , articleIdNum = aid
                            , articlePubdate = minimal
                            , articleTags    = ["foo", "bar"]
                            , articleContent = "<p>fuga</p>"
                            , articleSourceFile = name'
                            , articleLastModified = lastMod
                            , articleIsImported   = False }
      getArticleFromFile name' aid `shouldReturn` Right article

    it "get empty file" $ onTemporaryDirectory $ \path -> do
      let name   = "test_article_empty.md"
          name'  = path </> name
          errMsg = name' ++ ": yaml and gfm parse error"
      writeFile name ""
      getArticleFromFile name' 0 `shouldReturn` Left errMsg

    it "get invalid file" $ onTemporaryDirectory $ \path -> do
      let name   = "test_article_invalid.md"
          name'  = path </> name
          body   = BL.intercalate "\n" [ "---", "foo: bar", "---", "" ]
          errMsg = name' ++ ": key \"title\" not present"
      BL.writeFile name body
      getArticleFromFile name' 0 `shouldReturn` Left errMsg


  describe "Web.Kirstie.IO.decodeTemplateFile" $ do
    it "ascii characters" $ onTemporaryDirectory $ \path -> do
      let file = "ascii.txt"
          body = "The quick brown fox jumps over the lazy dog"
      T.writeFile file body
      decodeTemplateFile file `shouldReturn` body

    it "japanese characters" $ onTemporaryDirectory $ \path -> do
      let file = "japanese.txt"
          body = "上品でも冷淡な若槻よりも" `mappend`
                 "下品でも猛烈な浪花節語りに、打ち込むのが自然だと考えるんだ。"
      T.writeFile file body
      decodeTemplateFile file `shouldReturn` body
      

  describe "Web.Kirstie.IO.generateHtml" $ do
    it "generate" $ onTemporaryDirectory $ \path -> do
      let article = Article { articleTitle = "title"
                            , articleIdNum = 0
                            , articlePubdate = 0
                            , articleTags    = ["foo", "bar baz"]
                            , articleContent = "<p>hoge</p>"
                            , articleSourceFile = minimal
                            , articleLastModified = minimal
                            , articleIsImported   = minimal }
          expect  = BS.concat [ BS.pack $ blogUrl testConfig
                              , "title"
                              , "0"
                              , "1858-11-17", "1858", "Nov", "17"
                              , "foo", "foo", "bar baz", "bar%20baz"
                              , "<p>hoge</p>" ]
          template = T.concat [ "{{blog.url}}"
                              , "{{& article.title}}"
                              , "{{article.aid}}"
                              , "{{article.pubdate.date}}"
                              , "{{article.pubdate.year}}"
                              , "{{article.pubdate.month}}"
                              , "{{article.pubdate.day}}"
                              , "{{#article.tags}}"
                              , "{{tag}}"
                              , "{{encoded_tag}}"
                              , "{{/article.tags}}"
                              , "{{& article.content}}" ]
      generateHtmlFile template testConfig article
      BS.readFile (htmlDirectory testConfig </> "archives/0.html") 
            `shouldReturn` expect


  describe "Web.Kirstie.IO.removeHtmlFiles" $ do
    it "html files" $ onTemporaryDirectory $ \path -> do
      let dir = htmlDirectory testConfig
      createDirectory dir
      before <- getDirectoryContents dir
      generateNullFiles $ map (dir </>) ["bar.html", "foo.html"]
      removeHtmlFiles testConfig
      getDirectoryContents dir `shouldReturn` before

    it "other files" $ onTemporaryDirectory $ \path -> do
      let dir = htmlDirectory testConfig
          files = ["bar.py", "foo.hs"]
      createDirectory dir
      generateNullFiles $ map (dir </>) files
      after <- getDirectoryContents dir
      removeHtmlFiles testConfig
      getDirectoryContents dir `shouldReturn` after

    it "mixed files" $ onTemporaryDirectory $ \path -> do
      let dir = htmlDirectory testConfig
          isNotHtml = (/= ".html") . takeExtension
      createDirectory dir
      generateNullFiles $ map (dir </>) ["bar.html", "foo.hs"]
      before <- getDirectoryContents dir
      removeHtmlFiles testConfig
      getDirectoryContents dir `shouldReturn` filter isNotHtml before


  describe "Web.Kirstie.IO.getUpdatedMdFiles" $ do
    it "update markdown files all" $ onTemporaryDirectory $ \path -> do
      let files = ["bar.md", "foo.md"]
      generateNullFiles files
      getUpdatedMdFiles minimal "./" `shouldReturn` map ("." </>) files

    it "no updated files" $ onTemporaryDirectory $ \path -> do
      generateNullFiles ["bar.md", "foo.md"]
      time <- getCurrentTime
      getUpdatedMdFiles time "./" `shouldReturn` []

    it "include updated non markdown file" $ onTemporaryDirectory $ \path -> do
      generateNullFiles ["bar.md", "foo.hs"]
      getUpdatedMdFiles minimal "./" `shouldReturn` ["./bar.md"]


  describe "Web.Kirstie.IO.expandTilde" $ do
    it "starts with tilde" $ do
      dir <- getHomeDirectory
      expandTilde "~/foo" `shouldReturn` (dir </> "foo")

    it "no tilde" $ do
      let path = "path/to/file"
      expandTilde path `shouldReturn` path

    it "include tilde" $ do
      let path = "path/~to/file"
      expandTilde path `shouldReturn` path


  describe "Web.Kirstie.IO.getLastModified" $ do
    it "get last modified time" $ onTemporaryDirectory $ \path -> do
      let time = CTime . truncate . utcTimeToPOSIXSeconds $ minimal
          file = "very_old_file"
      writeFile file ""
      setFileTimes file time time
      getLastModified file `shouldReturn` (minimal :: UTCTime)
    

  describe "Web.Kirstie.IO.parseArgs" $ do
    it "parse arguments" $ do
      let args   = ["foo", "--bar", "--baz", "qux", "-quux", "foobar"
                   , "foobaz"]
          expect = [ ("quux",["foobaz","foobar"]), ("baz",["qux"]), ("bar",[])
                   , ("no label",["foo"]) ]
      parseArgs args `shouldBe` expect


  describe "Web.Kirstie.IO.safeRead" $ do
    it "readable string" $ do
      (safeRead "400" :: Maybe Int) `shouldBe` Just 400

    it "non readable string" $ do
      (safeRead "foo" :: Maybe Int) `shouldBe` Nothing

    it "mixed string" $ do
      (safeRead "4oo" :: Maybe Int) `shouldBe` Just 4


  describe "Web.Kirstie.IO.strErr" $ do
    it "Left Error" $ do
      let e = Left $ userError "foo" :: Either IOError String
      strError e `shouldBe` Left "user error (foo)"

    it "Right" $ do
      let e = Right "foo" :: Either String String
      strError e `shouldBe` Right "foo"


generateNullFiles :: [FilePath] -> IO ()
generateNullFiles = mapM_ (\file -> writeFile file "")
      
onTemporaryDirectory :: (FilePath -> IO a) -> IO a
onTemporaryDirectory act = do
  curDir  <- getCurrentDirectory
  tempDir <- getTemporaryDirectory
  let setup      = (mkdtemp $ tempDir </> testDirectoryName)
                   >>= canonicalizePath
      teardown p = setCurrentDirectory curDir >> removeDirectoryRecursive p
      act' p     = setCurrentDirectory p >> act p
  bracket setup teardown act'
