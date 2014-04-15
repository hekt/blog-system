{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Exception
import           Control.Monad
import           Database.MongoDB
import qualified Data.Text as T
import           System.Directory
import           System.FilePath
import           System.Posix.Temp
import           System.Posix.Files

import Web.Kirstie.Model

onTestSpace :: (Configure -> Pipe -> FilePath -> IO a) -> IO a
onTestSpace act = withDatabase $
                  \pipe -> onTemporaryDirectory $
                  \path -> act testConfig pipe path

withDatabase :: (Pipe -> IO a) -> IO a
withDatabase act = do
  let setup         = runIOE $ connect $ host $ databaseHost testConfig
      teardown p = do
        close p
        pipe <- runIOE $ connect $ host $ databaseHost testConfig
        e    <- access pipe master (databaseName testConfig) $ allCollections
        whenRight e $ \cols ->
          forM_ cols $ \col -> access pipe master (databaseName testConfig) $
                               delete (select [] col)
        close pipe
  bracket setup teardown act

isRight :: Either a b -> Bool
isRight = either (\_ -> False) (\_ -> True)

whenRight :: Monad m => Either a b -> (b -> m ()) -> m ()
whenRight e f = either (\_ -> return ()) f e

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

