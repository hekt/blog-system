{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Monad
import           System.Directory (canonicalizePath)

import Model
import IO

runConfig :: FilePath -> IO ()
runConfig path = do
  let labels = [ "blog_url: "
               , "template_directory: "
               , "source_directory: "
               , "html_directory: "
               , "database_name: "
               , "database_host: "
               ]
  path'  <- expandTilde path
  values <- forM labels $ \l -> putStr l >> getLine >>= expandTilde
  writeFile path' $ unlines $ zipWith (++) labels values
  path'' <- canonicalizePath path'
  putLog InfoLog $ unwords [ "Successfully generated config file at"
                           , path'' ]
