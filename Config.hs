{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Monad

import IO

runConfig :: FilePath -> IO ()
runConfig path = do
  let labels = [ "template_directory: "
               , "source_directory: "
               , "html_directory: "
               , "database_name: "
               , "database_host: "
               ]
  values <- forM labels $ \l -> putStrLn l >> getLine >>= expandTilde
  writeFile path $ unlines $ zipWith (++) labels values
