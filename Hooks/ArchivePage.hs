{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hooks.ArchivePage (archivePage) where

import           Data.Data (Data, Typeable)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text.Lazy.IO as TL
import           Database.MongoDB
import           Text.Hastache
import           Text.Hastache.Context
import           System.FilePath ((</>))

import Model
import DB
import IO

data ArchivePageValues = ArchivePageValues 
    { config_data :: Configure
    , articles    :: [TArticle]
    } deriving (Data, Typeable)

archivePage :: Configure -> [Article] -> IO ()
archivePage conf _ = do
  let tempPath = case "archive_template_file" `M.lookup` optConfs conf of
                   Just p  -> p
                   Nothing -> articleTemplateFile conf
  template <- decodeTemplateFile tempPath
  result   <- accessToBlog conf $ rest =<< find (select [] "articles") 
               { sort = ["pubdate" =: -1]
               , project = [ "title" =: 1, "id" =: 1
                           , "tags" =: 1, "pubdate" =: 1 ] }
  case result of
    Left msg   -> putLog ErrorLog $ "ArchivePage: " ++ show msg
    Right docs -> generateArchiveFile template conf $ map parseBSON docs
    

generateArchiveFile :: Text -> Configure -> [MaybeArticle] -> IO ()
generateArchiveFile template conf atcs = do
  let filePath = case "archive_page_file" `M.lookup` optConfs conf of
                   Just p  -> p
                   Nothing -> htmlDirectory conf </> "archive.html"
      tatcs = ArchivePageValues conf $
              map (articleToTArticle . mArticleToArticle) atcs
  result <- hastacheStr defaultConfig template $ mkGenericContext tatcs
  TL.writeFile filePath result
  putLog InfoLog $ unwords ["ArchivePage: Successfully generated", filePath]
