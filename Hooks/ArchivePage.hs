{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hooks.ArchivePage (archivePage) where

import           Control.Monad.Error
import           Data.Data (Data, Typeable)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
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

ioeLogger' = ioeLoggerWithLabel "ArchivePage: "
putLog' level = putLog level . (++) "ArchivePage: "

archivePage :: Configure -> [Article] -> IO ()
archivePage conf _ = ioeLogger' . runErrorT $ do
  let tempPath = maybe (articleTemplateFile conf) id $
                 "archive_template_file" `M.lookup` optConfs conf
      savePath = maybe (htmlDirectory conf </> "archive.html") id $
                 "archive_page_file" `M.lookup` optConfs conf
      pjs = ["title" =: 1, "id" =: 1, "tags" =: 1, "pubdate" =: 1]
  template <- liftIO $ decodeTemplateFile tempPath
  docs     <- ErrorT $ accessToBlog' conf $
              rest =<< find (select [] "articles") 
              {sort = ["pubdate" =: -1], project = pjs}
  body     <- liftIO $ generateArchive conf template $ map parseBSON docs
  liftIO $ do
    TL.writeFile savePath body
    putLog' InfoLog $ unwords ["Successfully generated", savePath]
    
generateArchive :: Configure -> Text -> [MaybeArticle] -> IO TL.Text
generateArchive conf template atcs =
  hastacheStr defaultConfig template . mkGenericContext $
  ArchivePageValues conf $ map mArticleToTArticle atcs
