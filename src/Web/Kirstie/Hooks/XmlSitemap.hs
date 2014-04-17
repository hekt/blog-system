{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.Kirstie.Hooks.XmlSitemap (xmlSitemap) where

import           Control.Monad.Error
import           Data.Data (Data, Typeable)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime)
import           Text.Hastache
import           Text.Hastache.Context
import           Database.MongoDB
import           System.FilePath ((</>))
import           System.Locale (defaultTimeLocale)

import Web.Kirstie.Model
import Web.Kirstie.IO
import Web.Kirstie.DB

data BlogData     = BlogData { url     :: String
                             , lastmod :: String
                             } deriving (Data, Typeable)
data TemplateData = TemplateData { blog     :: BlogData
                                 , articles :: [TArticle]
                                 } deriving (Data, Typeable)

ioeLogger' = ioeLoggerWithLabel "XmlSitemap: "
putLog' level = putLog level . (++) "XmlSitemap: "

xmlSitemap :: Configure -> [Article] -> IO ()
xmlSitemap conf _ = ioeLogger' . runErrorT $ do
  let path = htmlDirectory conf </> "sitemap.xml"
  docs     <- ErrorT $ getDocuments conf
  res      <- liftIO $ buildContent conf $ map parseBSON docs
  liftIO $ do
    TL.writeFile path res
    putLog' InfoLog $ unwords ["Successfully generated", path]

blogLastMod :: [Article] -> UTCTime
blogLastMod []       = minimal
blogLastMod articles = maximum $ map articleLastModified articles

getDocuments :: Configure -> IO (Either String [Document])
getDocuments conf = accessToBlog' conf $ rest =<< find (select [] "articles")
                    { project = ["id" =: 1, "last_modified" =: 1] }

buildContent :: Configure -> [Article] -> IO TL.Text
buildContent conf articles = do
  template <- decodeTemplateFile $ templateDirectory conf </> "xml-sitemap.xml"
  let blogData = BlogData (blogUrl conf) $ rfcTime $ blogLastMod articles
      tempData = TemplateData blogData $ map articleToTArticle articles
  hastacheStr defaultConfig template $ mkGenericContext tempData
