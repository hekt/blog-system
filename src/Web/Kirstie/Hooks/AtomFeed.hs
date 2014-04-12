{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.Kirstie.Hooks.AtomFeed (atomFeed) where

import           Control.Monad.Error
import           Data.Data (Data, Typeable)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import           Data.Time.Clock (UTCTime)
import           Database.MongoDB
import           Text.Hastache
import           Text.Hastache.Context
import           System.FilePath ((</>))

import Web.Kirstie.Model
import Web.Kirstie.IO
import Web.Kirstie.DB

data BlogData     = BlogData { url     :: String
                             , lastmod :: String
                             } deriving (Data, Typeable)
data TemplateData = TemplateData { blog     :: BlogData
                                 , articles :: [TArticle]
                                 } deriving (Data, Typeable)

ioeLogger' = ioeLoggerWithLabel "AtomFeed: "
putLog' level = putLog level . (++) "AtomFeed: "

atomFeed :: Configure -> [Article] -> IO ()
atomFeed conf _ = ioeLogger' . runErrorT $ do
  let path = htmlDirectory conf </> "atom.xml"
  docs <- ErrorT $ getDocuments conf
  res  <- liftIO $ buildContent conf $ map parseBSON docs
  liftIO $ do
    TL.writeFile path res
    putLog' InfoLog $ unwords ["Successfully generated", path]

getDocuments :: Configure -> IO (Either String [Document])
getDocuments conf = accessToBlog' conf $ rest =<< find (select [] "articles")
                    { limit = 10 }

buildContent :: Configure -> [Article] -> IO TL.Text
buildContent conf articles = do
  let blogData = BlogData (blogUrl conf) $ rfcTime $ blogLastMod articles
      tempData = TemplateData blogData $ map articleToTArticle articles
  template <- decodeTemplateFile $ templateDirectory conf </> "atom-feed.xml"
  hastacheStr defaultConfig template $ mkGenericContext tempData

blogLastMod :: [Article] -> UTCTime
blogLastMod articles = maximum $ map articleLastModified articles
