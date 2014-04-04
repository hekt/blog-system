{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Hooks.XmlSitemap (xmlSitemap) where

import           Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock (UTCTime)
import           Database.MongoDB
import           System.FilePath ((</>))

import Model
import IO
import DB
import XML


dummyConf :: Configure
dummyConf = Configure "note.hekt.org" "http://note.hekt.org/" "/Users/kaz/Works/blog-kari/tempaltes/article.html" "/Users/kaz/Works/blog-kari/html" "MyBlog" "127.0.0.1"


xmlSitemap :: Configure -> [Article] -> IO ()
xmlSitemap conf _ = do
  e <- accessToBlog conf $ rest =<< find (select [] "articles")
  case e of
    Left  msg      -> putLog ErrorLog $ "XmlSitemap: " ++ show msg
    Right articles -> generateSitemapFile conf $ map parseBSON articles

generateSitemapFile :: Configure -> [Article] -> IO ()
generateSitemapFile conf articles = 
    let doc = generateSitemap conf articles
        path = htmlDirectory conf </> "sitemap.xml"
    in T.writeFile path $ renderXml doc

generateSitemap :: Configure -> [Article] -> XmlDocument
generateSitemap conf articles = XmlDocument "1.0" "UTF-8" $
                                [elem (indexPage: map f articles)]
    where 
      indexPage = urlElem (blogUrl conf) (rfcTime $ blogLastMod articles)
                  "weekly" "0.5"
      f a = urlElem (article2url a) (rfcTime $ articleLastModified a) 
            "never" "1.0"
      elem ns = XmlElement "urlset" 
                [("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")]
                (createXmlNodes ns)

blogLastMod :: [Article] -> UTCTime
blogLastMod articles = maximum $ map articleLastModified articles

urlElem :: Text -> Text -> Text -> Text -> XmlElement
urlElem loc mod freq pri = XmlElement "url" [] $ createXmlNodes
                           [ simpleElem "loc" loc
                           , simpleElem "lastmod" mod
                           , simpleElem "changefreq" freq
                           , simpleElem "priority" pri ]

article2url :: Article -> Text
article2url a = T.concat ["http://note.hekt.org/", tshow $ articleIdNum a]

tshow :: Show a => a -> Text
tshow = pack . show

