{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Hooks.XmlSitemap (xmlSitemap) where

import qualified Data.Map as M
import           Data.Maybe (catMaybes)
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


xmlSitemap :: Configure -> [Article] -> IO ()
xmlSitemap conf _ = do
  e <- accessToBlog conf $ rest =<< find (select [] "articles")
       { project = ["id" =: 1, "last_modified" =: 1] }
  case e of
    Left  msg  -> putLog ErrorLog $ "XmlSitemap: " ++ show msg
    Right docs -> generateSitemapFile conf $ map parseBSON docs

generateSitemapFile :: Configure -> [MaybeArticle] -> IO ()
generateSitemapFile conf mas = 
    let doc = generateSitemap conf mas
        path = case "xml_sitemap_file" `M.lookup` optConfs conf of
                 Just p  -> p
                 Nothing -> htmlDirectory conf </> "sitemap.xml"
    in T.writeFile path $ renderXml doc

generateSitemap :: Configure -> [MaybeArticle] -> XmlDocument
generateSitemap conf mas = XmlDocument "1.0" "UTF-8" $
                           elem $ indexPage : catMaybes (map f mas)
    where 
      indexPage = urlElem (blogUrl conf) (rfcTime $ blogLastMod mas)
                  "weekly" "0.5"
      f ma = do
        url <- fmap (id2url conf) $ mArticleIdNum ma
        mod <- fmap rfcTime $ mArticleLastModified ma
        return $ urlElem url mod "never" "1.0"
      elem ns = [ XmlElement "urlset" 
                  [("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")]
                  (createXmlNodes ns) ]

blogLastMod :: [MaybeArticle] -> UTCTime
blogLastMod articles = maximum . catMaybes $ map mArticleLastModified articles

urlElem :: Text -> Text -> Text -> Text -> XmlElement
urlElem loc mod freq pri = XmlElement "url" [] $ createXmlNodes
                           [ simpleElem "loc" loc
                           , simpleElem "lastmod" mod
                           , simpleElem "changefreq" freq
                           , simpleElem "priority" pri ]

id2url :: Configure -> Int -> Text
id2url conf a = T.concat [blogUrl conf, tshow a]

tshow :: Show a => a -> Text
tshow = pack . show

