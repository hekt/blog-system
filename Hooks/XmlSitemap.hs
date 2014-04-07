{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Hooks.XmlSitemap (xmlSitemap) where

import           Control.Monad.Error
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

ioeLogger' = ioeLoggerWithLabel "XmlSitemap: "
putLog' level = putLog level . (++) "XmlSitemap: "

xmlSitemap :: Configure -> [Article] -> IO ()
xmlSitemap conf _ = ioeLogger' . runErrorT $ do
  docs <- ErrorT $ accessToBlog' conf $ 
          rest =<< find (select [] "articles")
          { project = ["id" =: 1, "last_modified" =: 1] }
  let xml = generateSitemap conf $ map parseBSON docs
      path = maybe (htmlDirectory conf </> "sitemap.xml") id $
             "xml_sitemap_file" `M.lookup` optConfs conf
  liftIO $ do
    T.writeFile path $ renderXml xml
    putLog' InfoLog $ unwords ["Successfully generated", path]

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

