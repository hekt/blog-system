{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Hooks.AtomFeed (atomFeed) where

import qualified Data.Map as M
import           Data.Text (Text, pack, intercalate, append)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (UTCTime)
import           Data.Time.Format (formatTime)
import           Database.MongoDB
import           System.FilePath ((</>))
import           System.Locale (defaultTimeLocale)

import Model
import DB
import XML


atomFeed :: Configure -> [Article] -> IO ()
atomFeed conf _ = do
  e <- accessToBlog conf $ rest =<< find (select [] "articles") 
                {limit = 10, sort = ["pubdate" =: -1]}
  case e of 
    Left _         -> return ()
    Right articles -> generateXmlFile conf $ map parseBSON articles

generateXmlFile :: Configure -> [Article] -> IO ()
generateXmlFile conf articles = 
    let doc = generateXmlDoc conf articles
        path = case "atom_feed_file" `M.lookup` optConfs conf of
                 Just p  -> p
                 Nothing -> htmlDirectory conf </> "atom.xml"
    in T.writeFile path doc

generateXmlDoc :: Configure -> [Article] -> Text
generateXmlDoc conf articles = renderXml $ XmlDocument "1.0" "UTF-8"
                               [generateFeedElement conf articles]

generateFeedElement :: Configure -> [Article] -> XmlElement
generateFeedElement conf articles = 
    XmlElement "feed" [("xmlns", "http://www.w3.org/2005/Atom")] $ 
               createXmlNodes (elems ++ map generateEntryElement articles)
        where 
          elems = map (\(t,a,c) -> XmlElement t a c)
                  [ ("title", [], createTextNode $ blogUrl conf)
                  , ("updated", [], createTextNode $ 
                              article2time $ head articles)
                  , ("id", [], createTextNode "tag:hekt.org,2005:blog2")
                  , ( "author", []
                    , createXmlNodes 
                      [XmlElement "name" [] $ createTextNode "hekt"])
                  , ( "link", [("rel", "self"), ("href", atomUrl conf)]
                    , createEmptyNode)
                  ]

generateEntryElement :: Article -> XmlElement
generateEntryElement article = XmlElement "entry" [] $ 
                               createXmlNodes (elems ++ categories)
    where 
      elems = [ XmlElement "id" [] $ createTextNode $ article2xmlId article
              , XmlElement "updated" [] $ createTextNode $ article2time article
              , XmlElement "link" [("href", article2url article)] 
                           createEmptyNode
              , XmlElement "title" [("type", "html")] $ 
                      createTextNode . cdata $ articleTitle article
              , XmlElement "content" [("type", "html")] $
                      createTextNode . cdata $ articleContent article
              ]
      categories = map genCategoryTag $ articleTags article

text2author :: Text -> XmlElement
text2author t = XmlElement "name" [] $ createTextNode t

genCategoryTag :: Text -> XmlElement
genCategoryTag tag = XmlElement "category" [("term", tag)] createEmptyNode

article2xmlId :: Article -> Text
article2xmlId a = T.concat [ "tag:hekt.org,2005:blog2.entry-"
                           , tshow $ articleIdNum a ]

article2time :: Article -> Text
article2time = rfcTime . articleLastModified

article2url :: Article -> Text
article2url a = T.concat ["http://note.hekt.org/", tshow $ articleIdNum a]

atomUrl :: Configure -> Text
atomUrl conf = blogUrl conf `append` "atom"

tshow :: Show a => a -> Text
tshow = pack . show
