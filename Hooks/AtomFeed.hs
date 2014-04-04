{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Hooks.AtomFeed (atomFeed) where

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


data XmlElement = XmlElement { tagName    :: Text
                             , attributes :: [(Text, Text)]
                             , childNode  :: XmlContent }
data XmlContent = EmptyNode | TextNode Text | XmlNodes [XmlElement] 


dummyConf :: Configure
dummyConf = Configure "note.hekt.org" "http://note.hekt.org/" "/Users/kaz/Works/blog-kari/tempaltes/article.html" "/Users/kaz/Works/blog-kari/html" "MyBlog" "127.0.0.1"

atomFeed :: Configure -> [Article] -> IO ()
atomFeed conf _ = do
  e <- accessToBlog conf $ rest =<< find (select [] "articles") 
                {limit = 10, sort = ["pubdate" =: -1]}
  case e of 
    Left _         -> return ()
    Right articles -> generateXmlFile conf $ map parseBSON articles

generateXmlFile :: Configure -> [Article] -> IO ()
generateXmlFile conf articles = let doc = generateXmlDoc conf articles
                                    path = htmlDirectory conf </> "atom.xml"
                                in T.writeFile path doc

generateXmlDoc :: Configure -> [Article] -> Text
generateXmlDoc conf articles = T.concat [xmlDeclaration, renderXml feedElem]
    where 
      xmlDeclaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      feedElem = generateFeedElement conf articles

renderXml :: XmlElement -> Text
renderXml (XmlElement tag attrs EmptyNode)     = renderEmptyTag tag attrs
renderXml (XmlElement tag attrs (TextNode t))  = renderTag tag attrs t
renderXml (XmlElement tag attrs (XmlNodes ns)) = renderTag tag attrs $ 
                                                 T.concat $ map renderXml ns

renderTag :: Text -> [(Text, Text)] -> Text -> Text
renderTag tag attrs content = 
    let attrs' = attrs2texts attrs
    in T.concat [ "<", intercalate " " (tag: attrs'), ">"
                , content, "</", tag, ">" ]

renderEmptyTag :: Text -> [(Text, Text)] -> Text
renderEmptyTag tag attrs =
    let attrs' = attrs2texts attrs
    in T.concat ["<", intercalate " " (tag: attrs'), "/>"]

attrs2texts :: [(Text, Text)] -> [Text]
attrs2texts = map (\(a,v) -> T.concat [a, "=", "\"", v, "\""])

generateFeedElement :: Configure -> [Article] -> XmlElement
generateFeedElement conf articles = 
    XmlElement "feed" [("xmlns", "http://www.w3.org/2005/Atom")] $ 
               XmlNodes  (elems ++ map generateEntryElement articles)
        where 
          elems = map (\(t,a,c) -> XmlElement t a c)
                  [ ("title", [], TextNode $ blogUrl conf)
                  , ("updated", [], TextNode $ article2time $ head articles)
                  , ("id", [], TextNode "tag:hekt.org,2005:blog2")
                  , ( "author", []
                    , XmlNodes [XmlElement "name" [] $ TextNode "hekt"])
                  , ( "link", [("rel", "self"), ("href", atomUrl conf)]
                    , EmptyNode)
                  ]

generateEntryElement :: Article -> XmlElement
generateEntryElement article = XmlElement "entry" [] $ 
                               XmlNodes (elems ++ categories)
    where 
      elems = [ XmlElement "id" [] $ TextNode $ article2xmlId article
              , XmlElement "updated" [] $ TextNode $ article2time article
              , XmlElement "link" [("href", article2url article)] EmptyNode
              , XmlElement "title" [("type", "html")] $ 
                      TextNode . cdata $ articleTitle article
              , XmlElement "content" [("type", "html")] $
                      TextNode . cdata $ articleContent article
              ]
      categories = map genCategoryTag $ articleTags article

text2author :: Text -> XmlElement
text2author t = XmlElement "name" [] $ TextNode t

xelem :: Text -> [(Text, Text)] -> XmlContent -> XmlElement
xelem = XmlElement

genCategoryTag :: Text -> XmlElement
genCategoryTag tag = XmlElement "category" [("term", tag)] EmptyNode

cdata :: Text -> Text
cdata d = T.concat ["<![CDATA[", d, "]]>"]

article2xmlId :: Article -> Text
article2xmlId a = T.concat [ "tag:hekt.org,2005:blog2.entry-"
                           , tshow $ articleIdNum a ]

article2time :: Article -> Text
article2time = pack . rfcTime . articleLastModified

article2url :: Article -> Text
article2url a = T.concat ["http://note.hekt.org/", tshow $ articleIdNum a]

atomUrl :: Configure -> Text
atomUrl conf = blogUrl conf `append` "atom"

tshow :: Show a => a -> Text
tshow = pack . show

rfcTime :: UTCTime -> String
rfcTime  = formatTime defaultTimeLocale "%FT%TZ"
