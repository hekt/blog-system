{-# LANGUAGE OverloadedStrings #-}

module XML
    ( XmlDocument (XmlDocument)
    , XmlElement (XmlElement)
    , TagName
    , Attribute
    , renderXml
    , renderXml'
    , cdata
    , rfcTime
    , simpleElem
    , createTextNode
    , createEmptyNode
    , createXmlNodes
    ) where


import           Data.Text (Text, append, intercalate)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)


data XmlDocument = XmlDocument { xmlVersion  :: Text
                               , xmlEncoding :: Text
                               , xmlElements :: [XmlElement] }
data XmlElement = XmlElement { tagName    :: TagName
                             , attributes :: [Attribute]
                             , childNode  :: XmlContent }
data XmlContent = EmptyNode | TextNode Text | XmlNodes [XmlElement]
type TagName   = Text
type Attribute = (Text, Text)


renderXml :: XmlDocument -> Text
renderXml doc = let ver = attr2text ("version", xmlVersion doc)
                    enc = attr2text ("encoding", xmlEncoding doc)
                in T.concat $ [ "<?xml ", ver, " ", enc, "?>" ]
                       ++ map renderXml' (xmlElements doc)

renderXml' :: XmlElement -> Text
renderXml' (XmlElement tag attrs content) =
    case content of
      EmptyNode   -> renderEmptyTag tag attrs
      TextNode t  -> renderTag tag attrs t
      XmlNodes ns -> renderTag tag attrs $ T.concat $ map renderXml' ns

renderTag :: TagName -> [Attribute] -> Text -> Text
renderTag tag attrs content = 
    let attrs' = map attr2text attrs
    in T.concat [ "<", intercalate " " (tag: attrs'), ">"
                , content, "</", tag, ">" ]

renderEmptyTag :: TagName -> [Attribute] -> Text
renderEmptyTag tag attrs =
    let attrs' = map attr2text attrs
    in T.concat ["<", intercalate " " (tag: attrs'), "/>"]

simpleElem :: TagName -> Text -> XmlElement
simpleElem tag content = XmlElement tag [] $ createTextNode content

attr2text :: Attribute -> Text
attr2text (a, v) = T.concat [a, "=", "\"", v, "\""]

cdata :: Text -> Text
cdata d = T.concat ["<![CDATA[", d, "]]>"]

rfcTime :: UTCTime -> Text
rfcTime = T.pack . formatTime defaultTimeLocale "%FT%TZ"

createEmptyNode :: XmlContent
createEmptyNode = EmptyNode

createTextNode :: Text -> XmlContent
createTextNode = TextNode

createXmlNodes :: [XmlElement] -> XmlContent
createXmlNodes = XmlNodes
