{-# LANGUAGE OverloadedStrings #-}

module XML
    ( XmlDocument (XmlDocument)
    , XmlElement (XmlElement)
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
data XmlElement = XmlElement { tagName    :: Text
                             , attributes :: [(Text, Text)]
                             , childNode  :: XmlContent }
data XmlContent = EmptyNode | TextNode Text | XmlNodes [XmlElement] 


renderXml :: XmlDocument -> Text
renderXml doc = let ver = attr2text "version" $ xmlVersion doc
                    enc = attr2text "encoding" $ xmlEncoding doc
                in T.concat $ [ "<?xml ", ver, " ", enc, "?>" ]
                       ++ map renderXml' (xmlElements doc)

renderXml' :: XmlElement -> Text
renderXml' (XmlElement tag attrs EmptyNode)     = renderEmptyTag tag attrs
renderXml' (XmlElement tag attrs (TextNode t))  = renderTag tag attrs t
renderXml' (XmlElement tag attrs (XmlNodes ns)) = renderTag tag attrs $ 
                                                  T.concat $ map renderXml' ns

renderTag :: Text -> [(Text, Text)] -> Text -> Text
renderTag tag attrs content = 
    let attrs' = attrs2texts attrs
    in T.concat [ "<", intercalate " " (tag: attrs'), ">"
                , content, "</", tag, ">" ]

simpleElem :: Text -> Text -> XmlElement
simpleElem tag content = XmlElement tag [] $ createTextNode content

attr2text :: Text -> Text -> Text
attr2text a v = T.concat [a, "=", "\"", v, "\""]

attrs2texts :: [(Text, Text)] -> [Text]
attrs2texts = map (uncurry attr2text)

renderEmptyTag :: Text -> [(Text, Text)] -> Text
renderEmptyTag tag attrs =
    let attrs' = attrs2texts attrs
    in T.concat ["<", intercalate " " (tag: attrs'), "/>"]

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
