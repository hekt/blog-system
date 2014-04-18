{-# LANGUAGE OverloadedStrings #-}

module Web.Kirstie.Util where

import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar (Day (ModifiedJulianDay))
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime)
import           Network.HTTP (urlEncode)
import           System.FilePath
import           System.Locale (defaultTimeLocale)


import Web.Kirstie.Model


-- | List of markdown file extensions
-- 
markdownExtensions :: [String]
markdownExtensions = [ ".markdown", ".mdown", ".mkdn", ".md", ".mkd", ".mdwn"
                     , ".mdtxt", ".mdtext", ".text" ]

-- | List of html file extensions
-- 
htmlExtensions :: [String]
htmlExtensions = [ ".html", ".htm" ]


-- | check whether a filename starts with '.'
-- 
-- >>> isInvisibleFile "foo/.bar"
-- True
isInvisibleFile :: FilePath -> Bool
isInvisibleFile []   = False
isInvisibleFile file = safeHead (takeFileName file) == Just '.'

-- | not `isInvisibleFile`
-- 
-- >>> isVisibleFile "foo/.bar"
-- False
isVisibleFile :: FilePath -> Bool
isVisibleFile = not . isInvisibleFile

-- | check whether a filename ends with any of `markdownExtensions`
-- 
-- >>> isMarkdownFile "path/to/file.md"
-- True
isMarkdownFile :: FilePath -> Bool
isMarkdownFile file = takeExtension file `elem` markdownExtensions

-- | check whether a filename ends with any of `htmlExtensions`
-- 
-- >>> isHtmlfile "path/to/file.html"
-- True
isHtmlFile :: FilePath -> Bool
isHtmlFile file = takeExtension file `elem` htmlExtensions

-- | convert non-string error to string in Left
-- 
-- >>> strError . Left $ userError "foo"
-- Left "user error (foo)"
-- 
-- >>> strError $ Right 123
-- Right 123
strError :: Show a => Either a b -> Either String b
strError (Left err) = Left $ show err
strError (Right x)  = Right x

-- | non-partial `read`
-- 
-- >>> safeRead "400" :: Maybe Int
-- Just 400
-- 
-- >>> safeRead "4oo" :: Maybe Int
-- Just 4
-- 
-- >>> saferead "foo" :: Maybe int
-- Nothing
safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . listToMaybe . reads

-- | non-partial `head`
-- 
-- >>> safeHead [1,2,3]
-- Just 1
-- 
-- >>> safeHead []
-- Nothing
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | non-partial `maximum`
-- 
-- >>> safeMaximum [1,2,3]
-- Just 3
-- 
-- >>> safeMaximum []
-- Nothing
safeMaximum [] = Nothing
safeMaximum xs = maximum xs


-- | print UTCTime with RFC3339
--
-- >>> rfcTime $ UTCTime (fromGregorian 2014 4 17) 0
-- "2014-04-17T00:00:00Z"
rfcTime :: UTCTime -> String
rfcTime = formatTime defaultTimeLocale "%FT%TZ"

-- | Escape reserved characters
-- 
-- >>> filenameEncode "foo\NULbar\0baz/qux"
-- "foo-bar-baz-qux"
filenameEncode :: String -> String
filenameEncode = let cs = ['\NUL', '/']
                 in map (\c -> if c `elem` cs then '-' else c)


-- | Convert `Article` to `TArticle` (for embed to hastache template)
-- 
articleToTArticle :: Article -> TArticle
articleToTArticle a = TArticle
                     { title   = TL.fromStrict $ articleTitle a
                     , aid     = TL.pack . show $ articleIdNum a
                     , pubdate = TPubdate { date  = pdFormat "%F"
                                          , year  = pdFormat "%Y"
                                          , month = pdFormat "%b"
                                          , day   = pdFormat "%-e" }
                     , tags    = map tagObj $ articleTags a
                     , content = TL.fromStrict $ articleContent a
                     , lastmod = lmFormat "%FT%TZ" }
    where
      format s t = TL.pack $ formatTime defaultTimeLocale s t
      lmFormat s = format s $ articleLastModified a
      pdFormat s = format s $ ModifiedJulianDay $ articlePubdate a
      tagObj t = TTag { tag         = TL.fromStrict t
                      , encoded_tag = TL.pack . urlEncode $ T.unpack t }


-- | Parse command line arguments
-- 
parseArgs :: [String] -> [(String, [String])]
parseArgs args = f args [("no label", [])] where
    f [] ys = ys
    f (x:xs) ys@((k,vs):kvs)
        | isLabel x = f xs ((stripHyphen x, []  ) : ys)
        | otherwise = f xs ((k            , x:vs) : kvs)

isLabel :: String -> Bool
isLabel ('-':_) = True
isLabel _       = False

stripHyphen :: String -> String
stripHyphen ('-':s) = stripHyphen s
stripHyphen s       = s
