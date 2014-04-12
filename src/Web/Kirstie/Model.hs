{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.Kirstie.Model where

import           Prelude hiding (lookup)
import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad (mzero)
import           Control.Monad.Error
import           Data.Aeson
import           Data.Bson (Document, (=:), at, look, lookup, cast)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Data (Data, Typeable)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Monoid
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal as TI
import           Data.Time.Calendar ( Day (ModifiedJulianDay)
                                    , toModifiedJulianDay )
import           Data.Time.Clock ( UTCTime (UTCTime) )
import           Data.Time.Format (formatTime)
import           Network.HTTP (urlEncode)
import           Text.Hastache
import           System.Locale (defaultTimeLocale)


class ToBSON a where
    toBSON :: a -> Document

class FromBSON a where
    parseBSON :: Document -> a

class Minimal a where
    minimal :: a


instance Minimal T.Text where
    minimal = ""
instance Minimal Int where
    minimal = 0
instance Minimal ([] a) where
    minimal = []
instance Minimal Bool where
    minimal = False
instance Minimal UTCTime where
    minimal = UTCTime (ModifiedJulianDay 0) 0
instance Minimal Integer where
    minimal = 0
instance Minimal (Maybe a) where
    minimal = Nothing


type TextFile = (FilePath, T.Text)
type ByteStringFile = (FilePath, B.ByteString)
type ArticleId = Int
type PathWithId = (FilePath, ArticleId)
type AbsPath = FilePath
type MJDay = Integer
type StrIOE = ErrorT String IO


data LogLevel = DebugLog | InfoLog | WarnLog | ErrorLog deriving (Eq, Ord)
instance Show LogLevel where
    show DebugLog = "DEBUG"
    show InfoLog  = "INFO "
    show WarnLog  = "WARN "
    show ErrorLog = "ERROR"

data Configure = Configure 
    { blogUrl           :: String
    , templateDirectory :: String
    , sourceDirectory   :: String
    , htmlDirectory     :: String
    , databaseName      :: T.Text
    , databaseHost      :: String
    } deriving (Show, Data, Typeable)
instance FromJSON Configure where
    parseJSON (Object v) = Configure 
                           <$> v .: "blog_url"
                           <*> v .: "template_directory"
                           <*> v .: "source_directory"
                           <*> v .: "html_directory"
                           <*> v .: "database_name"
                           <*> v .: "database_host"
    parseJSON _          = mzero
instance ToJSON Configure where
    toJSON c = object [ "blog_url"           .= blogUrl c
                      , "template_directory" .= templateDirectory c
                      , "source_directory"   .= sourceDirectory c
                      , "html_directory"     .= htmlDirectory c
                      , "database_name"      .= databaseName c
                      , "database_host"      .= databaseHost c
                      ]

data ArticleYaml = ArticleYaml
    { yamlTitle   :: T.Text
    , yamlTags    :: T.Text
    , yamlPubdate :: T.Text
    }
instance FromJSON ArticleYaml where
    parseJSON (Object v) = ArticleYaml
                           <$> v .: "title"
                           <*> v .: "tags"
                           <*> v .: "pubdate"
    parseJSON _          = mzero

data Article = Article 
    { articleTitle        :: T.Text
    , articleIdNum        :: ArticleId
    , articlePubdate      :: MJDay
    , articleTags         :: [T.Text]
    , articleContent      :: T.Text
    , articleSourceFile   :: String
    , articleLastModified :: UTCTime
    , articleIsImported   :: Bool
    } deriving (Show, Data, Typeable)
instance FromJSON Article where
    parseJSON (Object v) = Article
                           <$> v .: "title"
                           <*> v .: "id"
                           <*> v .: "pubdate"
                           <*> v .: "tags"
                           <*> v .: "content"
                           <*> v .: "source_file"
                           <*> v .: "last_modified"
                           <*> v .: "imported"
    parseJSON _          = mzero
instance ToJSON Article where
    toJSON a = object [ "title"         .= articleTitle a
                      , "id"            .= articleIdNum a
                      , "pubdate"       .= articlePubdate a
                      , "tags"          .= articleTags a
                      , "content"       .= articleContent a 
                      , "source_file"   .= articleSourceFile a
                      , "last_modified" .= articleLastModified a
                      , "imported"      .= articleIsImported a ]
instance FromBSON Article where
    parseBSON doc = Article
                    { articleTitle = "title" `at` doc
                    , articleIdNum = "id" `at` doc
                    , articlePubdate = "pubdate" `at` doc
                    , articleTags    = "tags" `at` doc
                    , articleContent = "content" `at` doc
                    , articleSourceFile = "source_file" `at` doc
                    , articleLastModified = "last_modified" `at` doc
                    , articleIsImported   = "imported" `at` doc
                    }
instance ToBSON Article where
    toBSON a = [ "title"         =: articleTitle a
               , "id"            =: articleIdNum a
               , "pubdate"       =: articlePubdate a
               , "tags"          =: articleTags a
               , "content"       =: articleContent a 
               , "source_file"   =: articleSourceFile a
               , "last_modified" =: articleLastModified a
               , "imported"      =: articleIsImported a ]
instance Minimal Article where
    minimal = Article minimal minimal minimal minimal
                         minimal minimal minimal minimal

data TArticle = TArticle 
    { title :: T.Text
    , aid :: ArticleId
    , pubdate :: TPubdate
    , tags :: [TTag]
    , content :: T.Text
    , lastmod :: T.Text
    } deriving (Data, Typeable)

data TPubdate = TPubdate
    { date :: T.Text
    , year :: T.Text
    , month :: T.Text
    , day :: T.Text
    } deriving (Data, Typeable)
data TTag = TTag
    { tag :: T.Text
    , encoded_tag :: T.Text
    } deriving (Data, Typeable)

data MaybeArticle = MaybeArticle 
    { mArticleTitle :: Maybe T.Text
    , mArticleIdNum :: Maybe Int
    , mArticlePubdate :: Maybe MJDay
    , mArticleTags    :: Maybe [T.Text]
    , mArticleContent :: Maybe T.Text
    , mArticleSourceFile :: Maybe String
    , mArticleLastModified :: Maybe UTCTime
    , mArticleIsImported   :: Maybe Bool
    }
instance FromBSON MaybeArticle where
    parseBSON doc = MaybeArticle 
                    { mArticleTitle = f "title"
                    , mArticleIdNum = f "id"
                    , mArticlePubdate = f "pubdate"
                    , mArticleTags    = f "tags"
                    , mArticleContent = f "content"
                    , mArticleSourceFile = f "source_file"
                    , mArticleLastModified = f "last_modified"
                    , mArticleIsImported   = f "imported"
                    } 
        where f l = l `look` doc >>= cast
instance Minimal MaybeArticle where
    minimal = MaybeArticle minimal minimal minimal minimal
                              minimal minimal minimal minimal

articleToTArticle :: Article -> TArticle
articleToTArticle article = 
    TArticle
    { title = articleTitle article
    , aid    = articleIdNum article
    , pubdate = pd
    , tags    = ts
    , content = articleContent article
    , lastmod = T.pack . rfcTime $ articleLastModified article
    }
    where pd = let f s = T.pack . formatTime defaultTimeLocale s $ 
                         ModifiedJulianDay $ articlePubdate article
               in TPubdate { date = f "%F"
                           , year = f "%Y"
                           , month = f "%b"
                           , day   = f "%-e" }
          f t = TTag t . T.pack . urlEncode . filenameEncode $ T.unpack t
          ts = map f $ articleTags article

mArticleToArticle :: MaybeArticle -> Article
mArticleToArticle ma = Article
                       { articleTitle = f mArticleTitle
                       , articleIdNum = f mArticleIdNum
                       , articlePubdate = f mArticlePubdate
                       , articleTags    = f mArticleTags 
                       , articleContent = f mArticleContent
                       , articleSourceFile = f mArticleSourceFile
                       , articleLastModified = f mArticleLastModified
                       , articleIsImported   = f mArticleIsImported }
    where f g = maybe minimal id $ g ma

mArticleToTArticle :: MaybeArticle -> TArticle
mArticleToTArticle = articleToTArticle . mArticleToArticle

filenameEncode :: String -> String
filenameEncode str = map f str
    where f c = if c `elem` ['/', '\0'] then '-' else c

rfcTime :: UTCTime -> String
rfcTime = formatTime defaultTimeLocale "%FT%TZ"
