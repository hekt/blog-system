{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import           Prelude hiding (lookup)
import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad (mzero)
import           Control.Monad.Error
import           Data.Aeson
import           Data.Bson (Document, (=:), at, look, lookup, cast)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromJust)
import           Data.Monoid
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar ( Day (ModifiedJulianDay)
                                    , toModifiedJulianDay )
import           Data.Time.Clock ( UTCTime (UTCTime) )
import           Data.Time.Format (formatTime)
import           System.Locale (defaultTimeLocale)


class ForTemplate a where
    forTemplate :: a -> Value

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
instance Minimal MJDay where
    minimal = 0
instance Minimal (Maybe a) where
    minimal = Nothing


type TextFile = (FilePath, T.Text)
type ByteStringFile = (FilePath, B.ByteString)
type ArticleId = Int
type PathWithId = (FilePath, ArticleId)
type AbsPath = FilePath


data LogLevel = DebugLog | InfoLog | WarnLog | ErrorLog deriving (Eq, Ord)
instance Show LogLevel where
    show DebugLog = "DEBUG"
    show InfoLog  = "INFO "
    show WarnLog  = "WARN "
    show ErrorLog = "ERROR"

data Configure = Configure
    { blogTitle           :: T.Text
    , blogUrl             :: T.Text
    , articleTemplateFile :: String
    , htmlDirectory       :: String
    , databaseName        :: T.Text
    , databaseHost        :: String
    }
instance FromJSON Configure where
    parseJSON (Object v) = Configure 
                           <$> v .: "title"
                           <*> v .: "url"
                           <*> v .: "article_template"
                           <*> v .: "html_directory"
                           <*> v .: "database_name"
                           <*> v .: "database_host"
    parseJSON _          = mzero
instance ToJSON Configure where
    toJSON c = object [ "title"            .= blogTitle c
                      , "url"              .= blogUrl c
                      , "article_template" .= articleTemplateFile c
                      , "html_directory"   .= htmlDirectory c
                      , "database_name"    .= databaseName c
                      , "database_host"    .= databaseHost c
                      ]
instance ForTemplate Configure where
    forTemplate c = object [ "title" .= blogTitle c
                           , "url"   .= blogUrl c
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
    } deriving (Show)
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
instance ForTemplate Article where
    forTemplate a = object [ "title"   .= articleTitle a
                           , "id"      .= articleIdNum a
                           , "pubdate" .= (forTemplate $ articlePubdate a)
                           , "tags"    .= articleTags a
                           , "content" .= articleContent a ]
instance Minimal Article where
    minimal = Article minimal minimal minimal minimal
                         minimal minimal minimal minimal

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

type MJDay = Integer
instance ForTemplate MJDay where
    forTemplate d = object [ "month" .= format "%b"
                           , "day"   .= format "%e"
                           , "year"  .= format "%Y"
                           , "date"  .= format "%F" ]
        where format s = formatTime defaultTimeLocale s $ ModifiedJulianDay d


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
