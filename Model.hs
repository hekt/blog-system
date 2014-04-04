{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad (mzero)
import           Control.Monad.Error
import           Data.Aeson
import           Data.Bson (Document, (=:), at, look)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar ( Day (ModifiedJulianDay)
                                    , toModifiedJulianDay )
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime)
import           System.Locale (defaultTimeLocale)


class ForTemplate a where
    forTemplate :: a -> Value

class ToBSON a where
    toBSON :: a -> Document

class FromBSON a where
    parseBSON :: Document -> a


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
                    ("title" `at` doc)
                    ("id" `at` doc)
                    ("pubdate" `at` doc)
                    ("tags" `at` doc)
                    ("content" `at` doc)
                    ("source_file" `at` doc)
                    ("last_modified" `at` doc)
                    ("imported" `at` doc)
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

data ArticleDate = ArticleDate
    { articleDate  :: T.Text
    , articleMonth :: T.Text
    , articleDay   :: T.Text
    , articleYear  :: T.Text
    }

type MJDay = Integer
instance ForTemplate MJDay where
    forTemplate d = object [ "month" .= format "%b"
                           , "day"   .= format "%e"
                           , "year"  .= format "%Y"
                           , "date"  .= format "%F" ]
        where format s = formatTime defaultTimeLocale s $ ModifiedJulianDay d
