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
import           Data.Bson (Document, (=:), lookup, Label, Val)
import           Data.Data (Data, Typeable)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar (Day (ModifiedJulianDay))
import           Data.Time.Clock (UTCTime (UTCTime), DiffTime)


class ToBSON a where
    toBSON :: a -> Document

class FromBSON a where
    parseBSON :: Document -> a

class Minimal a where
    minimal :: a


instance Minimal Int where
    minimal = 0
instance Minimal Integer where
    minimal = 0
instance Minimal Bool where
    minimal = False
instance Minimal T.Text where
    minimal = ""
instance Minimal TL.Text where
    minimal = ""
instance Minimal Day where
    minimal = ModifiedJulianDay minimal
instance Minimal DiffTime where
    minimal = 0
instance Minimal UTCTime where
    minimal = UTCTime minimal minimal
instance Minimal ([] a) where
    minimal = []
instance Minimal (Maybe a) where
    minimal = Nothing


type ArticleId  = Int


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
    } deriving (Show, Data, Typeable, Eq)

instance FromJSON Configure where
    parseJSON (Object v) = Configure 
                           <$> v .: "blog_url"
                           <*> v .: "template_directory"
                           <*> v .: "source_directory"
                           <*> v .: "html_directory"
                           <*> v .: "database_name"
                           <*> v .: "database_host"
    parseJSON _          = mzero


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
    , articlePubdate      :: Integer
    , articleTags         :: [T.Text]
    , articleContent      :: T.Text
    , articleSourceFile   :: String
    , articleLastModified :: UTCTime
    , articleIsImported   :: Bool
    } deriving (Show, Eq, Ord, Data, Typeable)

instance FromBSON Article where
    parseBSON doc = Article
                    { articleTitle        = f "title"
                    , articleIdNum        = f "id"
                    , articlePubdate      = f "pubdate"
                    , articleTags         = f "tags"
                    , articleContent      = f "content"
                    , articleSourceFile   = f "source_file"
                    , articleLastModified = f "last_modified"
                    , articleIsImported   = f "imported"
                    }
        where f :: (Minimal a, Val a) => Label -> a
              f = maybe minimal id . flip lookup doc

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


-- for templating

data TArticle = TArticle 
    { title   :: TL.Text
    , aid     :: TL.Text
    , pubdate :: TPubdate
    , tags    :: [TTag]
    , content :: TL.Text
    , lastmod :: TL.Text
    } deriving (Show, Eq, Ord, Data, Typeable)

data TPubdate = TPubdate
    { date  :: TL.Text
    , year  :: TL.Text
    , month :: TL.Text
    , day   :: TL.Text
    } deriving (Show, Eq, Ord, Data, Typeable)

data TTag = TTag
    { tag         :: TL.Text
    , encoded_tag :: TL.Text
    } deriving (Show, Eq, Ord, Data, Typeable)
