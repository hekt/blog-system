{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.Kirstie.Hooks.TagArchives (tagArchives) where

import           Control.Exception
import           Control.Monad
import           Data.Data (Data, Typeable)
import qualified Data.Map as M
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import           Database.MongoDB
import           Text.Hastache
import           Text.Hastache.Context
import           System.FilePath ((</>))

import Web.Kirstie.Model
import Web.Kirstie.IO
import Web.Kirstie.DB

data TagArchiveValues = TagArchiveValues
    { config_data :: Configure
    , tag_name :: Text
    , articles :: [TArticle]
    } deriving (Data, Typeable)

putLog' level = putLog level . (++) "TagArchives: "

handler :: IOException -> IO ()
handler e = putLog' ErrorLog $ show e

tagArchives :: Configure -> [Article] -> IO ()
tagArchives conf articles = handle handler $ do
  let tags = getTagsFromArticles articles
      tempPath = templateDirectory conf </> "tag-archives.html"
  pipe     <- runIOE $ connect (host $ databaseHost conf)
  template <- decodeTemplateFile tempPath
  forM_ tags $ \tag -> do
    e <- getDocsByTag conf pipe tag
    case e of
      Left  msg  -> putLog' ErrorLog msg
      Right docs -> generateTagArchive template conf tag $ map parseBSON docs
  close pipe

getTagsFromArticles :: [Article] -> [Text]
getTagsFromArticles = nubOrd . concatMap articleTags

generateTagArchive :: Text -> Configure -> Text -> [Article] -> IO ()
generateTagArchive template conf tag atcs = do
  let dir = htmlDirectory conf </> "tags"
      name = (filenameEncode $ T.unpack tag) ++ ".html"
      tatcs = TagArchiveValues conf tag $ 
              map articleToTArticle atcs
  result   <- hastacheStr defaultConfig template $ mkGenericContext tatcs
  TL.writeFile (dir </> name) result
  putLog' InfoLog $ unwords ["Successfully generated", dir </> name]

getDocsByTag :: Configure -> Pipe -> Text -> IO (Either String [Document])
getDocsByTag conf pipe tag = 
    access' pipe master (databaseName conf) $ rest =<<
    find (select ["tags" =: tag] "articles")
             { sort = ["pubdate" =: -1] 
             , project = [ "title" =: 1, "id" =: 1
                         , "tags" =: 1, "pubdate" =: 1 ] }


-- http://stackoverflow.com/questions/3098391/unique-elements-in-a-haskell-list/3100764#3100764
nubOrd :: Ord a => [a] -> [a] 
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []
