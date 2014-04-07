{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hooks.TagArchives (tagArchives) where

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

import Model
import IO
import DB

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
      tempPath = maybe (articleTemplateFile conf) id $ 
                 "tag_archives_template_file" `M.lookup` optConfs conf
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

generateTagArchive :: Text -> Configure -> Text -> [MaybeArticle] -> IO ()
generateTagArchive template conf tag atcs = do
  let dir = case "tag_archives_directory" `M.lookup` optConfs conf of
              Just d  -> d
              Nothing -> htmlDirectory conf </> "tags"
      name = (filenameEncode $ T.unpack tag) ++ ".html"
      tatcs = TagArchiveValues conf tag $ 
              map (articleToTArticle . mArticleToArticle) atcs
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

filenameEncode :: String -> String
filenameEncode str = map f str
    where f c = if c `elem` ['/', '\0'] then '-' else c


-- http://stackoverflow.com/questions/3098391/unique-elements-in-a-haskell-list/3100764#3100764
nubOrd :: Ord a => [a] -> [a] 
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []
