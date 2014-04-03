{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module DB 
    ( saveArticlesToDB
    , getLatestIdNumber
    , getKnownList
    , getAllArticleSourceAndIds
    , getAllArticlesFromDB
    , updateLastRunTime
    , getLastRunTime
    , resetDB
    , accessToBlog
    ) where

import           Control.Monad
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Database.MongoDB

import Model
import IO

accessToBlog :: Configure -> Action IO a -> IO (Either Failure a)
accessToBlog conf act = do
  let dbName = databaseName conf
      dbHost = databaseHost conf
  pipe <- runIOE $ connect (host dbHost)
  e    <- access pipe master dbName act
  close pipe
  return e

saveArticlesToDB :: Configure -> [Article] -> IO ()
saveArticlesToDB conf articles = do
  let dbName = databaseName conf
      dbHost = databaseHost conf
  pipe <- runIOE $ connect (host dbHost)
  forM_ articles $ \article -> do
    let selector = ["id" =: articleIdNum article]
    access pipe master dbName $ 
           repsert (select selector "articles") $ toBSON article
      
                        
getLatestIdNumber :: Configure -> IO Int
getLatestIdNumber conf = do
  e <- accessToBlog conf $ findOne (select [] "articles") 
       {sort = ["id" =: -1], project = ["id" =: 1]}
  case e of
    Right (Just d) -> return $ "id" `at` d
    _              -> return 0

getKnownList :: Configure -> IO [FilePath]
getKnownList conf = do
  e <- accessToBlog conf $ 
       rest =<< find (select [] "articles") {project = ["source_file" =: 1]}
  return $ either (\_ -> []) (map ("source_file" `at`)) e

getAllArticleSourceAndIds :: Configure -> IO [(FilePath, Int)]
getAllArticleSourceAndIds conf = do
  e <- accessToBlog conf $ rest =<< find (select [] "articles") 
       {project = ["source_file" =: 1, "id" =: 1]}
  return $ either (\_ -> []) (map f) e
    where f d = ("source_file" `at` d, "id" `at` d)

getAllArticlesFromDB :: Configure -> IO [Article]
getAllArticlesFromDB conf = do
  e <- accessToBlog conf $ rest =<< find (select [] "articles")
  return $ either (\_ -> []) (map parseBSON) e

updateLastRunTime :: Configure -> IO ()
updateLastRunTime conf = do
  t <- getCurrentTime
  void . accessToBlog conf $ repsert (select [] "last_run") ["time" =: t]

getLastRunTime :: Configure -> IO (UTCTime)
getLastRunTime conf = do
  e <- accessToBlog conf $ 
       findOne (select [] "last_run") {project = ["time" =: 1]}
  case e of
    Right (Just d) -> return $ "time" `at` d
    _              -> return pureTime

resetDB :: Configure -> IO ()
resetDB conf = do
  let dbName = databaseName conf
      dbHost = databaseHost conf
  pipe <- runIOE $ connect (host dbHost)
  access pipe master dbName $ delete (select [] "last_run")
  access pipe master dbName $ delete (select [] "articles")
  close pipe
