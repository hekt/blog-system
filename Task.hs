{-# LANGUAGE OverloadedStrings #-}

module Task 
    ( runUpdate
    , runRebuild
    , runForceRebuild
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Data.Either (partitionEithers)
import           Data.Time.Clock (UTCTime)
import           Text.Pandoc (Template)
import           System.Environment (getArgs)
import           System.Directory
import           System.FilePath

import Setting
import Model
import IO
import DB
import Hook

-- update

runUpdate :: Configure -> FilePath -> ErrorT String IO ()
runUpdate conf dir = do
  lastRun    <- liftIO $ getLastRunTime conf
  files      <- liftIO $ getUpdatedMdFiles lastRun dir
  liftIO $ putLog InfoLog $ unwords [ "Found", show $ length files
                                    , "new/updated file(s)" ]
  knownFiles <- liftIO $ getAllArticleSourceAndIds conf
  latestId   <- liftIO $ getLatestIdNumber conf
  ErrorT . runWithFiles conf $ addIdToFiles knownFiles files latestId
  liftIO $ updateLastRunTime conf

addIdToFiles :: [PathWithId] -> [FilePath] -> ArticleId -> [PathWithId]
addIdToFiles pairs files = fst . runState (mapM addId files)
    where addId file = state $ case lookup file pairs of
                                 Just m  -> \n -> ((file, m  ), n  )
                                 Nothing -> \n -> ((file, n+1), n+1)


-- rebuild

runRebuild :: Configure -> ErrorT String IO ()
runRebuild conf = do
  liftIO $ removeHtmlFiles conf
  ErrorT $ runWithDB conf
  liftIO $ updateLastRunTime conf

runForceRebuild :: Configure -> ErrorT String IO ()
runForceRebuild conf = do
  liftIO $ removeHtmlFiles conf
  pairs  <- liftIO $ getAllArticleSourceAndIds conf
  liftIO $ resetDB conf
  ErrorT $ runWithFiles conf pairs
  liftIO $ updateLastRunTime conf


-- shared functions

runWithFiles :: Configure -> [PathWithId] -> IO (Either String ())
runWithFiles conf pairs = do
  articles <- mapM (uncurry getArticleFromFile) pairs
  result   <- runWith conf articles
  case result of
    Left  msg       -> return $ Left msg
    Right articles' -> do saveArticlesToDB conf articles'
                          doAfterSaveHooks conf articles'
                          return $ Right ()

runWithDB :: Configure -> IO (Either String ())
runWithDB conf = do
  articles <- fmap (map Right) $ getAllArticlesFromDB conf
  result   <- runWith conf articles
  case result of
    Left  msg       -> return $ Left msg
    Right articles' -> do doAfterSaveHooks conf articles'
                          return $ Right ()

runWith :: Configure -> [Either String Article] -> IO (Either String [Article])
runWith conf eitherArticles = runErrorT $ do
  let (errors, articles) = partitionEithers eitherArticles
  template  <- liftIO $ decodeTemplateFile $ articleTemplateFile conf
  articles' <- liftIO $ doBeforeSaveHooks conf articles
  liftIO $ forM_ articles' $ generateHtmlFileWithLog template conf
  liftIO $ forM_ errors $ putLog ErrorLog
  return articles'

doBeforeSaveHooks :: Configure -> [Article] -> IO [Article]
doBeforeSaveHooks conf articles = 
    foldM (\a h -> h conf a) articles beforeSaveHooks

doAfterSaveHooks :: Configure -> [Article] -> IO ()
doAfterSaveHooks _    []       = return ()
doAfterSaveHooks conf articles = mapM_ (\h -> h conf articles) afterSaveHooks
