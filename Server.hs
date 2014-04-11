{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Server (runServer) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson ((.=), toJSON, object, Value)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import           Database.MongoDB hiding (lookup, Value)
import           Web.Scotty

import Model
import IO
import DB hiding (access')

runServer :: Configure -> Int -> IO ()
runServer conf port = scotty port $ do
  get "/api/related-posts" $ do
    ps <- params
    relateds <- liftIO $ getRelatedPosts ps
    case relateds of
      Just articles -> json $ articles
      Nothing       -> json $ object []
    setHeader "content-type" "application/json; charset=UTF-8"
    setHeader "Access-Control-Allow-Origin" "http://localhost:8888"

getRelatedPosts :: [Param] -> IO (Maybe [Value])
getRelatedPosts ps = runMaybeT $ do
  aid        <- MaybeT . return $ params2aid ps
  pipe       <- liftIO . runIOE $ connect (host "localhost")
  maybeIds   <- MaybeT $ access' pipe master "MyBlog" $
                findOne (select ["id" =: aid] "related_posts")
  relatedIds <- MaybeT . return $ maybeIds
  let aids = "relateds" `at` relatedIds
  docs       <- MaybeT $ access' pipe master "MyBlog" $
                rest =<< find (select (buildSelectorByIds aids) "articles")
                                 { project = ["title" =: 1, "id" =: 1]}
  return $ map doc2json docs

params2aid :: [Param] -> Maybe ArticleId
params2aid ps = "aid" `lookup` ps >>= safeRead . TL.unpack

access' :: Pipe -> AccessMode -> Database -> Action IO a -> IO (Maybe a)
access' pipe mode db act = do
  e <- access pipe mode db act
  return $ either (\_ -> Nothing) Just e

doc2json :: Document -> Value
doc2json d = object [ "aid"    .= ("id" `at` d :: Int)
                    , "title" .= ("title" `at` d :: Text)
                    ]

buildSelectorByIds :: [Int] -> Document
buildSelectorByIds aids = ["$or" =: map (\a -> ["id" =: a]) aids]
