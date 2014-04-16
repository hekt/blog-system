{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Kirstie.Server (runServer, serverApp) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson ((.=), toJSON, object, Value)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import           Database.MongoDB hiding (lookup, Value)
import           Web.Scotty
import           System.FilePath ((</>), takeExtension)

import Web.Kirstie.Model
import Web.Kirstie.IO
import Web.Kirstie.DB hiding (access')

runServer :: Configure -> Int -> IO ()
runServer conf port = scotty port $ serverApp conf

serverApp :: Configure -> ScottyM ()
serverApp conf = do
  -- related posts
  get "/api/related-posts" $ do
    ps <- params
    relateds <- liftIO $ getRelatedPosts ps
    case relateds of
      Just articles -> json $ articles
      Nothing       -> json $ object []
    setHeader "content-type" "application/json; charset=UTF-8"
    setHeader "Access-Control-Allow-Origin" "http://localhost:38888"

  -- articles
  get "/archives/:aid" $ do
    (aid :: Int) <- param "aid"
    file $ htmlDirectory conf </> "archives" </> show aid ++ ".html"
    setHeader "content-type" "text/html; charset=UTF-8"

  -- archive page
  get "/archives/" $ do
    file $ htmlDirectory conf </> "archive.html"
    setHeader "content-type" "text/html; charset=UTF-8"

  -- tag archives
  get "/tags/:tag" $ do
    tag <- param "tag"
    file $ htmlDirectory conf </> "tags" </> tag ++ ".html"
    setHeader "content-type" "text/html; charset=UTF-8"

  -- static files
  get (regex staticFileRegex) $ do
    path <- param "1"
    file $ htmlDirectory conf </> path
    setHeader "content-type" $ mimeType path

staticFileRegex :: String
staticFileRegex = 
    "^/((css|js|images|fonts)/.*\\.(css|js|jpe?g|png|gif|bmp|woff|ttf))$"

mimeType :: FilePath -> TL.Text
mimeType file = case takeExtension file of
                  -- images
                  ".jpg"  -> "image/jpeg"
                  ".jpeg" -> "image/jpeg"
                  ".png"  -> "image/png"
                  ".gif"  -> "image/gif"
                  ".bmp"  -> "image/bmp"
                  -- styles
                  ".css"  -> "text/css; charset=UTF-8"
                  -- scripts
                  ".js"   -> "text/javascript; charset=UTF-8"
                  -- html
                  ".html" -> "text/html; charset=UTF-8"
                  -- fonts
                  ".woff" -> "application/x-font-woff"
                  ".ttf"  -> "application/x-font-ttf"

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
