{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Web.Kirstie.Hooks.RelatedPosts (relatedPosts) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Data.List (sort, sortBy)
import           Data.Ord (comparing)
import           Data.Text (Text)
import           Database.MongoDB hiding (sort, lookup, count)

import Web.Kirstie.Model
import Web.Kirstie.IO
import Web.Kirstie.DB

ioeLogger' = ioeLoggerWithLabel "RelatedPosts: "
putLog' level = putLog level . (++) "RelatedPosts: "

relatedPosts :: Configure -> [Article] -> IO ()
relatedPosts conf articles = ioeLogger' . runErrorT $ do
  let pairs = map article2pair articles
  pipe   <- liftIO . runIOE $ connect (host $ databaseHost conf)
  scores <- getScoreList conf pipe
  result <- forM pairs $ \(aid, tags) -> do
    docs <- findByTags conf pipe tags
    return $ (aid, take' 6 aid $ calcRelated scores tags docs)
  liftIO $ do
    saveToDB conf pipe result
    putLog' InfoLog $ "Successfully updated"

take' :: Int -> ArticleId -> [(ArticleId, Float)] -> [ArticleId]
take' n aid = take n . filter (/= aid) . 
              map fst . reverse . sortBy (comparing snd)

findByTags :: Configure -> Pipe -> [Text] -> StrIOE [Document]
findByTags conf pipe tags = ErrorT $ do
  e <- access pipe master (databaseName conf) $
       rest =<< find (select (buildSelectorByTags tags) "articles")
  return $ strError e

calcRelated :: [(Text, Float)] -> [Text] -> [Document] -> [(ArticleId, Float)]
calcRelated scores tags = calcScores scores tags . map doc2pair

-- io
getScoreList :: Configure -> Pipe -> StrIOE [(Text, Float)]
getScoreList conf pipe = ErrorT $ do
  e <- access pipe master (databaseName conf) $
       rest =<< find (select [] "articles") {project = ["tags" =: 1]}
  return $ strError $ fmap (generateScoreList . concatMap doc2tags) e

buildSelectorByTags :: [Text] -> Document
buildSelectorByTags tags = ["$or" =: map (\t -> ["tags" =: t]) tags]

saveToDB :: Configure -> Pipe -> [(ArticleId, [ArticleId])] -> IO ()
saveToDB conf pipe pairs = mapM_ repsert' pairs
    where access' = access pipe master (databaseName conf)
          repsert' (aid, aids) = access' $ repsert
                                 (select ["id" =: aid] "related_posts")
                                 ["id" =: aid, "relateds" =: aids]

-- supplements
article2pair :: Article -> (ArticleId, [Text])
article2pair a = (articleIdNum a, articleTags a)

doc2pair :: Document -> (ArticleId, [Text])
doc2pair d = ("id" `at` d, "tags" `at` d)

doc2tags :: Document -> [Text]
doc2tags d = "tags" `at` d

sndMap :: (b -> c) -> [(a, b)] -> [(a, c)]
sndMap f = map (\(x,y) -> (x, f y))

count :: (Eq a, Ord a) => [a] -> [(a, Int)]
count xs = let (y:ys) = sort xs in f 1 y ys
    where
      f c x []     = [(x, c)]
      f c x (y:ys) | x == y    = f (c+1) x ys
                   | otherwise = (x, c): f 1 y ys

getScore :: [(Text, Float)] -> Text -> Float
getScore scores = maybe 0 id . flip lookup scores

calcScores :: [(Text, Float)] -> [Text] -> [(ArticleId, [Text])] 
           -> [(ArticleId, Float)]
calcScores dic xs = sndMap (calcScore dic xs)

calcScore :: [(Text, Float)] -> [Text] -> [Text] -> Float
calcScore dic xs ys = sum . map (getScore dic) $ filter (`elem` ys) xs

generateScoreList :: [Text] -> [(Text, Float)]
generateScoreList tags = map calc pairs
    where pairs = count tags
          total = sum $ map snd pairs
          calc (t, n) = (t, fromIntegral total / fromIntegral n)
