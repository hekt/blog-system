{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Hooks.AmazonAssociates (amazonAssociates) where

import           Data.Text (Text, pack, unpack, breakOn, breakOnEnd)
import qualified Data.Text as T
import           Text.Regex

import Model

amazonAssociates :: Configure -> [Article] -> IO [Article]
amazonAssociates _ = return . map f
    where f a = let t = articleContent a
                in a { articleContent = replaceAllTag t }

replaceAllTag :: Text -> Text
replaceAllTag src = case replaceTag src of
                      Just (a,t,z) -> T.concat [a, t, replaceAllTag z]
                      Nothing      -> src

breakOnTag :: Text -> Maybe (Text, Text, Text)
breakOnTag src = do
  (a, xs)      <- breakOn' "[amazon]" src
  (content, z) <- breakOn' "[/amazon]" xs
  return (a, content, z)

replaceTag :: Text -> Maybe (Text, Text, Text)
replaceTag src = do
  (a, t, z)    <- breakOnTag src
  (name, asin) <- breakOnEnd' ":" t
  return (a, generateTag name asin, z)

breakOn' :: Text -> Text -> Maybe (Text, Text)
breakOn' pat src = let (a, b) = breakOn pat src
                   in if T.null b then Nothing
                      else Just (a, T.drop (T.length pat) b)

breakOnEnd' :: Text -> Text -> Maybe (Text, Text)
breakOnEnd' pat src = let (a, b) = breakOnEnd pat src
                      in if T.null b then Nothing
                         else Just (T.take (T.length a - T.length pat) a, b)
                
generateTag :: Text -> Text -> Text
generateTag name asin = 
    T.concat [ "<dl class=\"amazon-associates\">"
             , "<dt class=\"name\">商品名</dt>"
             , "<dd class=\"name\">"
             , "<a href=\"", asin2url asin, "\">", name, "</a>"
             , "</dd>"
             , "<dt class=\"image\">画像</dt>"
             , "<dd class=\"image\">"
             , "<img src=\"", asin2img asin, "\">"
             , "</dd>"
             , "</dl>" ]

asin2url :: Text -> Text
asin2url asin = T.concat [ "http://www.amazon.co.jp/dp/"
                         , asin, "&tag=hekt-22" ]

asin2img :: Text -> Text
asin2img asin = T.concat [ "http://images-jp.amazon.com/images/P/"
                         , asin, ".09.LZZZZZZZ.jpg" ]
