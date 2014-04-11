{-# LANGUAGE OverloadedStrings #-}

module IO
    -- logging
    ( ioeLogger
    , ioeLoggerWithLabel
    , putLog
    -- read file
    , getConf
    , getArticleFromFile
    , decodeTemplateFile
    -- generate file
    , generateHtmlFileWithLog
    -- remove file
    , removeHtmlFiles
    -- directory
    , getUpdatedMdFiles
    , expandTilde
    -- time
    , pureTime
    -- args
    , parseArgs
    -- utils
    , safeRead
    , strError
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (listToMaybe)
import           Data.Text (Text, strip, splitOn, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format (formatTime)
import           Data.Yaml
import           Network.HTTP (urlEncode)
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hastache
import           Text.Hastache.Context
import           Text.Pandoc
import           Text.Pandoc.UTF8 (toString)
import           Text.Regex.Posix
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Locale (defaultTimeLocale)
import           System.Posix.Files

import Model


-- PURE

markdownExtensions :: [String]
markdownExtensions = [ ".markdown", ".mdown", ".mkdn", ".md", ".mkd", ".mdwn"
                     , ".mdtxt", ".mdtext", ".text" ]

htmlExtensions :: [String]
htmlExtensions = [ ".html", ".htm" ]

-- | parse command line arguments
-- 
-- >>> parseArgs ["foo", "--bar", "--baz", "qux", "-quux", "foobar", "foobaz"]
-- [("quux",["foobaz","foobar"]),("baz",["qux"]),("bar",[]),("no label",["foo"])]
parseArgs :: [String] -> [(String, [String])]
parseArgs args = f args [("no label", [])] where
    f []     kvs            = kvs
    f (x:xs) ys@((k,vs):kvs)
        | isLabel x = f xs ((stripHyphen x, []  ) : ys)
        | otherwise = f xs ((k            , x:vs) : kvs)

-- | is string starts with '-'
-- 
-- >>> isLabel "foo"
-- False
-- 
-- >>> isLabel "-bar"
-- True
isLabel :: String -> Bool
isLabel ('-':_) = True
isLabel _       = False

-- | remove hyphens in beginning of string
-- 
-- >>> stripHyphen "--foo"
-- "foo"
-- 
-- >>> stripHyphen "bar"
-- "bar"
stripHyphen :: String -> String
stripHyphen ('-':s) = stripHyphen s
stripHyphen s       = s

-- | is filepath starts with '.'
-- 
-- >>> isInvisibleFile ".foo"
-- True
-- 
-- >>> isInvisibleFile "bar"
-- False
isInvisibleFile :: FilePath -> Bool
isInvisibleFile []   = False
isInvisibleFile path = case takeFileName path of
                         ('.':_) -> True
                         _       -> False
isVisibleFile :: FilePath -> Bool
isVisibleFile = not . isInvisibleFile

-- | is filepath ends with any of `markdownExtensions`
-- 
-- >>> isMarkdownFile "foo.md"
-- True
-- 
-- >>> isMarkdownFile "bar.txt"
-- False
isMarkdownFile :: FilePath -> Bool
isMarkdownFile file = takeExtension file `elem` markdownExtensions

-- | isfilepath ends with any of `htmlExtensions`
-- 
-- >>> isHtmlFile "foo.html"
-- True
-- 
-- >>> isHtmlFile "bar.css"
-- False
isHtmlFile :: FilePath -> Bool
isHtmlFile file = takeExtension file `elem` htmlExtensions 

-- | convert non-string error to string in Left
-- 
-- >>> strError . Left $ userError "foo"
-- Left "user error (foo)"
-- 
-- >>> strError . Right $ "foo"
-- Right "foo"
strError :: Show a => Either a b -> Either String b
strError (Left err) = Left $ show err
strError (Right r)  = Right r

-- | read with Maybe
-- 
-- >>> safeRead "400" :: Maybe Int
-- Just 400
-- 
-- >>> safeRead "bar" :: Maybe Int
-- Nothing
safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . listToMaybe . reads

-- | convert comma separated values to list of values
-- 
-- >>> csv2texts "foo,bar,baz"
-- ["foo","bar","baz"]
csv2texts :: Text -> [Text]
csv2texts = map strip . splitOn ","

-- | parse "YYYY-MM-DD" then convert to ModifiedJulianDay
-- 
-- >>> getDayFromText "2000-01-01"
-- Right 51544
getDayFromText :: Text -> Either String MJDay
getDayFromText = let l = Left "cannot parse pubdate"
                     r = Right . toModifiedJulianDay
                 in maybe l r . safeRead . unpack

-- | split "---" separated bytestring
-- 
-- >>> splitYamlAndGfm "---\nfoo\n---\nbar"
-- Right ("foo","bar")
splitYamlAndGfm :: B.ByteString -> Either String (B.ByteString, B.ByteString)
splitYamlAndGfm body = 
    let reStr = "---\n+(.*)\n+---\n+(.*)" :: B.ByteString
        regex = makeRegexOpts compExtended defaultExecOpt reStr
    in case match regex body of 
         ((_:y:g:_):_) -> Right (y, g)
         _             -> Left "yaml and gfm parse error"

-- | convert github fravored markdown to html
-- 
-- >>> renderHtml $ gfmStr2html "# foo"
-- "<h1 id=\"foo\">foo</h1>"
gfmStr2html :: String -> Html
gfmStr2html = let mdDef = def {readerExtensions = githubMarkdownExtensions}
              in writeHtml def . readMarkdown mdDef

-- | get html file path fron Configure and Article
-- 
-- >>> :{
--  let
--    u        = undefined
--    conf     = Configure u u u "dir" u u u
--    articles = minimal {articleIdNum = 100}
--  in getHtmlFilePath conf articles 
-- :}
-- "dir/archives/100.html"
getHtmlFilePath :: Configure -> Article -> FilePath
getHtmlFilePath conf article = htmlDirectory conf </> "archives" </>
                               show (articleIdNum article) ++ ".html"

-- | default time
-- 
-- >>> pureTime
-- 1858-11-17 00:00:00 UTC
pureTime :: UTCTime
pureTime = UTCTime (ModifiedJulianDay 0) 0


-- IO

ioeLogger :: IO (Either String ()) -> IO ()
ioeLogger act = handle ioeHandler $ do
  result <- act
  case result of
    Left msg -> putLog ErrorLog msg
    _        -> return ()

ioeHandler :: IOException -> IO ()
ioeHandler = putLog ErrorLog . show

ioeLoggerWithLabel :: String -> IO (Either String ()) -> IO ()
ioeLoggerWithLabel label act = handle (ioeHandlerWithLabel label) $ do
  result <- act
  case result of 
    Left msg -> putLog ErrorLog $ label ++ msg
    _        -> return ()

ioeHandlerWithLabel :: String -> IOException -> IO ()
ioeHandlerWithLabel label e = putLog ErrorLog $ label ++ show e

putLog :: LogLevel -> String -> IO ()
putLog level msg = do
  time <- getLogTime
  let log = unwords [time, show level, msg]
  putStrLn log
  when (level >= ErrorLog) $ hPutStrLn stderr log

getConf :: FilePath -> IO (Either String Configure)
getConf path = decodeYamlFile path

getArticleFromFile :: AbsPath -> ArticleId -> IO (Either String Article)
getArticleFromFile file aid = do
  eitherArticle <- getArticleFromFile' file aid
  case eitherArticle of
    Left msg -> return . Left $ concat [file, ": ", msg]
    r        -> return r

getArticleFromFile' :: AbsPath -> ArticleId -> IO (Either String Article)
getArticleFromFile' file aid = runErrorT $ do
  (yaml, html) <- ErrorT $ decodeYamlAndGfmFile file
  date         <- ErrorT . return $ getDayFromText $ yamlPubdate yaml
  time         <- liftIO $ getLastModified file
  return Article { articleTitle        = yamlTitle yaml
                 , articleIdNum        = aid
                 , articlePubdate      = date
                 , articleTags         = csv2texts $ yamlTags yaml
                 , articleContent      = TL.toStrict $ renderHtml html
                 , articleSourceFile   = file
                 , articleLastModified = time
                 , articleIsImported   = False
                 }

decodeYamlFile :: FromJSON a => FilePath -> IO (Either String a)
decodeYamlFile file = B.readFile file >>= return . f . decodeEither'
    where f :: Either ParseException a -> Either String a
          f (Left (InvalidYaml (Just e))) = Left . withFile $ yamlProblem e
          f (Left e)                      = Left . withFile $ show e
          f (Right a)                     = Right a
          withFile msg = concat ["yaml parse error: ", msg]

decodeYamlAndGfmFile :: FromJSON a => FilePath -> IO (Either String (a, Html))
decodeYamlAndGfmFile file = runErrorT $ do
  body        <- liftIO $ B.readFile file
  (yaml, gfm) <- ErrorT . return $ splitYamlAndGfm body
  obj         <- ErrorT . return $ decodeEither yaml
  let html = gfmStr2html $ toString gfm
  return (obj, html)

decodeTemplateFile :: FilePath -> IO Text
decodeTemplateFile file = readFile file >>= return . encodeStr

generateHtmlFileWithLog :: Text -> Configure -> Article -> IO ()
generateHtmlFileWithLog template conf article = do
  generateHtmlFile template conf article
  putLog InfoLog $ unwords [ "Successfully generated"
                           , getHtmlFilePath conf article
                           , "from"
                           , articleSourceFile article ]

generateHtmlFile :: Text -> Configure -> Article -> IO ()
generateHtmlFile template conf article = do
  let dir = htmlDirectory conf </> "archives"
      name = (show $ articleIdNum article) ++ ".html"
      tempData = articleToTArticle article
  res <- hastacheStr defaultConfig template $ mkGenericContext tempData
  TL.writeFile (dir </> name) res

removeHtmlFiles :: Configure -> IO ()
removeHtmlFiles conf = getHtmlFiles (htmlDirectory conf) >>= mapM_ removeFile

expandTilde :: FilePath -> IO FilePath
expandTilde ('~':'/':path) = getHomeDirectory >>= return . (</> path)
expandTilde path           = return path

getUpdatedMdFiles :: UTCTime -> AbsPath -> IO [FilePath]
getUpdatedMdFiles lastRun dir = do
  files   <- getMdFiles dir
  filterM (isUpdated lastRun) files

getMdFiles :: AbsPath -> IO [FilePath]
getMdFiles dir =
    let f = map (dir </>) . filter (\x -> isVisibleFile x && isMarkdownFile x)
    in fmap f (getDirectoryContents dir) >>= mapM canonicalizePath

getHtmlFiles :: AbsPath -> IO [FilePath]
getHtmlFiles dir = 
    let f = map (dir </>) . filter (\x -> isVisibleFile x && isHtmlFile x)
    in fmap f (getDirectoryContents dir) >>= mapM canonicalizePath


isUpdated :: UTCTime -> FilePath -> IO Bool
isUpdated time file = getLastModified file >>= return . (> time)

getLastModified :: FilePath -> IO UTCTime
getLastModified file = 
    getFileStatus file >>=
    return . posixSecondsToUTCTime . realToFrac . modificationTime

getLogTime :: IO String
getLogTime = getCurrentTime >>= 
             return . formatTime defaultTimeLocale "[%F %X]"

