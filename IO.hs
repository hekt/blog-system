{-# LANGUAGE OverloadedStrings #-}

module IO
    -- logging
    ( ioeLogger
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
    --- time
    , pureTime
    --- utils
    , safeRead
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (listToMaybe)
import           Data.Text (Text, strip, splitOn, unpack)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format (formatTime)
import           Data.Yaml
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Pandoc
import           Text.Pandoc.UTF8 (toString)
import           Text.Regex.Posix
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Locale (defaultTimeLocale)
import           System.Posix.Files

import Setting
import Model


-- logging

ioeLogger :: IO (Either String ()) -> IO ()
ioeLogger act = handle ioeLogger' $ do
  result <- act
  case result of
    Left msg -> putLog ErrorLog msg
    _        -> return ()

ioeLogger' :: IOException -> IO ()
ioeLogger' = putLog ErrorLog . show

putLog :: LogLevel -> String -> IO ()
putLog level msg = do
  time <- getLogTime
  let log = unwords [time, show level, msg]
  putStrLn log
  when (level >= ErrorLog) $ hPutStrLn stderr log

-- read file

getConf :: IO (Either String Configure)
getConf = getAppUserDataDirectory appDirName 
          >>= return . (</> confFileName) >>= decodeYamlFile

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
  return Article { articleTitle      = yamlTitle yaml
                 , articleIdNum      = aid
                 , articlePubdate    = date
                 , articleTags       = csv2texts $ yamlTags yaml
                 , articleContent    = TL.toStrict $ renderHtml html
                 , articleSourceFile = file
                 , articleIsImported = False
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

decodeTemplateFile :: FilePath -> IO (Either String Template)
decodeTemplateFile file = T.readFile file >>= return . compileTemplate


-- generate file

generateHtmlFile :: Template -> Configure -> Article -> IO ()
generateHtmlFile template conf article = do
  let dir = htmlDirectory conf
      name = (show $ articleIdNum article) ++ ".html"
      obj = object [ "blog" .= forTemplate conf
                   , "article" .= forTemplate article ]
      body = renderTemplate template obj
  BL.writeFile (dir </> name) body

generateHtmlFileWithLog :: Template -> Configure -> Article -> IO ()
generateHtmlFileWithLog template conf article = do
  generateHtmlFile template conf article
  putLog InfoLog $ unwords [ "Successfully generated"
                           , getHtmlFilePath conf article
                           , "from"
                           , articleSourceFile article ]


-- remove file

removeHtmlFiles :: Configure -> IO ()
removeHtmlFiles conf = getHtmlFiles (htmlDirectory conf) >>= mapM_ removeFile
  

-- file info

getUpdatedMdFiles :: UTCTime -> AbsPath -> IO [FilePath]
getUpdatedMdFiles lastRun dir = do
  files   <- getMdFiles dir
  filterM (isUpdated lastRun) files

getMdFiles :: AbsPath -> IO [FilePath]
getMdFiles dir = getDirectoryContents dir >>=
                 return . map (dir </>) . filter isMarkdownFile >>=
                 mapM (canonicalizePath)

getHtmlFiles :: AbsPath -> IO [FilePath]
getHtmlFiles dir = getDirectoryContents dir >>=
                   return . map (dir </>) . filter isHtmlFile >>=
                   mapM (canonicalizePath)

isMarkdownFile :: FilePath -> Bool
isMarkdownFile file = takeExtension file `elem` markdownExtensions

isHtmlFile :: FilePath -> Bool
isHtmlFile file = takeExtension file `elem` htmlExtensions 

isUpdated :: UTCTime -> FilePath -> IO Bool
isUpdated time file = getLastModified file >>= return . (> time)

getLastModified :: FilePath -> IO UTCTime
getLastModified file = 
    getFileStatus file >>=
    return . posixSecondsToUTCTime . realToFrac . modificationTime


-- supplements

getLogTime :: IO String
getLogTime = getCurrentTime >>= 
             return . formatTime defaultTimeLocale "[%F %X]"

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . listToMaybe . reads

csv2texts :: Text -> [Text]
csv2texts = map strip . splitOn ","

getDayFromText :: Text -> Either String MJDay
getDayFromText = let l = Left "cannot parse pubdate"
                     r = Right . toModifiedJulianDay
                 in maybe l r . safeRead . unpack

splitYamlAndGfm :: B.ByteString -> Either String (B.ByteString, B.ByteString)
splitYamlAndGfm body = 
    let reStr = "---\n+(.*)\n+---\n+(.*)" :: B.ByteString
        regex = makeRegexOpts compExtended defaultExecOpt reStr
    in case match regex body of 
         ((_:y:g:_):_) -> Right (y, g)
         _             -> Left "yaml and gfm parse error"

gfmStr2html :: String -> Html
gfmStr2html = let mdDef = def {readerExtensions = githubMarkdownExtensions}
              in writeHtml def . readMarkdown mdDef

getHtmlFilePath :: Configure -> Article -> FilePath
getHtmlFilePath conf article = htmlDirectory conf </> 
                               show (articleIdNum article) ++ ".html"

pureTime :: UTCTime
pureTime = UTCTime (ModifiedJulianDay 0) 0
