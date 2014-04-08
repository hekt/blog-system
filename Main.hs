{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Error
import           System.Environment (getArgs)

import Model (Configure)
import IO (parseArgs, getConfWithPath, ioeLogger)
import Task (runUpdate, runRebuild, runForceRebuild)

main :: IO ()
main = ioeLogger . runErrorT $ do
  args <- liftIO $ fmap parseArgs getArgs
  path <- ErrorT . return $ lookupValue "conf" args
  conf <- ErrorT $ getConfWithPath path
  mode <- ErrorT . return $ lookupValue "no label" args
  
  case mode of
    "update"  -> runUpdate' conf args
    "rebuild" -> runRebuild' conf args
    _         -> ErrorT . return $ Left "invalid arguments"

lookupValue :: String -> [(String, [String])] -> Either String String
lookupValue k args = case k `lookup` args of
                       Just (v:_)  -> Right v
                       _           -> Left "invalid arguments"

runUpdate' :: Configure -> [(String, [String])] -> ErrorT String IO ()
runUpdate' conf args = do
  dir <- ErrorT . return $ lookupValue "dir" args
  runUpdate conf dir

runRebuild' :: Configure -> [(String, [String])] -> ErrorT String IO ()
runRebuild' conf args = case "force" `lookup` args of
                          Nothing -> runRebuild conf
                          Just _  -> runForceRebuild conf

displayHelp :: IO ()
displayHelp = putStrLn "think for yourself"
