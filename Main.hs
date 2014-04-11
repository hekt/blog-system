{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Error
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))
import           System.Environment (getArgs)

import Model
import IO
import Config (runConfig)
import Server (runServer)
import Task (runUpdate, runRebuild, runForceRebuild)

type Args = [(String, [String])]

main :: IO ()
main = do
  (cmd:args) <- getArgs
  let args' = parseArgs args
  case cmd of
    "update"  -> runUpdate' args'
    "rebuild" -> runRebuild' args'
    "config"  -> runConfig' args'
    "server"  -> runServer' args'
    _         -> putStrLn "no help"

getConfByArgs :: Args -> IO (Either String Configure)
getConfByArgs args = do
  path <- case "conf" `lookup` args of
            Just [p] -> expandTilde p
            Nothing  -> fmap (</> ".kirstie.conf") getHomeDirectory
  getConf path

runUpdate' :: Args -> IO ()
runUpdate' args = ioeLogger . runErrorT $ do
  conf <- ErrorT $ getConfByArgs args
  runUpdate conf

runRebuild' :: Args -> IO ()
runRebuild' args = ioeLogger . runErrorT $ do
  conf <- ErrorT $ getConfByArgs args
  case "force" `lookup` args of
    Nothing -> runRebuild conf
    Just _  -> runForceRebuild conf

runConfig' :: Args -> IO ()
runConfig' args = ioeLogger . runErrorT $ do
  path <- case "output" `lookup` args of
            Nothing  -> liftIO $ fmap (</> ".kirstie.conf") getHomeDirectory
            Just [p] -> liftIO $ expandTilde p
  liftIO $ runConfig path

runServer' :: Args -> IO ()
runServer' args = ioeLogger . runErrorT $ do
  conf <- ErrorT $ getConfByArgs args
  let port = case "port" `lookup` args of
               Nothing  -> 53908
               Just [p] -> maybe 53908 id $ safeRead p
  liftIO $ runServer conf port
