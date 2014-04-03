{-# LANGUAGE OverloadedStrings #-}

import           System.Environment (getArgs)

import Task (runUpdate, runRebuild, runForceRebuild)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("update": as)             -> runUpdate as
    ("rebuild": "--force": as) -> runForceRebuild as
    ("rebuild": as)            -> runRebuild as
    _                          -> displayHelp

displayHelp :: IO ()
displayHelp = putStrLn "think for yourself"
