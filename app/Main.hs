module Main where

import GabsParse

import Control.Monad
import Control.Monad.Extra
import Data.List
import Data.List.Extra
import System.Environment
import System.Console.GetOpt
import System.IO

import System.Console.Haskeline

data Flags = Help | Repl

optDescrs = [Option ['h'] ["help"] (NoArg Help) "Print this message",
             Option ['r'] ["repl"] (NoArg Repl) "Launch interactive interpreter"]

helpMsg = usageInfo "Pass a filename, or select one of the following flags: " optDescrs

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ unwords args
  let (flags, notFlags, errs) = getOpt RequireOrder optDescrs args
  if not $ null errs then
    putStrLn $ unlines $ "Error: " : errs
  else
    case (flags, notFlags) of
      ([Help], []) -> putStrLn helpMsg
      ([Repl], []) -> replLoop
      ([], [file]) -> interpFileTest file
      _ -> putStrLn helpMsg

replLoop = runInputT defaultSettings repl

repl = do
  ln <- getInputLine "> "
  whenJust ln $ \ln ->
    case stripPrefix ":" ln of
      Just c  -> command $ trim c
      Nothing -> do
        outputStrLn $ case interp ln of
          (Right res) -> show res
          (Left  err) -> "Error: " ++ err
        repl
  where
    command "q" = pure ()
    command "help" = do
      outputStrLn "Commands: \"q\" to quit; \"help\" to print commands"
      repl
    command c = do
      outputStrLn $ "Unrecognised command: " ++ c
      outputStrLn "Valid commands: \"q\" to quit; \"help\" to print commands"
      repl
