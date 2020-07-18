module Main where

import Gabs.Parse

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
    commands = "\"t <expr>\" to type an epression; \"q\" to quit; \"help\" to print commands"
    command "q" = pure ()
    command ('t':e) = do
      outputStrLn $ case interpType e of
        Left  err -> "Error: " ++ err
        Right res -> show res
      repl
    command "help" = do
      outputStrLn $ "Commands: " ++ commands
      repl
    command c = do
      outputStrLn $ "Unrecognised command: " ++ c
      outputStrLn $ "Valid commands: " ++ commands
      repl
