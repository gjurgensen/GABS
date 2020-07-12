module Main where

import GabsParse

import Control.Monad
import System.Environment
import System.Console.GetOpt
import System.IO

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

-- TODO: arrow key nav (left/right for cursor, up/down for )
replLoop = do
  putStr "> " >> hFlush stdout
  ln <- getLine
  when (ln /= ":q") $
    interpTest ln
    >> replLoop
