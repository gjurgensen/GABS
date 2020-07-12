module Main where

import GabsAst
import GabsEval
import GabsParse

iArith = "1 + 7 * 3 - 2"
iArith_value = I 20
iArith_bad = "1 + * 2"
iArith_test = interp iArith == Right iArith_value

bArith = "True or False and not False"
bArith_value = B True
bArith_bad = "or and"

iteTest = "if " ++ bArith ++ " then " ++ iArith ++ "else -2"
iteTest_value = iArith_value

-- Needs var
-- lambdaTest

main :: IO ()
main = putStrLn "Todo"
