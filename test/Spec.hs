module Main where

import GabsAst
import GabsEval
import GabsParse

iArith = "1 + 7 * 3 - 2"
iArith_value = I 20
iArith_bad = "1 + * 2"

bArith = "True or False and not False"
bArith_value = B True
bArith_bad = "or and"

iteTest = "if " ++ bArith ++ " then " ++ iArith ++ "else -2"
iteTest_value = iArith_value

lambdaTest = "(λx: Int -> Bool. λy: Int. x y) (λz: Int. True) 3"
lambdatest_value = B True

letTest = "let foo: Bool = True in not foo"
letTest_value = B False

-- Note, the parenthesis around the lambda are unnecessary. They are only for readability
fixTest = "  let fact: Int -> Int = fix λfact: Int -> Int.    \
          \      λx: Int. if x > 1 then x * fact (x-1) else 1 \
          \  in fact 4"

letrecTest = "  letrec fact: Int -> Int = λx: Int.      \
             \      if x > 1 then x * fact (x-1) else 1 \
             \  in fact 4"

main :: IO ()
main = putStrLn "Todo"
