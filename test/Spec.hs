module Main where

import Gabs.Ast
-- import Gabs.Eval
import Gabs.Parse
import Gabs.Type

-- GABS test cases

iArith = "1 + 7 * 3 - 2"
iArith_value = I 20
iArith_bad = "1 + * 2"

bArith = "True or False and not False"
bArith_value = B True
bArith_bad = "or and"

iteTest = "if " ++ bArith ++ " then " ++ iArith ++ "else -2"
iteTest_value = iArith_value

lambdaTest = "(λx. λy. x y) (λz. True) 3"
lambdatest_value = B True

letTest = "let foo = True in not foo"
letTest_value = B False

fact = "fix λfact. λx. if x > 1 then x * fact (x-1) else 1"

fixTest = "let fact = fix λfact.    \
        \      λx. if x > 1 then x * fact (x-1) else 1 \
        \  in fact 4"

letrecTest = "letrec fact = λx.      \
           \      if x > 1 then x * fact (x-1) else 1 \
           \  in fact 4"

-- Unification test cases

-- represents constraint {a = b -> a}
badCycle :: [Constraint]
badCycle = [(UTHook 0, UTArr (UTHook 1) (UTHook 0))]

-- represents {a = b -> c, c = b -> d}
smallConstr :: [Constraint]
smallConstr = [(UTHook 0, UTArr (UTHook 1) (UTHook 2))
              ,(UTHook 2, UTArr (UTHook 1) (UTHook 3))]

apply = "fix λapply. λf. λv. λt. if t = 0 then v else apply f (f v) (t-1)"

main :: IO ()
main = putStrLn "Todo"
