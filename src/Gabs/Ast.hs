module Gabs.Ast where

import qualified Data.Map.Strict as Map

type Name = String

type Env = Map.Map Name Expr
emptyEnv :: Env
emptyEnv = Map.empty

data NormalExpr
  = B Bool
  | I Integer
  | Lambda Env Name Expr
  deriving Eq

-- TODO: print environment with lambdas
instance Show NormalExpr where
  show exp = case exp of
    B b -> show b
    I i -> show i
    Lambda _ n e -> "λ" ++ n ++ ". " ++ show e

data Expr
  = Norm NormalExpr
  | Var Name
  | Fix Expr
  | Ite Expr Expr Expr
  | Eq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Lte Expr Expr
  | Gte Expr Expr
  | App Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Times Expr Expr
  | Div Expr Expr
  deriving Eq

-- Over eager with the parentheses.
-- TODO: use precedence to intelligently parenthesize
instance Show Expr where
  show expr = case expr of
    Norm n -> show n
    Var n -> n
    Fix e -> "fix " ++ parens (show e)
    Eq e1 e2 -> parens $ show e1 ++ " = " ++ show e2
    Lt e1 e2 -> parens $ show e1 ++ " < " ++ show e2
    Gt e1 e2 -> parens $ show e1 ++ " > " ++ show e2
    Lte e1 e2 -> parens $ show e1 ++ " <= " ++ show e2
    Gte e1 e2 -> parens $ show e1 ++ " >= " ++ show e2
    And e1 e2 -> parens $ show e1 ++ " and " ++ show e2
    Or e1 e2 -> parens $ show e1 ++ " or " ++ show e2
    Not e -> "not " ++ parens (show e)
    Plus e1 e2 -> parens $ show e1 ++ " + " ++ show e2
    Minus e1 e2 -> parens $ show e1 ++ " - " ++ show e2
    Times e1 e2 -> parens $ show e1 ++ " * " ++ show e2
    Div e1 e2 -> parens $ show e1 ++ " / " ++ show e2
    Ite e1 e2 e3 -> "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    App e1 e2 -> parens (show e1) ++ " " ++ parens (show e2)
    where
      parens s = "(" ++ s ++  ")"

data SugarNormalExpr
  = SB Bool
  | SI Integer
  | SLambda Env Name SugarExpr
  deriving Eq

data SugarExpr
  = SNorm SugarNormalExpr
  | SVar Name
  | SFix SugarExpr
  | SIte SugarExpr SugarExpr SugarExpr
  | SEq SugarExpr SugarExpr
  | SLt SugarExpr SugarExpr
  | SGt SugarExpr SugarExpr
  | SLte SugarExpr SugarExpr
  | SGte SugarExpr SugarExpr
  | SApp SugarExpr SugarExpr
  | SAnd SugarExpr SugarExpr
  | SOr SugarExpr SugarExpr
  | SNot SugarExpr
  | SPlus SugarExpr SugarExpr
  | SMinus SugarExpr SugarExpr
  | STimes SugarExpr SugarExpr
  | SDiv SugarExpr SugarExpr
  -- New exprs
  | LetIn Name SugarExpr SugarExpr
  | LetRec Name SugarExpr SugarExpr
  deriving Eq

data Type
  = TBool
  | TInt
  | TArr Type Type
  deriving Eq

instance Show Type where
  show TBool = "Bool"
  show TInt  = "Int"
  show (TArr t1 t2) = show t1 ++ " -> " ++ show t2
