module GabsAst where

import qualified Data.Map.Strict as Map

type Name = String

type Env = Map.Map Name Expr
emptyEnv = Map.empty :: Env

data NormalExpr =
    B Bool
  | I Integer
  | Lambda Env Name Type Expr
  deriving Eq

instance Show NormalExpr where
  show exp = case exp of
    B b -> show b
    I i -> show i
    Lambda _ n t e -> "λ" ++ n ++ ": " ++ show t ++ ". " ++ show e

data Expr =
    Norm NormalExpr
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

data Type =
    TBool
  | TInt
  | TArr Type Type
  deriving Eq

instance Show Type where
  show TBool = "Bool"
  show TInt  = "Int"
  show (TArr t1 t2) = show t1 ++ " -> " ++ show t2
