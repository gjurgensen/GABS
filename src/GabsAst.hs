module GabsAst where

type Name = String

data Exp = 
    B Bool
  | I Integer
  | Lambda Name Type Exp
  -- Non-normal form:
  | Var Name
  | And Exp Exp
  | Or Exp Exp
  | Not Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Ite Exp Exp Exp
  | App Exp Exp
  deriving Eq

data Type =
    TBool
  | TInt
  | TArr Type Type
  deriving Eq

instance Show Type where
  show TBool = "Bool"
  show TInt  = "Int"
  show (TArr t1 t2) = show t1 ++ " -> " ++ show t2
