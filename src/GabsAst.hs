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

instance Show Exp where
  show exp = case exp of
    B b -> show b
    I i -> show i
    Lambda n t e -> "λ" ++ n ++ ": " ++ show t ++ ". " ++ show e
    Var n -> n
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
