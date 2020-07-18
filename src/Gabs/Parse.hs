-- Adapted from the following tutorial: https://wiki.haskell.org/Parsing_a_simple_imperative_language

{-# LANGUAGE FlexibleContexts #-}

module Gabs.Parse where

import Gabs.Ast
import Gabs.Type
import Gabs.Eval

import Data.Char
import Data.Maybe
import Control.Monad

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

languageDef = emptyDef {
  Token.commentStart  = "{-",
  Token.commentEnd    = "-}",
  Token.commentLine   = "--",
  Token.identStart    = letter   <|> char '_',
  Token.identLetter   = alphaNum <|> char '_',
  Token.reservedNames = ["if", "then", "else", "λ", "+", "-", "~", "*", "/",
                         "<", ">", "<=", ">=", "=",
                         "and", "or", "not", "Bool", "Int", "->", "lambda",
                         "fix", "let", "in", "letrec"]
}
lexer      = Token.makeTokenParser languageDef
lexeme     = Token.lexeme     lexer
parens     = Token.parens     lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
colon      = Token.colon      lexer
dot        = Token.dot        lexer
integer    = Token.integer    lexer
natural    = Token.natural    lexer
identifier = Token.identifier lexer
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol     lexer


expr = buildExpressionParser opTable (chainl1 subexpr $ pure App)
       <?> "expression"
  where
    foo = chainl1 subexpr $ pure App
    subexpr = choice [parens expr, ite, letIn, letRec,
                      true, false, int, lambda, var]
              <?> "expression"

opTable = [[fixOp, notOp],
           [andOp],
           [orOp],
           [eqOp],
           [ltOp, gtOp, lteOp, gteOp],
           [timesOp, divOp],
           [plusOp, minusOp]]
  where
    eqOp    = Infix  ( reservedOp "="   >> pure Eq    ) AssocNone
    ltOp    = Infix  ( reservedOp "<"   >> pure Lt    ) AssocNone
    gtOp    = Infix  ( reservedOp ">"   >> pure Gt    ) AssocNone
    lteOp   = Infix  ( reservedOp "<="  >> pure Lte   ) AssocNone
    gteOp   = Infix  ( reservedOp ">="  >> pure Gte   ) AssocNone
    andOp   = Infix  ( reservedOp "and" >> pure And   ) AssocLeft
    orOp    = Infix  ( reservedOp "or"  >> pure Or    ) AssocLeft
    plusOp  = Infix  ( reservedOp "+"   >> pure Plus  ) AssocLeft
    minusOp = Infix  ( reservedOp "-"   >> pure Minus ) AssocLeft
    timesOp = Infix  ( reservedOp "*"   >> pure Times ) AssocLeft
    divOp   = Infix  ( reservedOp "/"   >> pure Div   ) AssocLeft
    notOp   = Prefix $ reservedOp "not" >> pure Not
    fixOp   = Prefix $ reservedOp "fix" >> pure Fix

true  = reserved "True"  >> pure (Norm $ B True)
false = reserved "False" >> pure (Norm $ B False)

-- int = I <$> integer
int = do
  isNeg <- fmap isJust $ optionMaybe $ symbol "~"
  num   <- natural
  pure $ Norm $ I $ if isNeg then negate num else num

name = identifier

lambda = do
  void (symbol "λ") <|> reserved "lambda"
  name <- name
  dot
  expr <- expr
  pure $ Norm $ Lambda emptyEnv name expr

var = Var <$> name

ite = do
  reserved "if"
  cond <- expr
  reserved "then"
  expT <- expr
  reserved "else"
  expE <- expr
  pure $ Ite cond expT expE

-- Should let/in get its own constructor in the AST? This would allow us to
-- print back the exact syntax. Maybe a fullAst + coreAst.
letIn = do
  reserved "let"
  name <- name
  symbol "="
  bind <- expr
  reserved "in"
  body <- expr
  pure $ App (Norm $ Lambda emptyEnv name body) bind

-- This demonstrates the utility in having AST constructors. Parsing has to do
-- much, and it has to unravel several steps of "derived form" logic
letRec = do
  reserved "letrec"
  name <- name
  symbol "="
  bind <- expr
  reserved "in"
  body <- expr
  pure $ App (Norm $ Lambda emptyEnv name body)
       (Fix $ Norm $ Lambda emptyEnv name bind)


typ = buildExpressionParser tOpTable subTypExpr <?> "type"
  where
    subTypExpr = parens typ <|> tBool <|> tInt <?> "type"

tOpTable = [[arr]]
  where
    arr = Infix (reservedOp "->" >> pure TArr) AssocRight

tBool = reserved "Bool" >> pure TBool
tInt  = reserved "Int"  >> pure TInt

gabs = do
  whiteSpace
  expr <- expr
  whiteSpace >> eof
  pure expr

-- interpWithName :: String -> String -> Either String NormalExpr
-- interpWithName fileName src = do
--   expr <- mapLeft show $ parse gabs fileName src
--   maybeToEither "Type error" $ typeExp emptyContext expr
--   maybeToEither "Runtime error (shouldn't happen!)" $ eval emptyEnv expr
--   where
--     mapLeft _ (Right x) = Right x
--     mapLeft f (Left  x) = Left $ f x
--     maybeToEither x Nothing  = Left x
--     maybeToEither _ (Just x) = Right x
--
-- interpFile :: String -> IO (Either String NormalExpr)
-- interpFile file = do
--   src <- readFile file
--   pure $ interpWithName file src
--
-- interpFileTest file = do
--   eithExpr <- interpFile file
--   case eithExpr of
--     Left  err -> putStrLn $ "Error: " ++ err
--     Right res -> printResult res
--
-- interp :: String -> Either String NormalExpr
-- interp = interpWithName ""
--
-- interpTest str = case interp str of
--   Left  err -> putStrLn $ "Error: " ++ err
--   Right res -> printResult res
