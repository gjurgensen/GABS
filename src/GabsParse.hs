-- Adapted from the following tutorial: https://wiki.haskell.org/Parsing_a_simple_imperative_language

{-# LANGUAGE FlexibleContexts #-}

module GabsParse where

import GabsAst
import GabsType
import GabsEval

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
  Token.identStart    = letter   <|> satisfy (== '_'),
  Token.identLetter   = alphaNum <|> satisfy (== '_'),
  Token.reservedNames = ["if", "then", "else", "λ", "+", "-", "~", "*", "/",
                         "and", "or", "not", "Bool", "Int", "->", "lambda"]
}
lexer      = Token.makeTokenParser languageDef
lexeme     = Token.lexeme     lexer
parens     = Token.parens     lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
colon      = Token.colon      lexer
dot        = Token.dot        lexer
integer    = Token.integer    lexer
identifier = Token.identifier lexer
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol     lexer

expr = tryApp (buildExpressionParser opTable subexpr
              <?> "expression")
  where
    subexpr = parens expr
              <|> ite <|> true <|> false <|> int <|> lambda <|> var
              <?> "expression"
    tryApp p = do
      e <- p
      try (App e <$> expr) <|> pure e

opTable = [[notOp],
           [andOp],
           [orOp],
           [timesOp, divOp],
           [plusOp, minusOp]]
  where
    andOp   = Infix  ( reservedOp "and" >> pure And   ) AssocLeft
    orOp    = Infix  ( reservedOp "or"  >> pure Or    ) AssocLeft
    plusOp  = Infix  ( reservedOp "+"   >> pure Plus  ) AssocLeft
    minusOp = Infix  ( reservedOp "-"   >> pure Minus ) AssocLeft
    timesOp = Infix  ( reservedOp "*"   >> pure Times ) AssocLeft
    divOp   = Infix  ( reservedOp "/"   >> pure Div   ) AssocLeft
    notOp   = Prefix $ reservedOp "not" >> pure Not

true  = reserved "True"  >> pure (B True)
false = reserved "False" >> pure (B False)

int = I <$> integer

name = identifier

lambda = do
  void (symbol "λ") <|> reserved "lambda"
  name <- name
  colon
  typ  <- typ
  dot
  expr <- expr
  pure $ Lambda name typ expr

var = Var <$> name

ite = do
  reserved "if"
  cond <- expr
  reserved "then"
  expT <- expr
  reserved "else"
  expE <- expr
  pure $ Ite cond expT expE


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

interpWithName :: String -> String -> Either String Exp
interpWithName fileName src = do
  expr <- mapLeft show $ parse gabs fileName src
  maybeToEither "Type error" $ typeExp emptyContext expr
  maybeToEither "Runtime error (shouldn't happen!)" $ eval emptyEnv expr
  where
    mapLeft _ (Right x) = Right x
    mapLeft f (Left  x) = Left $ f x
    maybeToEither x Nothing  = Left x
    maybeToEither _ (Just x) = Right x

interpFile :: String -> IO (Either String Exp)
interpFile file = do
    src <- readFile file
    pure $ interpWithName file src

interpFileTest file = do
    eithExp <- interpFile file
    putStrLn $ case eithExp of
      Left  err -> "Error: " ++ err
      Right exp -> show exp

interp :: String -> Either String Exp
interp = interpWithName ""

interpTest str = putStrLn $ case interp str of
  Left  err -> "Error: " ++ err
  Right exp -> show exp
