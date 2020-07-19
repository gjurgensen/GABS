-- Adapted from the following tutorial: https://wiki.haskell.org/Parsing_a_simple_imperative_language

{-# LANGUAGE FlexibleContexts #-}

module Gabs.Parse where

import Gabs.Ast
import Gabs.Type
import Gabs.Eval

import Data.Bifunctor
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


expr = buildExpressionParser opTable (chainl1 subexpr $ pure SApp)
       <?> "expression"
  where
    foo = chainl1 subexpr $ pure SApp
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
    eqOp    = Infix  ( reservedOp "="   >> pure SEq    ) AssocNone
    ltOp    = Infix  ( reservedOp "<"   >> pure SLt    ) AssocNone
    gtOp    = Infix  ( reservedOp ">"   >> pure SGt    ) AssocNone
    lteOp   = Infix  ( reservedOp "<="  >> pure SLte   ) AssocNone
    gteOp   = Infix  ( reservedOp ">="  >> pure SGte   ) AssocNone
    andOp   = Infix  ( reservedOp "and" >> pure SAnd   ) AssocLeft
    orOp    = Infix  ( reservedOp "or"  >> pure SOr    ) AssocLeft
    plusOp  = Infix  ( reservedOp "+"   >> pure SPlus  ) AssocLeft
    minusOp = Infix  ( reservedOp "-"   >> pure SMinus ) AssocLeft
    timesOp = Infix  ( reservedOp "*"   >> pure STimes ) AssocLeft
    divOp   = Infix  ( reservedOp "/"   >> pure SDiv   ) AssocLeft
    notOp   = Prefix $ reservedOp "not" >> pure SNot
    fixOp   = Prefix $ reservedOp "fix" >> pure SFix

true  = reserved "True"  >> pure (SNorm $ SB True)
false = reserved "False" >> pure (SNorm $ SB False)

int = do
  isNeg <- fmap isJust $ optionMaybe $ symbol "~"
  num   <- natural
  pure $ SNorm $ SI $ if isNeg then negate num else num

name = identifier

-- TODO: Add multi-arg function to sugared AST rather than parsing directly
-- into nested lambdas
lambda = do
  void (symbol "λ") <|> reserved "lambda"
  args <- many1 name
  dot
  body <- expr
  pure $ foldr (\arg -> SNorm . SLambda emptyEnv arg) body args

var = SVar <$> name

ite = do
  reserved "if"
  cond <- expr
  reserved "then"
  expT <- expr
  reserved "else"
  expE <- expr
  pure $ SIte cond expT expE

letIn = do
  reserved "let"
  name <- name
  symbol "="
  bind <- expr
  reserved "in"
  body <- expr
  pure $ LetIn name bind body

letRec = do
  reserved "letrec"
  name <- name
  symbol "="
  bind <- expr
  reserved "in"
  body <- expr
  pure $ LetRec name bind body


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


interpWithName :: String -> String -> Either String NormalExpr
interpWithName fileName src = do
  expr <- bimap show desugarExpr $ parse gabs fileName src
  maybeToEither "Type error" $ inferType expr
  maybeToEither "Runtime error (shouldn't happen!)" $ eval emptyEnv expr
  where
    maybeToEither x Nothing  = Left  x
    maybeToEither _ (Just x) = Right x

interpFile :: String -> IO (Either String NormalExpr)
interpFile file = do
  src <- readFile file
  pure $ interpWithName file src

interpFileTest file = do
  eithExpr <- interpFile file
  putStrLn $ case eithExpr of
    Left  err -> "Error: " ++ err
    Right res -> show res

interp :: String -> Either String NormalExpr
interp = interpWithName ""

interpTest str = putStrLn $ case interp str of
  Left  err -> "Error: " ++ err
  Right res -> show res

interpType :: String -> Either String UnifType
interpType s = do
  expr <- bimap show desugarExpr $ parse gabs "" s
  maybeToEither "Type error" $ normalizeUT <$> inferType expr
  where
    maybeToEither x Nothing  = Left  x
    maybeToEither _ (Just x) = Right x
