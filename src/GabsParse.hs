{-# LANGUAGE FlexibleContexts #-}

module GabsParse where

import GabsAst
import GabsType
import GabsEval

import Data.Char
import Data.Maybe
import Control.Monad

import Text.Parsec

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens p = do
  char '(' >> spaces
  ret <- p
  spaces >> char ')'
  pure ret

spaceSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
spaceSep p = do
  many1 space
  ret <- p
  many1 space
  pure ret

optSpaceSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
optSpaceSep p = do
  spaces
  ret <- p
  spaces
  pure ret

true :: Stream s m Char => ParsecT s u m Exp
true = string "True" >> pure (B True)

false :: Stream s m Char => ParsecT s u m Exp
false = string "False" >> pure (B False)

int :: Stream s m Char => ParsecT s u m Exp
int = do
  isNeg  <- fmap isJust $ optionMaybe $ char '~'
  digits <- many1 digit
  let num = read digits
  pure $ I $ if isNeg then negate num else num

name :: Stream s m Char => ParsecT s u m Name
name = do
  h <- satisfy isLetter
  t <- many $ satisfy permittedChar
  let name = h:t
  when (name `elem` forbiddenWords) $ fail $ name ++ " is a keyword" --fail isn't right thing to do
  pure name
  where
    permittedChar c = isLetter c || isDigit c || c == '_'
    forbiddenWords  = ["True", "False"]

lambda :: Stream s m Char => ParsecT s u m Exp
lambda = do
  char 'λ' >> spaces
  name <- name
  optSpaceSep $ char ':'
  typ  <- typ
  optSpaceSep $ char '.'
  exp  <- expression
  pure $ Lambda name typ exp

var :: Stream s m Char => ParsecT s u m Exp
var = Var <$> name

-- Needs lookahead
--infixOp :: Stream s m Char => ParsecT s u m 

andOp :: Stream s m Char => ParsecT s u m Exp
andOp = do
  expL <- expression
  optSpaceSep $ string "&&"
  expR <- expression
  pure $ And expL expR

orOp :: Stream s m Char => ParsecT s u m Exp
orOp = do
  expL <- expression
  optSpaceSep $ string "||"
  expR <- expression
  pure $ Or expL expR

notOp :: Stream s m Char => ParsecT s u m Exp
notOp = do
  string "!" >> spaces
  exp <- expression
  pure $ Not exp

plusOp :: Stream s m Char => ParsecT s u m Exp
plusOp = do
  expL <- expression
  optSpaceSep $ string "+"
  expR <- expression
  pure $ Plus expL expR

minusOp :: Stream s m Char => ParsecT s u m Exp
minusOp = do
  expL <- expression
  optSpaceSep $ string "-"
  expR <- expression
  pure $ Minus expL expR

timesOp :: Stream s m Char => ParsecT s u m Exp
timesOp = do
  expL <- expression
  optSpaceSep $ string "*"
  expR <- expression
  pure $ Times expL expR

divOp :: Stream s m Char => ParsecT s u m Exp
divOp = do
  expL <- expression
  optSpaceSep $ string "/"
  expR <- expression
  pure $ Div expL expR

ite :: Stream s m Char => ParsecT s u m Exp
ite = do
  string "if" >> spaces
  cond <- expression
  spaceSep $ string "then"
  expT <- expression
  spaceSep $ string "else"
  expE <- expression
  pure $ Ite cond expT expE

app :: Stream s m Char => ParsecT s u m Exp
app = do
  lam <- expression
  many1 space
  exp <- expression
  pure $ App lam exp

expression :: Stream s m Char => ParsecT s u m Exp
expression = spaces >> choice [
  parens expression, ite, notOp, lambda, --these are all safe options to try
  -- the binops need some sort of look ahead?
  true, false, int, var] -- these need to be last resort

tbool :: Stream s m Char => ParsecT s u m Type
tbool = string "Bool" >> pure TBool

tint :: Stream s m Char => ParsecT s u m Type
tint = string "Int" >> pure TInt

tarr :: Stream s m Char => ParsecT s u m Type
tarr = do
  typeL <- typ
  spaces >> string "->" >> spaces
  typeR <- typ
  pure $ TArr typeL typeR

-- Broken
typ :: Stream s m Char => ParsecT s u m Type
typ = choice [
  parens typ,
  -- TArr -- needs lookahead
  tbool, tint]

interp :: String -> Either String Exp
interp str = do 
  exp <- mapLeft show $ parse expression "" str
  maybeToEither "Type error" $ typeExp emptyContext exp
  maybeToEither "Runtime error (shouldn't happen!)" $ eval emptyEnv exp
  where
    mapLeft _ (Right x) = Right x
    mapLeft f (Left  x) = Left $ f x
    maybeToEither x Nothing  = Left x
    maybeToEither _ (Just x) = Right x

interpTest str = putStrLn $ case interp str of
  Left  err -> "Error: " ++ err
  Right exp -> show exp
