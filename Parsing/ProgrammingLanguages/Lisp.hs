module Parsing.ProgrammingLanguages.Lisp where

import Control.Applicative (liftA2)
import Parsing

data LispProgram = LispProgram [SExpr] deriving (Eq, Show)

data SExpr = AtomExpr Atom | ListExpr [SExpr] deriving (Eq, Show)

data Atom = NumberExpr Number | SymbolExpr Symbol deriving (Eq, Show)

type Number = Double

type Symbol = String

lispProgram :: Parser Char LispProgram
lispProgram = sexp `sepByBetween` whiteSpace <&> LispProgram

sexp :: Parser Char SExpr
sexp = (atom <&> AtomExpr) <|> (list <&> ListExpr)

atom :: Parser Char Atom
atom = (number <&> NumberExpr) <|> (symbol <&> SymbolExpr)

-- | Parser for a number that matches the regex @[+-]?(\d+(\.(\d+)?)?|\.\d+)([eE][+-]?\d+)?@
number :: Parser Char Number
number = liftA2 (\s e -> s * 10 ^^ e) significand exponent
  where
    sign :: Num a => Parser Char a
    sign = option 1 $ char '+' $> 1 <|> char '-' $> -1
    signed p = liftA2 (*) sign (p <&> read)
    significand = signed $ withIntegerPart <|> withoutIntegerPart
    withIntegerPart = digits <> (option ".0" $ string "." <> option "0" digits)
    withoutIntegerPart = pure "0" <> string "." <> digits
    exponent = option 0 $ oneOf "eE" *> signed digits
    digits = some digit

symbol :: Parser Char Symbol
symbol = some alphaNum

list :: Parser Char [SExpr]
list = simpleList <|> quotedSexp
  where
    simpleList = between (char '(') (char ')') $ sexp `sepByBetween` whiteSpace
    -- Lisp's quote shorthand: ' (treat "'SEXP" as "(quote SEXP)")
    quotedSexp = char '\'' *> whiteSpace *> sexp <&> toQuoteList
    toQuoteList s = [AtomExpr (SymbolExpr "quote"), s]

whiteSpace :: Parser Char ()
whiteSpace = skipMany space
