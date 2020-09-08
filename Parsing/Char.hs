module Parsing.Char where

import Data.Char
import Parsing.Combinators (anyItem, item, items, satisfy)
import Parsing.Parser

anyChar :: Parser Char Char
anyChar = anyItem

char :: Char -> Parser Char Char
char = item

string :: String -> Parser Char String
string = items

space :: Parser Char Char
space = satisfy isSpace

lower :: Parser Char Char
lower = satisfy isLower

asciiLower :: Parser Char Char
asciiLower = satisfy isAsciiLower

upper :: Parser Char Char
upper = satisfy isUpper

asciiUpper :: Parser Char Char
asciiUpper = satisfy isAsciiUpper

alpha :: Parser Char Char
alpha = satisfy isAlpha

alphaNum :: Parser Char Char
alphaNum = satisfy isAlphaNum

digit :: Parser Char Char
digit = satisfy isDigit

octDigit :: Parser Char Char
octDigit = satisfy isOctDigit

hexDigit :: Parser Char Char
hexDigit = satisfy isHexDigit
