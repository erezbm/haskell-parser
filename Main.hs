module Main where

import Parsing
import Parsing.ProgrammingLanguages.Lisp

main :: IO ()
main = print $ parse lispProgram "(123 5fg (  this \nis a test\n)) '(2(8 9)) '5 another ' (one ) \n "
