module Parsing.Combinators (module Parsing.Combinators, (<$>), (<&>), (<$), ($>), (<|>), empty, some, many, optional, (<>), mempty) where

import Control.Applicative (Alternative (empty, many, some, (<|>)), liftA2, optional)
import Control.Monad (mfilter)
import Data.Foldable (asum)
import Data.Functor (($>), (<&>))
import Data.List (uncons)
import Parsing.Parser (Parser (Parser))

anyItem :: Parser a a
anyItem = Parser uncons

satisfy :: (a -> Bool) -> Parser a a
satisfy f = mfilter f anyItem

item :: Eq a => a -> Parser a a
item a = satisfy (== a)

items :: (Traversable t, Eq a) => t a -> Parser a (t a)
items as = traverse item as

oneOf :: (Foldable t, Eq a) => t a -> Parser a a
oneOf as = satisfy (`elem` as)

noneOf :: (Foldable t, Eq a) => t a -> Parser a a
noneOf as = satisfy (`notElem` as)

choice :: (Foldable t, Alternative f) => t (f a) -> f a
choice = asum

option :: b -> Parser a b -> Parser a b
option b p = p <|> pure b

between :: Parser a open -> Parser a close -> Parser a b -> Parser a b
between open close p = open *> p <* close

count :: Int -> Parser a b -> Parser a [b]
count n p = sequenceA $ replicate n p

sepBy :: Parser a b -> Parser a sep -> Parser a [b]
sepBy p sep = option [] $ sepBy1 p sep

sepBy1 :: Parser a b -> Parser a sep -> Parser a [b]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

sepBy2 :: Parser a b -> Parser a sep -> Parser a [b]
sepBy2 p sep = liftA2 (:) (p <* sep) (p `sepBy1` sep)

sepByBetween :: Parser a b -> Parser a delim -> Parser a [b]
sepByBetween p delim = delim *> p `sepBy` delim <* delim

sepByBetween1 :: Parser a b -> Parser a delim -> Parser a [b]
sepByBetween1 p delim = delim *> p `sepBy1` delim <* delim

skipSome :: Parser a b -> Parser a ()
skipSome p = some p $> ()

skipMany :: Parser a b -> Parser a ()
skipMany p = many p $> ()
