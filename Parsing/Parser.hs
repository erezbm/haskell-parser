module Parsing.Parser where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)
import Control.Monad (MonadPlus)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))

-- TODO better error reporting
newtype Parser a b = Parser {parse :: [a] -> Maybe (b, [a])}

instance Functor (Parser x) where
  -- fmap = flip (.) parse . (.) Parser . (.) . fmap . (.) swap . flip (.) swap . fmap
  -- fmap = flip (.) parse . (.) Parser . (.) . fmap . first
  fmap f p = Parser $ fmap (first f) . parse p

instance Applicative (Parser x) where
  pure a = Parser $ \xs -> pure (a, xs)
  f <*> a = Parser $ \xs -> do
    (parsedF, xs') <- parse f xs
    parse a xs' <&> first parsedF

instance Monad (Parser x) where
  a >>= f = Parser $ \xs -> do
    (parsedA, xs') <- parse a xs
    parse (f parsedA) xs'

instance Alternative (Parser x) where
  empty = Parser $ pure empty
  p1 <|> p2 = Parser $ \xs -> parse p1 xs <|> parse p2 xs

instance MonadPlus (Parser a)

instance Semigroup b => Semigroup (Parser a b) where
  (<>) = liftA2 (<>)

instance Monoid b => Monoid (Parser a b) where
  mempty = pure mempty
