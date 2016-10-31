module AParser where

import           Control.Applicative

import           Control.Monad       (void)
import qualified Data.Char           as C
import qualified Data.Maybe          as M

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x       = Just(x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span C.isDigit xs

first :: (a -> b) -> (a,c) -> (b,c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f Parser {runParser=g} = Parser {runParser=h}
    where h x = fmap (first f) (g x)

instance Applicative Parser where
  pure x = Parser {runParser= \s -> Just(x, s)}
  Parser {runParser=f} <*> Parser {runParser=g} = Parser {runParser=h}
    where
      h xs
        | M.isNothing $ f xs  = Nothing
        | M.isNothing $ g xs' = Nothing
        | otherwise           = Just (f' g', xs'')
        where Just (f', xs')  = f xs
              Just (g', xs'') = g xs'

instance Alternative Parser where
  empty = Parser h where h _ = Nothing
  Parser f <|> Parser g = Parser h
    where h xs = f xs <|> g xs

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (const . const ()) <$> void (char 'a') <*> void (char 'b')

intPair :: Parser [Integer]
intPair = (\ a _ b -> [a, b]) <$> posInt <*> char ' ' <*>  posInt

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy (`elem` ['A'..'Z']))
