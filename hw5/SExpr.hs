module SExpr where

import           AParser
import           Control.Applicative
import           Control.Monad       (void)
import qualified Data.Char           as C


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

spaces :: Parser String
spaces = zeroOrMore (satisfy C.isSpace)

-- .
dot :: Parser Char
dot  = satisfy $ not . C.isSpace 

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy C.isAlpha) <*> zeroOrMore (satisfy C.isAlphaNum)

type Ident = String

data Atom = N Integer | I Ident
  deriving Show

data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = spaces *> ((I <$> ident) <|> (N <$> posInt)) <* spaces

openB :: Parser ()
openB = void(char '(') <* spaces

closeB :: Parser ()
closeB = void spaces <* char ')'

parseSExpr :: Parser SExpr
parseSExpr =
  (A <$> parseAtom) <|> (openB *> (Comb <$> oneOrMore parseSExpr) <* closeB)

-- "5"
-- "foo5"
-- (bar (foo) 3 5 874)
-- (((lambda x (lambda y (plus x y))) 3)
-- (   lots  of   (  spaces   in  )  this ( one ) )
