module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--  E ::= T R
--  R ::= "+" T R | "-" T R | empty
--  T ::= num

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

-- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a -- may use synomym for easier portability to Parsec

type ParseError = String -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

pExp :: Parser Exp
pExp = do term <- pTerm; pRest term

pRest :: Exp -> Parser Exp
pRest term1 =
  do add_op <- pAdd; term2 <- pTerm; pRest (add_op term1 term2)
    <|> do minus_op <- pNegate; term2 <- pTerm; pRest (Add term1 (minus_op term2))
    <|> return term1

pTerm :: Parser Exp
pTerm = do n <- pNum; return (Num n)

pNum :: Parser Int
pNum =
  do char '-'; pNum' '-'
    <|> do pNum' ' '

pNum' :: Char -> Parser Int
pNum' sign = do n <- satisfy isDigit; ns <- munch (\x -> isDigit x && x /= '0'); return (read (sign : n : ns))

pAdd :: Parser (Exp -> Exp -> Exp)
pAdd = do string "+"; return Add

pNegate :: Parser (Exp -> Exp)
pNegate = do string "-"; return Negate

parseString :: String -> Either ParseError Exp
parseString input = case readP_to_S (do skipSpaces; a <- pExp; eof; return a) input of
  [] -> Left "Cannot parse"
  [(a, _)] -> Right a
  _ -> error "Grammar is ambigious"
