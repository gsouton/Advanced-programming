module WarmupParsec where

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

import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)


-- Optional: if not attempted, leave as undefined
parseString :: String -> Either ParseError Exp
parseString = undefined
