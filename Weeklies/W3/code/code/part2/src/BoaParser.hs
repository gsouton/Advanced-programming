-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Control.Applicative ((<|>))
import Data.Char (isAlpha, isDigit, isPrint, isSpace)
import Text.ParserCombinators.ReadP

-- Grammar:
--
--  Program         = Stmts
--
--  Stmts           = Stmt Stmts'
--
--  Stmts'          = ";" Stmts | empty
--
--  Stmt            = ident "=" Expr
--                  | Expr
--
--  Exp             = "not" Exp
--                  |  Exp'
--
--  Exp'           = ExpNum ExpBool
--                 | ExpNum
--
--  ExpBool        = "==" ExpNum
--                 | "<"  ExpNum
--                 | ">"  ExpNum
--                 | "in"  ExpNum
--                 
--  ExpNum         = ExprTerm ExprNum'
--
--  ExpNum'        = "+" ExpTerm ExprNum'
--                  | "-" ExpTerm ExprNum'
--                  | empty
--
--  ExpTerm'       = "*" Const ExpNum'
--                 | "//" Const ExpNum'
--                 | empty
--
--  Const'         = num | string | var | True | False | None
--                 | "(" Expr ")"
--                 | ident "(" Exprz ")"
--                 | "[" Exprz "]"
--                 | "[" Exp ForClause Clausez"]"
--
--
--  Exprz           = Exp Expz'
--
--  Expz'          = "," Exp Expz'
--                 | empty
--
--  ForClause       = "for" ident "in" Expr
--
--  IfClause        = "if" Expr
--
--  Clausez         = ForClause Clausez
--                  | IfClause Clausez
--                  | empty
--
--  ident           = [a-z][A-Z | 0-9]
--                    ^(?!None | ?!True| ?!False | ?!for | ?!if | ?!else | ?!in | ?!not)
--  numConst        = [0-9]*
--  stringConst     = ""
--
reserved :: [String]
reserved = ["if", "for", "while", "else", "do", "True", "False", "None", "in", "not"]

type Parser a = ReadP a -- may use synomym for easier portability to Parsec

type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString input = case readP_to_S (do skip; a <- pProgram; eof; return a) input of
  [] -> Left "Cannot parse"
  [(a, _)] -> Right a
  _ -> error "Grammar is ambigious"

skip :: Parser ()
skip =
  do
    skipSpaces
    satisfy (== '#')
    munch (/= '\n')
    skip
    <|> skipSpaces

skip1 :: Parser ()
skip1 =
  do
    satisfy (== '#')
    munch (/= '\n')
    skip1
    <|> do s <- look; if not (null s) && (head s == '(' || head s == '[' )  then do return() else do munch1 isSpace; skip


--  Program         = Stmts
pProgram :: Parser Program
pProgram = pStmts

--  Stmts           = Stmt Stmts'
pStmts :: Parser [Stmt]
pStmts = do stmt <- pStmt; rest <- pStmts'; return (stmt : rest)

--  Stmts'          = ";" Stmts | empty
pStmts' :: Parser [Stmt]
pStmts' =
  do string ";"; skip; pStmts
    <|> return []

--  Stmt            = ident "=" Expr
--                  | Expr
pStmt :: Parser Stmt
pStmt =
  do ident <- pIdent; skip; string "="; skip; exp <- pExp; return (SDef ident exp)
    <|> do exp <- pExp; skip; return (SExp exp)

--  ident           = [a-z][A-Z | 0-9]
--                    ^(?!None | ?!True| ?!False | ?!for | ?!if | ?!else | ?!in | ?!not)
pIdent :: Parser String
pIdent = do
  c <- satisfy (\x -> isAlpha x || x == '_')
  cs <- munch (\x -> isAlpha x || x == '_' || isDigit x)
  let res = c : cs
  if res `notElem` reserved
    then return res
    else pfail

--  Exp             = "not" Exp
--                  |  Exp'
pExp :: Parser Exp
pExp =
  do string "not"; skip; exp <- pExp; return (Not exp)
    <|> do pExp'

--  Exp'           = ExpNum ExpBool
--                 | ExpNum
pExp' :: Parser Exp
pExp' =
  do exp <- pExpNum; skip; pExpBool exp
    <|> pExpNum

--  ExpBool        = "==" ExpNum
--                 | "<"  ExpNum
--                 | ">"  ExpNum
--                 | "in"  ExpNum
pExpBool :: Exp -> Parser Exp
pExpBool left =
  do string "=="; skip; right <- pExpNum; return (Oper Eq left right)
    <|> do string "!="; skip; right <- pExpNum; return (Not (Oper Eq left right))
    <|> do string "<"; skip; right <- pExpNum; return (Oper Less left right)
    <|> do string "<="; skip; right <- pExpNum; return (Not (Oper Greater left right))
    <|> do string ">"; skip; right <- pExpNum; return (Oper Greater left right)
    <|> do string ">="; skip; right <- pExpNum; return (Not (Oper Less left right))
    <|> do string "in"; skip1; right <- pExpNum; return (Oper In left right)
    <|> do string "not"; skip1; string "in"; skip; right <- pExpNum; return (Not (Oper In left right))

--  ExpNum         = ExprTerm ExprNum'
pExpNum :: Parser Exp
pExpNum = do expTerm <- pExpTerm; pExpNum' expTerm

--  ExpNum'        = "+" ExpTerm ExprNum'
--                  | "-" ExpTerm ExprNum'
--                  | empty
pExpNum' :: Exp -> Parser Exp
pExpNum' exp =
  do char '+'; skip; expTerm <- pExpTerm; pExpNum' (Oper Plus exp expTerm)
    <|> do char '-'; skip; expTerm <- pExpTerm; pExpNum' (Oper Minus exp expTerm)
    <|> do return exp

--  ExpTerm        = Const ExpTerm'
pExpTerm :: Parser Exp
pExpTerm = do const <- pConst; pExpTerm' const

--  ExpTerm'       = "*" Const ExpNum'
--                 | "//" Const ExpNum'
--                 | empty
pExpTerm' :: Exp -> Parser Exp
pExpTerm' exp =
  do char '*'; skip; const <- pConst; pExpTerm' (Oper Times exp const)
    <|> do string "//"; skip; const <- pConst; pExpTerm' (Oper Div exp const)
    <|> do char '%'; skip; const <- pConst; pExpTerm' (Oper Mod exp const)
    <|> return exp

--  Const'         = num | string | var | True | False | None
--                 | "(" Expr ")"
--                 | ident "(" Exprz ")"
--                 | "[" Exprz "]"
--                 | "[" Exp ForClause Clausez"]"
pConst :: Parser Exp
pConst =
  do n <- pNum; return (Const (IntVal n))
    <|> do string <- pString; return (Const (StringVal string))
    <|> do ident <- pIdent; return (Var ident)
    <|> do string "True"; return (Const TrueVal)
    <|> do string "False"; return (Const FalseVal)
    <|> do string "None"; return (Const NoneVal)
    <|> do string "("; skip; exp <- pExp; skip; string ")"; skip; return exp
    <|> do ident <- pIdent; skip; string "("; skip; call <- pExpCall ident; skip; string ")"; skip; return call
    <|> do string "["; skip; exp <- pExpz; skip; string "]"; return (List exp)
    <|> do string "["; skip; exp <- pExp; skip; for_clause <- pFor; skip; res <- pClausez exp [for_clause]; string "]"; return res

pNum :: Parser Int
pNum =
  do char '-'; pNum' '-'
    <|> do pNum' ' '

pNum' :: Char -> Parser Int
pNum' sign = do n <- satisfy isDigit; ns <- munch (\x -> isDigit x && x /= '0'); return (read (sign : n : ns))

pString :: Parser String
pString = do char '\''; str <- pString' ""; skip; return str

pString' :: String -> Parser String
pString' s =
  do char '\\'; char 'n'; pString' (s ++ "\n")
    <|> do char '\\'; char '\''; pString' (s ++ "'")
    <|> do char '\\'; char '\\'; pString' (s ++ "\\")
    <|> do char '\\'; char '\n'; pString' s
    <|> do c <- satisfy (\c -> c /= '\\' && c /= '\'' && isPrint c); pString' (s ++ [c])
    <|> do char '\''; return s

--  Exprz           = Exp Expz'
pExpz :: Parser [Exp]
pExpz =
  do exp <- pExp; skip; pExpz' [exp]
    <|> return []

--  Expz'          = "," Exp Expz'
--                 | empty
pExpz' :: [Exp] -> Parser [Exp]
pExpz' exp_list =
  do char ','; skip; exp <- pExp; pExpz' (exp_list ++ [exp])
    <|> return exp_list

--  ExpCall         = Exp ExpCall'
pExpCall :: String -> Parser Exp
pExpCall name =
  do exp <- pExp; skip; res <- pExpCall' name [exp]; return res
    <|> return (Call name [])

--  ExpCall'       = "," Exp Expz'
--                 | empty
pExpCall' :: String -> [Exp] -> Parser Exp
pExpCall' name exp_list =
  do char ','; skip; exp <- pExp; skip; pExpCall' name (exp_list ++ [exp])
    <|> return (Call name exp_list)

--  ForClause       = "for" ident "in" Expr
pFor :: Parser CClause
pFor = do
  string "for"
  skip1
  ident <- pIdent
  skip1
  string "in"
  skip1
  exp <- pExp
  skip
  return (CCFor ident exp)

-- Clausez          = ForClause Clausez
--                  | IfClause Clausez
--                  | empty
pClausez :: Exp -> [CClause] -> Parser Exp
pClausez exp clauses =
  do for <- pFor; pClausez exp (clauses ++ [for])
    <|> do _if <- pIf; pClausez exp (clauses ++ [_if])
    <|> return (Compr exp clauses)

--  IfClause        = "if" Expr
pIf :: Parser CClause
pIf = do string "if"; skip1; exp <- pExp; skip; return (CCIf exp)
