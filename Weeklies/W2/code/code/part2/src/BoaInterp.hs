-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  ( Env,
    RunError (..),
    Comp (..),
    abort,
    look,
    withBinding,
    output,
    truthy,
    operate,
    apply,
    eval,
    exec,
    execute,
  )
where

import BoaAST
import Control.Monad
import Data.List (intercalate, isInfixOf)

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String])}

instance Monad Comp where
  return a = Comp (\env -> (Right a, []))
  m >>= f =
    Comp
      ( \env ->
          let (result_a, comment_a) = runComp m env
           in case result_a of
                Left e -> (Left e, comment_a)
                Right a ->
                  let (result_b, comment_b) = runComp (f a) env
                   in case result_b of
                        Left e -> (Left e, comment_b)
                        Right b -> (Right b, comment_a ++ comment_b)
      )

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM

instance Applicative Comp where
  pure = return
  (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort runError = Comp (\env -> (Left runError, []))

look :: VName -> Comp Value
look vname =
  Comp
    ( \env ->
        let filter_env = filter (\(name, value) -> name == vname) env
         in if null filter_env
              then (Left (EBadVar vname), [])
              else (Right (snd (head filter_env)), mempty)
    )

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding vname value m =
  Comp (\env -> runComp m ([(vname, value)] ++ env))

output :: String -> Comp ()
output msg = Comp (\env -> (Right (), [msg]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy value = case value of
  TrueVal -> True
  IntVal x -> x /= 0
  StringVal x -> not (null x)
  ListVal [] -> False
  ListVal _ -> True
  _ -> False

operate :: Op -> Value -> Value -> Either String Value
operate operator value1 value2 = case operator of
  Plus -> case (value1, value2) of
    (IntVal x, IntVal y) -> Right (IntVal (x + y))
    (IntVal x, StringVal y) -> Right (StringVal (show x ++ y))
    _ -> Left "unsupported operand, (+) between different type"
  Minus -> case (value1, value2) of
    (IntVal x, IntVal y) -> Right (IntVal (x - y))
    _ -> Left "unsupported operand, (-) between different type"
  Times -> case (value1, value2) of
    (IntVal x, IntVal y) -> Right (IntVal (x * y))
    _ -> Left "unsupported operand, (*) between different type"
  Div -> case (value1, value2) of
    (IntVal x, IntVal y) -> if y == 0 then Left "unsuported operation: division by 0" else Right (IntVal (x `div` y))
    _ -> Left "unsupported operand, (/) between different type"
  Mod -> case (value1, value2) of
    (IntVal x, IntVal y) -> if y == 0 then Left "unsuported operation: division by 0" else Right (IntVal (x `mod` y))
    _ -> Left "unsupported operand, (%) between different type"
  Eq -> case (value1, value2) of
    -- integer
    (IntVal x, IntVal y) -> Right (if x == y then TrueVal else FalseVal)
    -- string
    (StringVal x, StringVal y) -> Right (if x == y then TrueVal else FalseVal)
    (StringVal x, IntVal y) -> Right FalseVal
    (IntVal x, StringVal y) -> Right FalseVal
    -- None
    (NoneVal, _) -> Right FalseVal
    (_, NoneVal) -> Right FalseVal
    -- Bool
    (TrueVal, TrueVal) -> Right TrueVal
    (FalseVal, FalseVal) -> Right TrueVal
    (TrueVal, FalseVal) -> Right FalseVal
    (FalseVal, TrueVal) -> Right FalseVal
    -- list
    (ListVal x, ListVal y) -> if x == y then Right TrueVal else Right FalseVal
    (StringVal _, ListVal _) -> Right FalseVal
    (ListVal _, StringVal _) -> Right FalseVal
    _ -> Left "unsupported operand, (==) between different type"
  Less -> case (value1, value2) of
    (IntVal x, IntVal y) -> Right (if x < y then TrueVal else FalseVal)
    _ -> Left "unsupported operand, (<) between different type"
  Greater -> case (value1, value2) of
    (IntVal x, IntVal y) -> Right (if x > y then TrueVal else FalseVal)
    _ -> Left "unsupported operand, (>) between different type"
  In -> case (value1, value2) of
    (IntVal x, ListVal l) -> if isIn (IntVal x) l then Right TrueVal else Right FalseVal
    (FalseVal, ListVal l) -> if isIn FalseVal l then Right TrueVal else Right FalseVal
    (TrueVal, ListVal l) -> if isIn FalseVal l then Right TrueVal else Right FalseVal
    (NoneVal, ListVal l) -> if isIn FalseVal l then Right TrueVal else Right FalseVal
    -- (StringVal x, StringVal y) -> if isInfixOf x y then Right TrueVal else Right FalseVal
    _ -> Left "unsupported operand, (in)"

isIn :: Value -> [Value] -> Bool
isIn val [] = False
isIn val (h : t) =
  (val == h) || isIn val t

createList :: Int -> Int -> Int -> [Int] -> [Int]
createList start end step acc
  | step < 0 =
      if start <= end
        then acc
        else createList (start + step) end step (acc ++ [start])
  | step > 0 =
      if start >= end
        then acc
        else createList (start + step) end step (acc ++ [start])
  | otherwise = []

range :: [Value] -> Comp Value
range [end] = case end of
  IntVal x -> let r = init [0 .. x] in return (ListVal (map (\x -> IntVal x) r))
  _ -> abort (EBadArg "Arguments for range must be integers")
range [start, end] = case (start, end) of
  (IntVal x, IntVal y) ->
    let r = init [x .. y]
     in return (ListVal (map (\x -> IntVal x) r))
  _ -> abort (EBadArg "Arguments for range must be integers")
range [start, end, step] = case (start, end, step) of
  (IntVal x, IntVal y, IntVal z) ->
    let r = createList x y z []
     in return (ListVal (map (\x -> IntVal x) r))
  _ -> abort (EBadArg "Arguments for range must be integers")
range l = abort (EBadArg "Wrong number of arguments for built-in function 'range'")

printFunListString :: [Value] -> [String] -> [String]
printFunListString [] acc = ["[" ++ if null acc then "[" else (head acc)] ++ (if null acc then [] else tail (init acc)) ++ [if null acc then "]" else (last acc) ++ "]"]
printFunListString (h : t) acc = printFunListString t (acc ++ printFunString [h] [])

printFunString :: [Value] -> [String] -> [String]
printFunString [] acc = acc
printFunString (h : t) acc = case h of
  NoneVal -> printFunString t (acc ++ ["None"])
  TrueVal -> printFunString t (acc ++ ["True"])
  FalseVal -> printFunString t (acc ++ ["False"])
  IntVal x -> printFunString t (acc ++ [show x])
  StringVal x -> printFunString t (acc ++ [x])
  ListVal x -> printFunString t (acc ++ printFunListString x [])

printFun :: [Value] -> [String] -> Comp Value
printFun [] acc = do output (unwords acc); return NoneVal
printFun (h : t) acc = printFun t (acc ++ printFunString [h] [])

-- printFun (h:t) acc = case h of
--                 NoneVal -> printFun t (acc ++ "None")
--                 TrueVal -> printFun t (acc ++ "True") FalseVal -> printFun t (acc ++ "False") IntVal x -> printFun t (acc ++ show x)
--                 StringVal x -> printFun t (acc ++ x)
--                 ListVal x -> printFun t (acc ++ printFunString x "")

apply :: FName -> [Value] -> Comp Value
apply fname list_values = case fname of
  "range" -> range list_values
  "print" -> printFun list_values []
  _ -> abort (EBadFun fname)

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval exp = case exp of
  Const value -> return value
  Var name -> look name
  Oper op exp1 exp2 -> do
    left <- eval exp1
    right <- eval exp2
    case operate op left right of
      Left e -> abort (EBadArg e)
      Right v -> return v
  Not exp -> do
    value <- eval exp
    case value of
      NoneVal -> return TrueVal
      FalseVal -> return TrueVal
      IntVal 0 -> return TrueVal
      StringVal "" -> return TrueVal
      ListVal [] -> return TrueVal
      _ -> return FalseVal
  Call fname params -> do 
    let v = map eval params
    -- let x = map(\x -> a <- x) v
    -- x <- v
    -- Need syntax to get the binding for each Comp value of the array v
    -- apply fname v
    undefined
  List exprs -> undefined
  Compr exp clause_list -> undefined

exec :: Program -> Comp ()
exec [] = output "end"
exec (statement : other) =
  case statement of
    SDef vname exp -> do
      value <- eval exp
      withBinding vname value (exec other)
    SExp exp -> do
      eval exp
      exec other

execute :: Program -> ([String], Maybe RunError)
execute = undefined
