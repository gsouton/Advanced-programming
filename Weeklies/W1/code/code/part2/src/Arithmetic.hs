-- This is a skeleton file for you to edit

module Arithmetic
  ( showExp,
    evalSimple,
    extendEnv,
    evalFull,
    evalErr,
    showCompact,
    evalEager,
    evalLazy,
  )
where

import Definitions

showExp :: Exp -> String
showExp (Cst integer) = show integer
showExp (Add exp1 exp2) = "(" ++ showExp exp1 ++ ")" ++ "+" ++ "(" ++ showExp exp2 ++ ")"
showExp (Sub exp1 exp2) = "(" ++ showExp exp1 ++ ")" ++ "-" ++ "(" ++ showExp exp2 ++ ")"
showExp (Mul exp1 exp2) = "(" ++ showExp exp1 ++ ")" ++ "*" ++ "(" ++ showExp exp2 ++ ")"
showExp (Div exp1 exp2) = "(" ++ showExp exp1 ++ ")" ++ "`div`" ++ "(" ++ showExp exp2 ++ ")"
showExp (Pow exp1 exp2) = "(" ++ showExp exp1 ++ ")" ++ "^" ++ "(" ++ showExp exp2 ++ ")"
showExp (If {}) = undefined
showExp (Var _) = undefined
showExp (Let {}) = undefined
showExp (Sum {}) = undefined

evalSimple :: Exp -> Integer
evalSimple (Cst integer) = integer
evalSimple (Add exp1 exp2) = evalSimple exp1 + evalSimple exp2
evalSimple (Sub exp1 exp2) = evalSimple exp1 - evalSimple exp2
evalSimple (Mul exp1 exp2) = evalSimple exp1 * evalSimple exp2
evalSimple (Div exp1 exp2) =
  let left = evalSimple exp1
   in let right = evalSimple exp2
       in if right == 0
            then error "Cannot divide by 0"
            else left `div` right
evalSimple (Pow exp1 exp2) =
  let left = evalSimple exp1
   in let right = evalSimple exp2
       in if right < 0
            then error "Pow is non negative operation"
            else left ^ right
evalSimple (If {}) = undefined
evalSimple (Var _) = undefined
evalSimple (Let {}) = undefined
evalSimple (Sum {}) = undefined

extendEnv :: VName -> Integer -> Env -> Env
extendEnv variable_name integer env =
  \var -> if var == variable_name then Just integer else env var

evalFull :: Exp -> Env -> Integer
evalFull (Cst integer) _env = integer
evalFull (Add exp1 exp2) env = evalFull exp1 env + evalFull exp2 env
evalFull (Sub exp1 exp2) env = evalFull exp1 env - evalFull exp2 env
evalFull (Mul exp1 exp2) env = evalFull exp1 env * evalFull exp2 env
evalFull (Div exp1 exp2) env =
  let left = evalFull exp1 env
   in let right = evalFull exp2 env
       in if right == 0
            then error "Cannot divide by 0"
            else left `div` right
evalFull (Pow exp1 exp2) env =
  let left = evalFull exp1 env
   in let right = evalFull exp2 env
       in if right < 0
            then error "Pow is non negative operation"
            else left ^ right
evalFull (If {test = t, yes = y, no = n}) env =
  if evalFull t env /= 0
    then evalFull y env
    else evalFull n env
evalFull (Var name) env = case env name of
  Just int -> int
  Nothing -> error ("[ERROR]: Variable: {" ++ name ++ "} is not binded in the environment")
evalFull (Let {var = v, def = d, body = b}) env =
  evalFull b (extendEnv v (evalFull d env) env)
evalFull (Sum {var = v, from = f, to = t, body = b}) env =
  let begin = evalFull f env
   in let end = evalFull t env
       in sum (map (\n -> evalFull b (extendEnv v n env)) [begin .. end])

-- create the list from begin to end [0, 1, 2, 3, .. n]
-- map the body of the sum to each element of the list
-- sum each element of the list (reduce)

evalBranchErr :: Exp -> Exp -> Env -> Either ArithError (Integer, Integer)
evalBranchErr left_branch right_branch env =
  case evalErr left_branch env of
    Left err -> Left err
    Right l_res -> case evalErr right_branch env of
      Left err -> Left err
      Right r_res -> Right (l_res, r_res)

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst integer) _env = Right integer
evalErr (Add exp1 exp2) env =
  case evalBranchErr exp1 exp2 env of
    Left err -> Left err
    Right (left, right) -> Right (left + right)
evalErr (Sub exp1 exp2) env =
  case evalBranchErr exp1 exp2 env of
    Left err -> Left err
    Right (left, right) -> Right (left - right)
evalErr (Mul exp1 exp2) env =
  case evalBranchErr exp1 exp2 env of
    Left err -> Left err
    Right (left, right) -> Right (left * right)
evalErr (Div exp1 exp2) env =
  case evalBranchErr exp1 exp2 env of
    Left err -> Left err
    Right (left, right) ->
      if right == 0
        then Left EDivZero
        else Right (left `div` right)
evalErr (Pow exp1 exp2) env =
  case evalBranchErr exp1 exp2 env of
    Left err -> Left err
    Right (left, right) ->
      if right < 0
        then Left ENegPower
        else Right (left ^ right)
evalErr (If {test = t, yes = y, no = n}) env =
  case evalErr t env of
    Left err -> Left err
    Right res -> if res /= 0 then evalErr y env else evalErr n env
evalErr (Var name) env = case env name of
  Just integer -> Right integer
  Nothing -> Left (EBadVar name)
evalErr (Let {var = v, def = d, body = b}) env =
  case evalErr d env of
    Left err -> Left err
    Right definition -> evalErr b (extendEnv v definition env)
evalErr (Sum {var = v, from = f, to = t, body = b}) env =
  case evalErr f env of -- evaluate the start index for sum
    Left err -> Left err
    Right begin -> case evalErr t env of -- evaluate the end of index for sum
      Left err -> Left err
      Right end ->
        if begin > end
          then Right 0
          else case evalSum [begin .. end] v b env 0 of
            Left err -> Left err
            Right res -> Right res

-- Sum an array of value while applying to each element the body of Sum
evalSum :: [] Integer -> VName -> Exp -> Env -> Integer -> Either ArithError Integer
evalSum [] _ _ _ res = Right res
evalSum (h : list) vname body env res =
  case evalErr body (extendEnv vname h env) of
    Left err -> Left err
    Right r -> evalSum list vname body env (res + r)

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
