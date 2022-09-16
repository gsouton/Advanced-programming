-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Arithmetic
import Data.List (intercalate)
import Definitions
import System.Exit (exitFailure, exitSuccess) -- for when running stand-alone

tests :: [(String, Bool)]
tests = [test1, test2, test3]
  where
    test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
    test2 = ("test2", evalSimple (Mul (Cst 2) (Add (Cst 3) (Cst 4))) == 14)
    test3 =
      ( "test3",
        evalFull
          ( If
              { test = Sub (Cst 2) (Cst 2),
                yes = Div (Cst 3) (Cst 0),
                no = Cst 5
              }
          )
          initEnv
          == 5
      )
    test4 = ("test4", evalFull (Var "a") (extendEnv "a" 5 initEnv) == 5)
    test5 =
      ( "test5",
        evalFull
          ( Let
              { var = "a",
                def = Cst 1,
                body = Cst 5
              }
          )
          initEnv
          == 5
      )

-- test5 = ("test5", evalFull (Var "a") initEnv == 42)
-- test4 = ("test4", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)

-- test3 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
   in case failed of
        [] -> do
          putStrLn "All tests passed!"
          exitSuccess
        _ -> do
          putStrLn $ "Failed tests: " ++ intercalate ", " failed
          exitFailure
