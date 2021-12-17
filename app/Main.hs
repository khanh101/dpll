module Main where
import Data.Sort (sortOn)
import Solver (solve)
import Parser (parse)
import Debug.Trace (trace)
import System.Posix.Internals (puts)


main :: IO ()
main = do
    f <- parse "test.cnf"
    let (sat, assignment) = solve f
    if not sat
        then putStrLn "UNSAT"
        else
            putStrLn ("SAT\n" ++ unwords (map show (sortOn abs assignment)))
