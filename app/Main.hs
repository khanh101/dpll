module Main where
import Solver (solve, substitute, scanFormula)
import Parser (parse)
import Debug.Trace (trace)


main :: IO ()
main = do
    f <- parse "test.cnf"
    print f
    let (sat, assignment) = solve f
    print sat
    print assignment
