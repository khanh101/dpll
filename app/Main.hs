module Main where
import Data.Sort (sortOn)
import Solver (solve)
import Parser (parse)
import Debug.Trace (trace)


main :: IO ()
main = do
    f <- parse "test.cnf"
    let (sat, assignment) = solve f
    print sat
    print $ sortOn abs assignment
