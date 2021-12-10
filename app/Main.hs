module Main where
import Solver (solve)
import Parser (parse)


main = do
    f <- parse "test.cnf"
    let (sat, assignment) = solve f
    print sat
    print assignment