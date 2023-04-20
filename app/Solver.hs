module Solver (solve, Literal, Formula, Assignment) where

import Data.List ( delete )

type Literal = Int 

type Formula = [[Literal]]

type Assignment = [Literal]

data Scan = Unsat | Unit Literal | Guess deriving (Show)

-- scan : scan the formula and return either the formula is SAT, UNSAT or the literal of the first unit clause
scan :: Formula -> Scan
scan [] = Guess -- no unsat or unit clause found
scan (x:xs)
    | null x        = Unsat -- first clause is empty -> UNSAT
    | length x == 1 = Unit (head x) -- literal of the first unit clause
    | otherwise     = scan xs -- recursion



-- subs : set literal to true and return the reduced formula
subs ::  Literal -> Formula -> Formula
subs _ [] = []
subs l (x:xs)
    | l `elem` x    = subs l xs  -- if l in the first clause, remove that clause, return substitution of the rest
    | (-l) `elem` x = subs l (delete (-l) x : xs) -- if (-l) in the first clause, remove the first (-l) in the clause, return substitution of the remaining
    | otherwise     = x : subs l xs -- if neither l or (-l) not in the first clause, return first clause ++ substitution of the rest

-- solve : formula is sat
solve :: Formula -> (Bool, Assignment)
solve = solveWithAssumption []


-- solveWithAssumption : solve a formula, push assignment to assumption and return
solveWithAssumption :: Assignment -> Formula -> (Bool, Assignment)
solveWithAssumption assumption [] = (True, assumption)
solveWithAssumption assumption formula =
    case s of
        Unsat           -> (False, [])
        Unit unit  -> solveWithAssumption (unit:assumption) (subs unit formula) 
        Guess
            | b1        -> (True, l1)
            | b2        -> (True, l2)
            | otherwise -> (False, [])
    where 
        s = scan formula
        guess = head $ head formula
        (b1, l1) = solveWithAssumption (guess:assumption) (subs guess formula) 
        (b2, l2) = solveWithAssumption (-guess:assumption) (subs (-guess) formula) 
