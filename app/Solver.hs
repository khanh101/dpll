module Solver (solve, Literal, Formula, Assignment) where

import Data.List ( delete )

type Literal = Int 

type Formula = [[Literal]]

type Assignment = [Literal]

data Scan = Unsat | Unit Int | Otherwise deriving (Show)

-- scan : scan the formula and return either the formula is SAT, UNSAT or the literal of the first unit clause
scan :: Formula -> Scan
scan [] = Otherwise
scan (x:xs)
    | null x        = Unsat -- first clause is empty -> UNSAT
    | length x == 1 = Unit (head x) -- literal of the first unit clause
    | otherwise     = scan xs -- recursion



-- substitute : set literal to true and return the reduced formula
substitute ::  Formula -> Literal -> Formula
substitute [] _ = []
substitute (x:xs) l
    | l `elem` x    = substitute xs l  -- if l in the first clause, remove that clause, return substitution of the rest
    | (-l) `elem` x = substitute (delete (-l) x : xs) l -- if (-l) in the first clause, remove the first (-l) in the clause, return substitution of the remaining
    | otherwise     = x : substitute xs l -- if neither l or (-l) not in the first clause, return first clause ++ substitution of the rest

-- solve : formula is sat
solve :: Formula -> (Bool, Assignment)
solve f = solveWithAssumption f []


-- solveWithAssumption : solve a formula, push assignment to assumption and return
solveWithAssumption :: Formula -> Assignment -> (Bool, Assignment)
solveWithAssumption [] assumption = (True, assumption)
solveWithAssumption formula assumption =
    case s of
        Unsat           -> (False, [])
        Unit unit  -> solveWithAssumption (substitute formula unit) (unit:assumption)
        Otherwise
            | b1        -> (True, l1)
            | b2        -> (True, l2)
            | otherwise -> (False, [])
    where 
        s = scan formula
        guess = head $ head formula
        (b1, l1) = solveWithAssumption (substitute formula guess) (guess:assumption)
        (b2, l2) = solveWithAssumption (substitute formula (-guess)) (-guess:assumption)
