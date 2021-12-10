module Solver where

import Data.List ( delete )

type Literal = Int 

type Formula = [[Literal]]

-- isEmptyFormula : formula is empty
isEmptyFormula :: Formula -> Bool 
isEmptyFormula = null

-- hasEmptyClause : formula has at least an empty clause
hasEmptyClause :: Formula -> Bool 
hasEmptyClause [] = False 
hasEmptyClause (x:xs)
    | null x = True
    | otherwise = hasEmptyClause xs

-- getUnitClause : get clause idx with unit literal
-- if not exist, return length formula
getUnitClause :: Formula -> Int 
getUnitClause [] = 0
getUnitClause (x:xs)
    | length x == 1 = 0
    | otherwise = 1 + getUnitClause xs

-- getUnitLiteral : get unit literal from unit clause idx
getUnitLiteral :: Formula -> Int -> Literal
getUnitLiteral f i = head (f !! i)


-- substitute : set literal to true and return reduced formula
substitute ::  Literal -> Formula -> Formula
substitute _ []= []
substitute l (x:xs)
    | l `elem` x = substitute l xs  -- l in the first clause, remove that clause, return substitution of the rest
    | (-l) `elem` x = substitute l (delete (-l) x : xs) -- (-l) in the first clause, remove the first (-l) in the clause, return substitution of the remaining
    | otherwise = x : substitute l xs -- either l or (-l) not in the first clause, return first clause ++ substitution of the rest

-- solve : formula is sat
solve :: Formula -> (Bool, [Literal])
solve f = solve' f []

-- solve' : recursion
solve' :: Formula -> [Literal] -> (Bool, [Literal])
solve' f l
    | isEmptyFormula f = (True, []) -- empty formula is sat
    | hasEmptyClause f = (False, []) -- formula contains empty clause is unsat
    | unitLiteral > 0 = solve' (substitute unitLiteral f) (unitLiteral:l) -- unit propagation if there is a unit literal
    | b1 = (True, guessLiteral:l1)
    | b2 = (True, -guessLiteral:l2)
    | otherwise = (False, [])
    where   unitClauseIdx = getUnitClause f
            unitLiteral = if unitClauseIdx < length f then getUnitLiteral f unitClauseIdx else 0 
            guessLiteral = head (head f)
            (b1, l1) = solve' (substitute guessLiteral f) l
            (b2, l2) = solve' (substitute (-guessLiteral) f) l
