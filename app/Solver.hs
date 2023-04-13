module Solver (solve, Literal, Formula, Assignment) where

import Data.List ( delete )

type Literal = Int 

type Formula = [[Literal]]

type Assignment = [Literal]

data ScanResult = ScanEmptyClause | ScanUnitClause Int | ScanNothing deriving (Show)
-- scanFormula
scanFormula :: Formula -> ScanResult
scanFormula [] = ScanNothing
scanFormula (x:xs)
    | null x = ScanEmptyClause -- empty clause
    | length x == 1 = ScanUnitClause (head x) -- unit clause
    | otherwise = scanFormula xs -- recursion



-- substitute : set literal to true and return reduced formula
substitute ::  Formula -> Literal -> Formula
substitute [] _ = []
substitute (x:xs) l
    | l `elem` x = substitute xs l  -- l in the first clause, remove that clause, return substitution of the rest
    | (-l) `elem` x = substitute (delete (-l) x : xs) l -- (-l) in the first clause, remove the first (-l) in the clause, return substitution of the remaining
    | otherwise = x : substitute xs l -- either l or (-l) not in the first clause, return first clause ++ substitution of the rest

-- solve : formula is sat
solve :: Formula -> (Bool, Assignment)
solve f = solveWith f []


-- solveWith : solve a formula, push assignment to canvas and return
solveWith :: Formula -> Assignment -> (Bool, Assignment)
solveWith [] canvas = (True, canvas)
solveWith formula canvas =
    case scanResult of  ScanEmptyClause -> (False, [])
                        ScanUnitClause activated -> solveWith (substitute formula activated) (activated:canvas)
                        ScanNothing
                            | b1 -> (True, l1)
                            | b2 -> (True, l2)
                            | otherwise -> (False, [])
    where 
        scanResult = scanFormula formula
        guess = head $ head formula
        (b1, l1) = solveWith (substitute formula guess) (guess:canvas)
        (b2, l2) = solveWith (substitute formula (-guess)) (-guess:canvas)
