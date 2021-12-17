module Solver where

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
substitute ::  Literal -> Formula -> Formula
substitute _ [] = []
substitute l (x:xs)
    | l `elem` x = substitute l xs  -- l in the first clause, remove that clause, return substitution of the rest
    | (-l) `elem` x = substitute l (delete (-l) x : xs) -- (-l) in the first clause, remove the first (-l) in the clause, return substitution of the remaining
    | otherwise = x : substitute l xs -- either l or (-l) not in the first clause, return first clause ++ substitution of the rest

-- solve : formula is sat
solve :: Formula -> (Bool, Assignment)
solve f = solveWith f []

data Action = ActionUnitProp | ActionGuess deriving (Show)

-- solveWith : solve a formula, push assignment to canvas and return
solveWith :: Formula -> Assignment -> (Bool, Assignment)
solveWith [] canvas = (True, canvas)
solveWith formula canvas =
    case scanResult of  ScanEmptyClause -> (False, [])
                        ScanUnitClause activated -> solveWith (substitute activated formula) (activated:canvas)
                        ScanNothing
                            | b1 -> (True, l1)
                            | b2 -> (True, l2)
                            | otherwise -> (False, [])
    where 
        scanResult = scanFormula formula
        guess = head $ head formula
        (b1, l1) = solveWith (substitute guess formula) (guess:canvas)
        (b2, l2) = solveWith (substitute (-guess) formula) (-guess:canvas)
