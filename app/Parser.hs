module Parser where

import Language.CNF.Parse.ParseDIMACS (parseFile, Clause, CNF (clauses, CNF, numVars, numClauses))
import Data.Either (fromRight)
import Data.Array.IArray (bounds, (!))
import Solver (Literal, Formula)

cnf2formula :: [Clause] -> Formula
cnf2formula cnf = cnf2formula' cnf 0 []

cnf2formula' :: [Clause] -> Int -> Formula -> Formula
cnf2formula' cnf i f
    | i < length cnf = cnf2formula' cnf (i+1) (cnfClause2Clause' (cnf !! i) 0 [] : f)
    | otherwise = f


cnfClause2Clause' :: Clause -> Int -> [Literal] -> [Literal]
cnfClause2Clause' c i l
    | i <= snd (bounds c) = cnfClause2Clause' c (i+1) ((c ! i):l)
    | otherwise = l


parse :: FilePath -> IO Formula
parse path = do
    o <- parseFile path
    let c = clauses (fromRight (CNF{numVars = 0, numClauses = 0, clauses=[]}) o)
    let f = cnf2formula c
    return f