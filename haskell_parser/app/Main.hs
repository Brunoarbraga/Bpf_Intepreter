module Main where

import Type_Definitions
import Evaluators

main :: IO ()
main = do 
    (_, finalState) <- runInterp (interpInstruction (Mov "r0" (Lit 3)))  [("r0",  00000000000000000000000000000000), ("r1", 00000000000000000000000000000001)]
    putStrLn $ "final state: " ++ show finalState --instancia de show mostra o inteiro relacionado à palavra

