module Main where

import Type_Definitions
import Evaluators

main :: IO ()
main = do 
    let initialEnv = initializeEnv exampleInstructions
    (_, finalEnv) <- runInterp (runInterpreter) initialEnv
    putStrLn $ "final state: \n" 
    putStrLn $  show (registers finalEnv) --instancia de show mostra o inteiro relacionado à palavra
    putStrLn $  show (memoryEnv finalEnv) 

