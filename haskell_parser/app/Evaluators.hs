{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}


module Evaluators where


-- Imports
import Register
import Type_Definitions

import Data.Bits(complement, (.&.), (.|.), xor, shiftL, shiftR)
import Control.Monad.State
import Control.Concurrent.STM ()



interpInstruction :: Instruction t -> InterpM Int
interpInstruction (Lit word) = return word
interpInstruction (Reg reg) = do
    word <- lookUpRegister reg
    return (fromIntegral word)
interpInstruction (Mov regName value) = do
    val1 <- interpInstruction value
    modify (\env -> updateRegister regName (fromIntegral val1) env)
    return 1
interpInstruction (Add regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = fromIntegral val1 + val2
    modify (\env -> updateRegister regName (fromIntegral result) env)
    return 1
interpInstruction (Sub regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = fromIntegral val1 - val2
    modify (\env -> updateRegister regName (fromIntegral result) env)
    return 1
interpInstruction (Mul regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = fromIntegral val1 * val2
    modify (\env -> updateRegister regName (fromIntegral result) env)
    return 1    
interpInstruction (Div regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = fromIntegral val1 `div` val2
    modify (\env -> updateRegister regName (fromIntegral result) env)
    return 1
interpInstruction (Mod regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = fromIntegral val1 `mod` val2
    modify (\env -> updateRegister regName (fromIntegral result) env)
    return 1
interpInstruction (Neg regName) = do
    val1 <- lookUpRegister regName 
    let result = complement val1
    modify (\env -> updateRegister regName result env)
    return 1   
interpInstruction (And regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = val1 .&. fromIntegral val2
    modify (\env -> updateRegister regName result env)
    return 1
interpInstruction (Or regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = val1 .|. fromIntegral val2
    modify (\env -> updateRegister regName result env)
    return 1
interpInstruction (Xor regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = val1 `xor` fromIntegral val2
    modify (\env -> updateRegister regName result env)
    return 1
interpInstruction (Lsh regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = shiftL val1 (fromIntegral val2)
    modify (\env -> updateRegister regName result env)
    return 1
interpInstruction (Rsh regName value) = do
    val1 <- lookUpRegister regName 
    val2 <- interpInstruction value
    let result = shiftR val1 (fromIntegral val2)
    modify (\env -> updateRegister regName result env)
    return 1

-- runInstructions :: [Instruction a] -> InterpM ()
-- runInstructions instructions  = do
--     let result = map interpInstruction instructions
--     return ()