module Env_Manipulation where

import Type_Definitions
import Control.Monad.Except
import Control.Monad.State
import Data.Word (Word32)

-- increments the current instruction number by a given number, usefull for jump instruction and regular incrementing
incrementInstructionNumber :: Int -> InterpM()
incrementInstructionNumber n = do
    env <- get
    let newNumber = instructionNumber env + n
    put env {instructionNumber = newNumber}

getRegisterValue :: Name -> InterpM Word32
getRegisterValue name = do
    env <- get
    let regValue = lookup name (registers env)
    case regValue of
        Just value -> return value
        Nothing -> throwError $ "The register: " ++ name ++ "does not exist"

-- updates a register value, gievn its name and a new value
updateRegister :: Name -> Word32 -> InterpM ()
updateRegister regName newWord = do
    env <- get
    let newRegisters = map update (registers env)
    put env {registers = newRegisters}
        where
            update (name, value)
                | name == regName = (name, newWord)
                | otherwise = (name, value)

-- gets the instruction to be executed based on the counter inside the env
getInstruction :: InterpM Instruction
getInstruction = do
    currentInstruction <- gets instructionNumber
    instructions <- gets instructionList
    return (instructions !! currentInstruction)

-- gets a word from a given memmory position
getFromMemory :: Int -> InterpM Word32
getFromMemory n = do
    memory <- gets memoryEnv
    return (memory !! n)

updateMemory :: Word32 -> Int -> InterpM ()
updateMemory word n = do
    env <- get
    mem <- gets memoryEnv 
    let newMemory = take n mem ++ [word] ++ drop (n + 1) mem
    put env {memoryEnv = newMemory}
