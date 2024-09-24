{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}


module Evaluators where


-- Imports
import Register
import Type_Definitions
import Control.Monad.State 
import Data.Bits(complement, (.&.), (.|.), xor, shiftL, shiftR)

--if insnumber < sizeof instructions ...

interpInstruction :: Instruction -> InterpM ()

--Since were getting a Int as an argument, and the update function requires a word
-- we cast it into a Word32 using the fromIntegral function
interpInstruction (Mov regName (Lit value)) = do
    updateRegister regName (fromIntegral value)
    incrementInstructionNumber 1
interpInstruction (Mov regName (RegName name)) = do
    newValue <- getRegisterValue name
    updateRegister regName newValue
    incrementInstructionNumber 1

interpInstruction (Add regName (Lit value)) = do
    regValue <- getRegisterValue regName
    let result = toInteger regValue + value
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1
interpInstruction (Add regName (RegName name)) = do
    regValue <- getRegisterValue regName
    secondValue <- getRegisterValue name
    let result = toInteger regValue  + toInteger secondValue
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1

interpInstruction (Sub regName (Lit value)) = do
    regValue <- getRegisterValue regName
    let result = toInteger regValue - value
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1
interpInstruction (Sub regName (RegName name)) = do
    regValue <- getRegisterValue regName
    secondValue <- getRegisterValue name
    let result = toInteger regValue  - toInteger secondValue
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1

interpInstruction (Mul regName (Lit value)) = do
    regValue <- getRegisterValue regName
    let result = toInteger regValue * value
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1
interpInstruction (Mul regName (RegName name)) = do
    regValue <- getRegisterValue regName
    secondValue <- getRegisterValue name
    let result = toInteger regValue  * toInteger secondValue
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1

interpInstruction (Div regName (Lit value)) = do
    regValue <- getRegisterValue regName
    let result = toInteger regValue `div` value
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1
interpInstruction (Div regName (RegName name)) = do
    regValue <- getRegisterValue regName
    secondValue <- getRegisterValue name
    let result = toInteger regValue  `div` toInteger secondValue
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1

interpInstruction (Mod regName (Lit value)) = do
    regValue <- getRegisterValue regName
    let result = toInteger regValue `mod` value
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1
interpInstruction (Mod regName (RegName name)) = do
    regValue <- getRegisterValue regName
    secondValue <- getRegisterValue name
    let result = toInteger regValue  `mod` toInteger secondValue
    updateRegister regName (fromIntegral result)
    incrementInstructionNumber 1

interpInstruction (Neg regName) = do
    regValue <- getRegisterValue regName
    updateRegister regName (complement regValue)
    incrementInstructionNumber 1  

interpInstruction (And regName (Lit value)) = do
    regValue <- getRegisterValue regName
    let result = regValue .&. fromIntegral value
    updateRegister regName result
    incrementInstructionNumber 1  
interpInstruction (And regName (RegName name)) = do
    regValue1 <- getRegisterValue regName
    regValue2 <- getRegisterValue name
    let result = regValue1 .&. regValue2
    updateRegister regName result
    incrementInstructionNumber 1 
    
interpInstruction (Or regName (Lit value)) = do
    regValue <- getRegisterValue regName
    let result = regValue .|. fromIntegral value
    updateRegister regName result
    incrementInstructionNumber 1  
interpInstruction (Or regName (RegName name)) = do
    regValue1 <- getRegisterValue regName
    regValue2 <- getRegisterValue name
    let result = regValue1 .|. regValue2
    updateRegister regName result
    incrementInstructionNumber 1 

interpInstruction (Xor regName (Lit value)) = do
    regValue <- getRegisterValue regName
    let result = regValue `xor` fromIntegral value
    updateRegister regName result
    incrementInstructionNumber 1  
interpInstruction (Xor regName (RegName name)) = do
    regValue1 <- getRegisterValue regName
    regValue2 <- getRegisterValue name
    let result = regValue1 `xor` regValue2
    updateRegister regName result
    incrementInstructionNumber 1 

interpInstruction (Lsh regName value) = do
    regValue <- getRegisterValue regName
    let result = shiftL regValue value
    updateRegister regName result
    incrementInstructionNumber 1 
    
interpInstruction (Rsh regName value) = do
    regValue <- getRegisterValue regName
    let result = shiftR regValue value
    updateRegister regName result
    incrementInstructionNumber 1 

interpInstruction (Ld regName value) = do
    word <- getFromMemory value
    updateRegister regName word
    incrementInstructionNumber 1 

interpInstruction (Ldi regName value) = do
    updateRegister regName (fromIntegral value)
    incrementInstructionNumber 1 


initializeEnv :: [Instruction] -> Env
initializeEnv instr = Env 0 instr [00000000000000000000000000000001] [("regA", 0), ("regB", 0), ("regC", 0), ("regD", 0)]

exampleInstructions :: [Instruction]
exampleInstructions =
    [ Mov "regA" (Lit 10)
    , Mov "regB" (Lit 20)
    , Add "regA" (RegName "regB")
    , Sub "regB" (Lit 5)
    , Ld "regC" 0 
    , Ldi "regD" 15
    ]

runInterpreter :: InterpM ()
runInterpreter = do
    instructionNum <- gets instructionNumber
    instructions <- gets instructionList
    let currentInstruction = (instructions !! instructionNum)
    if instructionNum < length instructions
        then do 
            interpInstruction currentInstruction
            runInterpreter
        else do 
            return () 

