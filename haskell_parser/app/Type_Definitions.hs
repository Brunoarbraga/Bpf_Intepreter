{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}

module Type_Definitions where

import Control.Monad.Except
import Control.Monad.State

import Data.Word


-- Type Synnonyms
type Name = String
type Register = (Name, Word32) --Change from Integer to Word (from Data.Word), maybe? 

--Environments: Represent the memory and all the registers, respectively
type MemoryEnv = [Word32]
type VarEnv = [Register]

--Monads
type InterpM a = ExceptT String (StateT VarEnv IO) a

-- Monad interpreter function
runInterp :: InterpM a -> VarEnv -> IO (Either String a, VarEnv)
runInterp ev st = runStateT (runExceptT ev) st

-- Instruction Set type (performs operations with registers)
data Instruction a where
    Lit :: Int -> Instruction Int -- Literal, can only be integers (or words) in this instruction set
    Reg :: Name -> Instruction Int -- Since registers can be used in operations, we need a constructor to extract their values  
    Mov :: Name -> Instruction a -> Instruction a
    Add :: Name -> Instruction a -> Instruction a -- Addition can be done with Integers, in that case, just converts the Int to Bit and do bitwise OR, which is the same thing as adding
    Sub :: Name -> Instruction a -> Instruction a
    Mul :: Name -> Instruction a -> Instruction a
    Div :: Name -> Instruction a -> Instruction a
    Mod :: Name -> Instruction a -> Instruction a
    Neg :: Name -> Instruction a
    And :: Name -> Instruction a -> Instruction a
    Or  :: Name -> Instruction a -> Instruction a
    Xor :: Name -> Instruction a -> Instruction a
    Lsh :: Name -> Instruction a -> Instruction a
    Rsh :: Name -> Instruction a -> Instruction a





 

