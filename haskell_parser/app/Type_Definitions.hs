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

data Env = Env {
    instructionNumber :: Int ,
    instructionList :: [Instruction] ,
    memoryEnv :: [Word32] ,
    registers :: [Register]
} deriving Show


--Monads
type InterpM a = ExceptT String (StateT Env IO) a

-- Monad interpreter function
runInterp :: InterpM a -> Env -> IO (Either String a, Env)
runInterp ev st = runStateT (runExceptT ev) st

-- Since operations can be performed on either literals or register values, this type covers those cases
data Arg = Lit Integer | RegName String deriving Show

data Instruction = 
    Mov Name Arg
    | Add Name Arg
    | Sub Name Arg
    | Mul Name Arg
    | Div Name Arg
    | Mod Name Arg
    | Neg Name 
    | And Name Arg
    | Or  Name Arg
    | Xor Name Arg
    | Lsh Name Int
    | Rsh Name Int
    | Ldxw Name Int
    | Ldi Name Int --Same as Mov?
    | Stw Int Name --The actual St instruction in eBPF asm does not have an Int as a parameter, it has a value which is [dst + offset]
                   --For now just Stw, becaus we only have 32-bits words represented (stw = full words)
    deriving Show
 

