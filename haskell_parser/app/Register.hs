module Register where

import Type_Definitions
import Control.Monad.Except
import Control.Monad.State
import Data.Word

lookUpRegister :: Name -> InterpM Word32
lookUpRegister name = do
    env <- lift get
    case Prelude.lookup name env of
        Just value -> return value
        Nothing -> throwError "Undefined register"-- Gets the registers with a given name, from the list o registers

updateRegister :: Name -> Word32 -> VarEnv -> VarEnv
updateRegister name value [] = [(name, value)]
updateRegister name value ((n, v):xs)
    | name == n  = (name, value) : xs
    | otherwise  = (n, v) : updateRegister name value xs