module JvmGenerator.Monad where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Grammar.AbsInstant
import qualified Data.DList as DL
import JvmGenerator.Types

type Loc = Int

type VariablesMap = M.Map Ident Loc

data ProgramState = ProgramState {
  varsMap :: VariablesMap,
  nextLoc :: Loc,
  maxStackSize :: Int
}

emptyProgramState :: ProgramState
emptyProgramState = ProgramState M.empty 1 0

type JvmGenM a = WriterT (DL.DList JvmInstr) (StateT ProgramState IO) a

insertNewToStore :: Ident -> JvmGenM Loc
insertNewToStore var = do
  loc <- gets nextLoc
  modify $ \st -> st {varsMap = M.insert var loc (varsMap st), nextLoc = loc + 1}
  pure loc

getVarNum :: Ident -> JvmGenM Loc
getVarNum name = do
  vm <- gets varsMap
  case M.lookup name vm of
    Nothing -> insertNewToStore name
    Just loc -> return loc

addInstructions :: DL.DList JvmInstr -> Int -> JvmGenM ()
addInstructions instructions maybeNewMax = do
  modify $ \st -> st {maxStackSize = max (maxStackSize st) maybeNewMax}
  tell instructions
