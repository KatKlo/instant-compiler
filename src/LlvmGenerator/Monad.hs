module LlvmGenerator.Monad where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.DList as DL
import Grammar.AbsInstant
import LlvmGenerator.Types

type VariablesMap = M.Map Ident Op

data ProgramState = ProgramState {
  varsMap :: VariablesMap,
  varsNo :: Int,
  tmpsNo :: Int
}

emptyProgramState :: ProgramState
emptyProgramState = ProgramState M.empty 0 0

type LlvmGenM a = WriterT (DL.DList LlvmInstr) (StateT ProgramState IO) a

insertNewVar :: Ident -> LlvmGenM Op
insertNewVar var = do
  varNo <- gets varsNo
  let ref = Addr $ "var" ++ show varNo
  modify $ \st -> st {varsMap = M.insert var ref (varsMap st), varsNo = varNo + 1}
  tell $ DL.singleton (ALLOC ref)
  pure ref

getVar :: Ident -> LlvmGenM Op
getVar name = do
  st <- gets varsMap
  case M.lookup name st of
    Nothing -> insertNewVar name
    Just s -> pure s

getTmp :: LlvmGenM Op
getTmp = do
  tmpNo <- gets tmpsNo
  modify $ \st -> st {tmpsNo = tmpNo + 1}
  pure $ Addr ("tmp" ++ show tmpNo)
