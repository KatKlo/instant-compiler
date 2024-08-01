module LlvmGenerator.Types where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.DList as DL
import Grammar.AbsInstant

data Ref
  = Addr String
  | Lit Integer

data LlVMOp
  = ALLOC Ref
  | STORE Ref Ref
  | LOAD Ref Ref
  | ADD Ref Ref Ref
  | SUB Ref Ref Ref
  | MUL Ref Ref Ref
  | DIV Ref Ref Ref
  | PRINT Ref

instance Show Ref where
  show (Addr s) = "%" ++ s
  show (Lit n) = show n

instance Show LlVMOp where
  show (ALLOC var) = "  " ++ show var ++ " = alloca i32"
  show (STORE val var) = "  " ++ "store i32 " ++ show val ++ ", i32* " ++ show var
  show (LOAD addr var) = "  " ++ show addr ++ " = load i32, i32* " ++ show var
  show (ADD a b c) = "  " ++ show c ++ " = add i32 " ++ show a ++ ", " ++ show b
  show (SUB a b c) = "  " ++ show c ++ " = sub i32 " ++ show a ++ ", " ++ show b
  show (MUL a b c) = "  " ++ show c ++ " = mul i32 " ++ show a ++ ", " ++ show b
  show (DIV a b c) = "  " ++ show c ++ " = sdiv i32 " ++ show a ++ ", " ++ show b
  show (PRINT a) = "  " ++ "call void @printInt(i32 " ++ show a ++ ")"

type Mem = M.Map Ident Ref

type Store = ((Mem, Int), Int)

type LlvmGenM a = WriterT (DL.DList String) (StateT Store IO) a
