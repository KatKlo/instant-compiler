module LlvmGenerator.Types where

data Op
  = Addr String
  | Lit Integer

data LlvmInstr
  = ALLOC Op
  | STORE Op Op
  | LOAD Op Op
  | ADD Op Op Op
  | SUB Op Op Op
  | MUL Op Op Op
  | DIV Op Op Op
  | PRINT Op
  
type LlvmInstrConstructor = Op -> Op -> Op -> LlvmInstr

instance Show Op where
  show (Addr s) = "%" ++ s
  show (Lit n) = show n

instance Show LlvmInstr where
  show (ALLOC var) = "  " ++ show var ++ " = alloca i32"
  show (STORE val var) = "  " ++ "store i32 " ++ show val ++ ", i32* " ++ show var
  show (LOAD addr var) = "  " ++ show addr ++ " = load i32, i32* " ++ show var
  show (ADD a b c) = "  " ++ show c ++ " = add i32 " ++ show a ++ ", " ++ show b
  show (SUB a b c) = "  " ++ show c ++ " = sub i32 " ++ show a ++ ", " ++ show b
  show (MUL a b c) = "  " ++ show c ++ " = mul i32 " ++ show a ++ ", " ++ show b
  show (DIV a b c) = "  " ++ show c ++ " = sdiv i32 " ++ show a ++ ", " ++ show b
  show (PRINT a) = "  " ++ "call void @printInt(i32 " ++ show a ++ ")"
