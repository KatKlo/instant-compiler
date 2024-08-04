module JvmGenerator.Types where

data JvmInstr
  = ICONST Integer
  | ISTORE Int
  | ILOAD Int
  | ADD
  | SUB
  | MUL
  | DIV
  | SWAP
  | GETSTATIC String String
  | INVOKEVIRTUAL String

instance Show JvmInstr where
  show (ICONST n)
    | n <= 5 = "iconst_" ++ show n
    | n <= 127 = "bipush " ++ show n
    | n <= 32767 = "sipush " ++ show n
    | otherwise = "ldc " ++ show n
  show (ISTORE n)
    | n <= 3 = "istore_" ++ show n
    | otherwise = "istore " ++ show n
  show (ILOAD n)
    | n <= 3 = "iload_" ++ show n
    | otherwise = "iload " ++ show n
  show ADD = "iadd"
  show SUB = "isub"
  show MUL = "imul"
  show DIV = "idiv"
  show SWAP = "swap"
  show (GETSTATIC s1 s2) = "getstatic " ++ s1 ++ " " ++ s2
  show (INVOKEVIRTUAL s) = "invokevirtual " ++ " " ++ s
