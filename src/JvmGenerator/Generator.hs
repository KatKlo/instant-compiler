module JvmGenerator.Generator where

import Control.Monad.State
import qualified Data.Map as M
import Grammar.AbsInstant
import System.FilePath (takeBaseName)

data JVMOp
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

instance Show JVMOp where
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

type Loc = Int

type Mem = M.Map Ident Loc

type Store = (Mem, Loc)

insertNewToStore :: Ident -> JvmGenM Loc
insertNewToStore var = do
  (st, loc) <- get
  put (M.insert var loc st, loc + 1)
  pure loc

getVarNum :: Ident -> JvmGenM Loc
getVarNum name = do
  (st, _) <- get
  case M.lookup name st of
    Nothing -> insertNewToStore name
    Just loc -> return loc

type JvmGenM a = StateT Store IO a

generateJvm :: String -> Program -> IO [String]
generateJvm filename prog = evalStateT (renderJvm filename prog) (M.empty, 1)

invocation :: String -> [String]
invocation filename =
  [ ".source " ++ filename,
    ".class public " ++ takeBaseName filename,
    ".super java/lang/Object",
    "",
    ".method public <init>()V",
    "  .limit stack 1",
    "  .limit locals 1",
    "  aload_0",
    "  invokespecial java/lang/Object/<init>()V",
    "  return",
    ".end method"
  ]

renderJvm :: String -> Program -> JvmGenM [String]
renderJvm filename prog = do
  ops <- renderProgram prog
  (_, loc) <- get
  let stackSize = findStackSize ops
  let funBody = fmap (("  " <>) . show) ops
  return $
    invocation filename
      ++ [ "",
           ".method public static main([Ljava/lang/String;)V",
           "  .limit stack " ++ show stackSize,
           "  .limit locals " ++ show loc
         ]
      ++ funBody
      ++ [ "  return",
           ".end method",
           ""
         ]

renderProgram :: Program -> JvmGenM [JVMOp]
renderProgram (Prog _ stmts) = concat <$> mapM renderStmt stmts

findStackSize :: [JVMOp] -> Int
findStackSize ops = maximum $ scanl fun 0 ops
  where
    fun :: Int -> JVMOp -> Int
    fun acc (ICONST _) = acc + 1
    fun acc (ISTORE _) = acc - 1
    fun acc (ILOAD _) = acc + 1
    fun acc ADD = acc - 1
    fun acc SUB = acc - 1
    fun acc MUL = acc - 1
    fun acc DIV = acc - 1
    fun acc SWAP = acc
    fun acc (GETSTATIC _ _) = acc + 1
    fun acc (INVOKEVIRTUAL _) = acc - 2

renderStmt :: Stmt -> JvmGenM [JVMOp]
renderStmt (SAss _ ident exp) = do
  (renderedExp, _) <- renderExp exp
  loc <- getVarNum ident
  return $ renderedExp ++ [ISTORE loc]
renderStmt (SExp _ exp) = do
  (renderedExp, _) <- renderExp exp
  return $ [GETSTATIC "java/lang/System/out" "Ljava/io/PrintStream;"] ++ renderedExp ++ [INVOKEVIRTUAL "java/io/PrintStream/println(I)V"]

renderExp :: Exp -> JvmGenM ([JVMOp], Int)
renderExp (ExpAdd _ exp1 exp2) = renderExpUniversal exp1 exp2 ADD
renderExp (ExpSub _ exp1 exp2) = renderExpUniversal exp1 exp2 SUB
renderExp (ExpMul _ exp1 exp2) = renderExpUniversal exp1 exp2 MUL
renderExp (ExpDiv _ exp1 exp2) = renderExpUniversal exp1 exp2 DIV
renderExp (ExpLit _ int) = pure ([ICONST int], 1)
renderExp (ExpVar _ (Ident ident)) = do
  loc <- getVarNum (Ident ident)
  pure ([ILOAD loc], 1)

renderExpUniversal :: Exp -> Exp -> JVMOp -> JvmGenM ([JVMOp], Int)
renderExpUniversal e1 e2 op = do
  (result1, stackSize1) <- renderExp e1
  (result2, stackSize2) <- renderExp e2
  if stackSize1 < stackSize2
    then return (result2 ++ result1 ++ renderOpForSwapped op, stackSize2)
    else return (result1 ++ result2 ++ [op], max stackSize1 $ stackSize2 + 1)

renderOpForSwapped :: JVMOp -> [JVMOp]
renderOpForSwapped ADD = [ADD]
renderOpForSwapped MUL = [MUL]
renderOpForSwapped op = [SWAP, op]
