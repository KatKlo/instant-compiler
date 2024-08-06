module JvmGenerator.Generator where

import Control.Monad.State
import Control.Monad.Writer
import Grammar.AbsInstant
import System.FilePath (takeBaseName)
import JvmGenerator.Types
import JvmGenerator.Monad
import qualified Data.DList as DL

generateJvm :: String -> Program -> IO (DL.DList String)
generateJvm filename prog = do
  (jvmInstructions, endState) <- runStateT (execWriterT (renderProgram prog)) emptyProgramState
  let locsNo = nextLoc endState
  let stackSize = maxStackSize endState
  let funBody = DL.map (("  " <>) . show) jvmInstructions
  let startOfFile = startJvmFile filename stackSize locsNo
  return $ DL.concat [startOfFile, funBody, endJvmFile]

startJvmFile :: String -> Int -> Int -> DL.DList String
startJvmFile filename stackSize locsNo = DL.fromList [
    ".source " ++ filename,
    ".class public " ++ takeBaseName filename,
    ".super java/lang/Object",
    "",
    ".method public <init>()V",
    "  .limit stack 1",
    "  .limit locals 1",
    "  aload_0",
    "  invokespecial java/lang/Object/<init>()V",
    "  return",
    ".end method",
    "",
    ".method public static main([Ljava/lang/String;)V",
    "  .limit stack " ++ show stackSize,
    "  .limit locals " ++ show locsNo
  ]

endJvmFile :: DL.DList String
endJvmFile = DL.fromList ["  return", ".end method", ""]

renderProgram :: Program -> JvmGenM ()
renderProgram (Prog _ stmts) = foldr ((>>) . renderStmt) (return ()) stmts

renderStmt :: Stmt -> JvmGenM ()
renderStmt (SAss _ ident expr) = do
  (renderedExpr, stackSize) <- renderExpr expr
  addInstructions renderedExpr stackSize
  loc <- getVarNum ident
  tell $ DL.singleton (ISTORE loc)
renderStmt (SExp _ expr) = do
  tell $ DL.singleton (GETSTATIC "java/lang/System/out" "Ljava/io/PrintStream;")
  (renderedExpr, stackSize) <- renderExpr expr
  addInstructions renderedExpr (stackSize + 1)
  tell $ DL.singleton (INVOKEVIRTUAL "java/io/PrintStream/println(I)V")

renderExpr :: Exp -> JvmGenM (DL.DList JvmInstr, Int)
renderExpr (ExpAdd _ e1 e2) = renderExprUniversal e1 e2 ADD
renderExpr (ExpSub _ e1 e2) = renderExprUniversal e1 e2 SUB
renderExpr (ExpMul _ e1 e2) = renderExprUniversal e1 e2 MUL
renderExpr (ExpDiv _ e1 e2) = renderExprUniversal e1 e2 DIV
renderExpr (ExpLit _ int) = pure (DL.singleton (ICONST int), 1)
renderExpr (ExpVar _ (Ident ident)) = do
  loc <- getVarNum (Ident ident)
  pure (DL.singleton (ILOAD loc), 1)

renderExprUniversal :: Exp -> Exp -> JvmInstr -> JvmGenM (DL.DList JvmInstr, Int)
renderExprUniversal e1 e2 op = do
  (result1, stackSize1) <- renderExpr e1
  (result2, stackSize2) <- renderExpr e2
  if stackSize1 < stackSize2
    then return (DL.concat [result2, result1, renderOpForSwapped op], stackSize2)
    else return (DL.concat [result1, result2, DL.singleton op], max stackSize1 (stackSize2 + 1))

renderOpForSwapped :: JvmInstr -> DL.DList JvmInstr
renderOpForSwapped ADD = DL.singleton ADD
renderOpForSwapped MUL = DL.singleton MUL
renderOpForSwapped op = DL.fromList [SWAP, op]
