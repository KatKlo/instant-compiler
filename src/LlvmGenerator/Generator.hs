module LlvmGenerator.Generator where

import Control.Monad.State
import Control.Monad.Writer
import Grammar.AbsInstant
import LlvmGenerator.Types
import LlvmGenerator.Monad
import qualified Data.DList as DL

generateLlvm :: Program -> IO (DL.DList String)
generateLlvm prog = do
  llvmInstructions <- evalStateT (execWriterT (renderProgram prog)) emptyProgramState
  let funBody = DL.map (("  " <>) . show) llvmInstructions
  return $ DL.concat [startLlvmFile, funBody, endLlvmFile]

startLlvmFile :: DL.DList String
startLlvmFile = DL.fromList [
    "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
    "",
    "declare i32 @printf(i8*, ...)",
    "",
    "define void @printInt(i32 %x) {",
    "  %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
    "  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
    "  ret void",
    "}",
    "",
    "define i32 @main() {"
  ]

endLlvmFile :: DL.DList String
endLlvmFile = DL.fromList ["  ret i32 0", "}", ""]

renderProgram :: Program -> LlvmGenM ()
renderProgram (Prog _ stmts) = foldr ((>>) . renderStmt) (return ()) stmts

renderStmt :: Stmt -> LlvmGenM ()
renderStmt (SAss _ ident expr) = do
  valOp <- renderExpr expr
  varOp <- getVar ident
  tell $ DL.singleton (STORE valOp varOp)
renderStmt (SExp _ expr) = do
  valOp <- renderExpr expr
  tell $ DL.singleton (PRINT valOp)

renderExpr :: Exp -> LlvmGenM Op
renderExpr (ExpAdd _ e1 e2) = renderExprUniversal e1 e2 ADD
renderExpr (ExpSub _ e1 e2) = renderExprUniversal e1 e2 SUB
renderExpr (ExpMul _ e1 e2) = renderExprUniversal e1 e2 MUL
renderExpr (ExpDiv _ e1 e2) = renderExprUniversal e1 e2 DIV
renderExpr (ExpLit _ n) = return $ Lit n
renderExpr (ExpVar _ name) = do
  varOp <- getVar name
  tmpOp <- getTmp
  tell $ DL.singleton (LOAD tmpOp varOp)
  return tmpOp

renderExprUniversal :: Exp -> Exp -> LlvmInstrConstructor -> LlvmGenM Op
renderExprUniversal e1 e2 op = do
  valOp1 <- renderExpr e1
  valOp2 <- renderExpr e2
  tmpOp <- getTmp
  tell $ DL.singleton (op valOp1 valOp2 tmpOp)
  return tmpOp
