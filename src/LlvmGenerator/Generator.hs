module LlvmGenerator.Generator where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Grammar.AbsInstant
import LlvmGenerator.Types
import qualified Data.DList as DL

insertNewVar :: Ident -> LlvmGenM Ref
insertNewVar var = do
  ((st, varNo), tmpNo) <- get
  let ref = Addr $ "var" ++ show varNo
  put ((M.insert var ref st, varNo + 1), tmpNo)
  tell (DL.singleton (show $ ALLOC ref))
  pure ref

getVar :: Ident -> LlvmGenM Ref
getVar name = do
  ((st, _), _) <- get
  case M.lookup name st of
    Nothing -> insertNewVar name
    Just s -> pure s

getTmp :: LlvmGenM Ref
getTmp = do
  (varI, tmpNo) <- get
  put (varI, tmpNo + 1)
  pure . Addr $ "tmp" ++ show tmpNo

generateLlvm :: Program -> IO [String]
generateLlvm prog = do
  xd <- evalStateT (execWriterT (renderLlvm prog)) ((M.empty, 0), 0)
  return (DL.toList xd)

renderInvocation :: LlvmGenM ()
renderInvocation =
  tell
    (DL.fromList [ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
      "",
      "declare i32 @printf(i8*, ...)",
      "",
      "define void @printInt(i32 %x) {",
      "  %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
      "  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
      "  ret void",
      "}"
    ])

renderLlvm :: Program -> LlvmGenM ()
renderLlvm prog = do
  renderInvocation
  tell (DL.fromList ["", "define i32 @main() {"])
  renderProgram prog
  tell (DL.fromList ["  ret i32 0", "}", ""])

renderProgram :: Program -> LlvmGenM ()
renderProgram (Prog _ stmts) = foldr ((>>) . renderStmt) (return ()) stmts

renderStmt :: Stmt -> LlvmGenM ()
renderStmt (SAss _ ident exp) = do
  valRef <- renderExp exp
  varRef <- getVar ident
  tell (DL.singleton (show $ STORE valRef varRef))
renderStmt (SExp _ exp) = do
  valRef <- renderExp exp
  tell (DL.singleton (show $ PRINT valRef))

renderExp :: Exp -> LlvmGenM Ref
renderExp (ExpAdd _ e1 e2) = renderExpUniversal e1 e2 ADD
renderExp (ExpSub _ e1 e2) = renderExpUniversal e1 e2 SUB
renderExp (ExpMul _ e1 e2) = renderExpUniversal e1 e2 MUL
renderExp (ExpDiv _ e1 e2) = renderExpUniversal e1 e2 DIV
renderExp (ExpLit _ n) = return $ Lit n
renderExp (ExpVar _ name) = do
  varRef <- getVar name
  tmpRef <- getTmp
  tell (DL.singleton (show $ LOAD tmpRef varRef))
  return tmpRef

type LlvmOpConstructor = Ref -> Ref -> Ref -> LlVMOp

renderExpUniversal :: Exp -> Exp -> LlvmOpConstructor -> LlvmGenM Ref
renderExpUniversal e1 e2 op = do
  valRef1 <- renderExp e1
  valRef2 <- renderExp e2
  tmpRef <- getTmp
  tell (DL.singleton (show $ op valRef1 valRef2 tmpRef))
  return tmpRef
