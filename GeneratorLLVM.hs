module GeneratorLLVM where

import qualified AbsInstant
import AbsInstant (Program(..), Stmt(..), Exp(..), Ident(..)) -- Dodajemy odpowiednie importy

-- Funkcja do generowania kodu LLVM dla programu
generateLLVM :: AbsInstant.Program -> String
generateLLVM (Prog stmts) = 
  "define void @main() {\n" ++
  concatMap generateStmt stmts ++
  "  ret void\n" ++
  "}\n"

-- Funkcja do generowania kodu LLVM dla instrukcji
generateStmt :: AbsInstant.Stmt -> String
generateStmt (SAss ident exp) =
  "  " ++ show ident ++ " = " ++ generateExp exp ++ "\n"
generateStmt (SExp exp) =
  "  " ++ generateExp exp ++ "\n"

-- Funkcja do generowania kodu LLVM dla wyrażeń
generateExp :: AbsInstant.Exp -> String
generateExp (ExpAdd e1 e2) =
  let res = "add i32 " ++ generateExp e1 ++ ", " ++ generateExp e2
  in res 
generateExp (ExpSub e1 e2) =
  let res = "sub i32 " ++ generateExp e1 ++ ", " ++ generateExp e2
  in res
generateExp (ExpMul e1 e2) =
  let res = "mul i32 " ++ generateExp e1 ++ ", " ++ generateExp e2
  in res
generateExp (ExpDiv e1 e2) =
  let res = "sdiv i32 " ++ generateExp e1 ++ ", " ++ generateExp e2
  in res
generateExp (ExpLit n) =
  show n  
generateExp (ExpVar ident) =
  show ident 
