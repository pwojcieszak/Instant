module GeneratorLLVM where

import qualified AbsInstant
import AbsInstant (Program(..), Stmt(..), Exp(..), Ident(..))

type VarMap = [String]

isAllocated :: String -> VarMap -> Bool
isAllocated ident varMap = ident `elem` varMap

generateLLVM :: AbsInstant.Program -> String
generateLLVM (Prog stmts) = 
  let (code, _, _) = foldl generateStmt ([], 0, []) stmts
  in "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\n" ++
     "declare i32 @printf(i8*, ...) #1\n\n" ++
     "define i32 @main() {\n" ++
     concat (reverse code) ++
     "  ret i32 0\n" ++
     "}\n"

generateStmt :: ([String], Int, VarMap) -> AbsInstant.Stmt -> ([String], Int, VarMap)
generateStmt (code, reg, varMap) (SAss (Ident ident) (ExpLit l)) =
  let allocCode = if isAllocated ident varMap
                  then ""
                  else "  %" ++ ident ++ " = alloca i32\n"
      storeCode = "  store i32 " ++ show l ++ ", i32* %" ++ ident ++ "\n"
      newVarMap = if isAllocated ident varMap then varMap else ident : varMap
  in (storeCode : (allocCode : code), reg, newVarMap)

generateStmt (code, reg, varMap) (SAss (Ident ident) exp) =
  let allocCode = if isAllocated ident varMap
                  then ""
                  else "  %" ++ ident ++ " = alloca i32\n"
      (expCode, regAfterExp) = generateExp (allocCode : code) reg exp
      storeCode = "  store i32 %r" ++ show (regAfterExp - 1) ++ ", i32* %" ++ ident ++ "\n"
      newVarMap = if isAllocated ident varMap then varMap else ident : varMap
  in (storeCode : expCode, regAfterExp, newVarMap)

generateStmt (code, reg, varMap) (SExp (ExpLit l)) =
  let printCall = "  call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 " ++ show l ++ ")\n"
  in (printCall : code, reg, varMap)

generateStmt (code, reg, varMap) (SExp exp) =
  let (expCode, newReg) = generateExp code reg exp
      printCall = "  call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %r" ++ show (newReg - 1) ++ ")\n"
  in (printCall : expCode, newReg, varMap)


generateExp :: [String] -> Int -> AbsInstant.Exp -> ([String], Int)
generateExp code reg (ExpAdd e1 e2) = generateBinaryOp code reg e1 e2 "add"
generateExp code reg (ExpSub e1 e2) = generateBinaryOp code reg e1 e2 "sub"
generateExp code reg (ExpMul e1 e2) = generateBinaryOp code reg e1 e2 "mul"
generateExp code reg (ExpDiv e1 e2) = generateBinaryOp code reg e1 e2 "sdiv"
generateExp code reg (ExpLit n) =
  let litCode = "  " ++ show n ++ "\n"
  in (litCode : code, reg + 1)

generateExp code reg (ExpVar (Ident ident)) =
  let loadCode = "  %r" ++ show reg ++ " = load i32, i32* %" ++ ident ++ "\n"
  in (loadCode : code, reg + 1)

generateBinaryOp :: [String] -> Int -> AbsInstant.Exp -> AbsInstant.Exp -> String -> ([String], Int)
generateBinaryOp code reg (ExpLit l1) (ExpLit l2) op =
  let binOpCode = "  %r" ++ show reg ++ " = " ++ op ++ " i32 " ++ show (l1) ++ ", " ++ show (l2) ++ "\n"
  in (binOpCode : code, reg + 1)
generateBinaryOp code reg (ExpLit l) e2 op =
  let (code1, reg1) = generateExp code reg e2
      binOpCode = "  %r" ++ show reg1 ++ " = " ++ op ++ " i32 " ++ show (l) ++ ", %r" ++ show (reg1 - 1) ++ "\n"
  in (binOpCode : code1, reg1 + 1)
generateBinaryOp code reg e1 (ExpLit l) op =
  let (code1, reg1) = generateExp code reg e1
      binOpCode = "  %r" ++ show reg1 ++ " = " ++ op ++ " i32 %r" ++ show (reg1 - 1) ++ ", " ++ show (l) ++ "\n"
  in (binOpCode : code1, reg1 + 1)
generateBinaryOp code reg e1 e2 op =
  let (code1, reg1) = generateExp code reg e1
      (code2, reg2) = generateExp code1 reg1 e2
      binOpCode = "  %r" ++ show reg2 ++ " = " ++ op ++ " i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (binOpCode : code2, reg2 + 1)
