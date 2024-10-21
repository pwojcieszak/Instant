module GeneratorLLVM where

import qualified AbsInstant
import AbsInstant (Program(..), Stmt(..), Exp(..), Ident(..))

type VarMap = [String]

isAllocated :: String -> VarMap -> Bool
isAllocated ident varMap = ident `elem` varMap

generateLLVM :: AbsInstant.Program -> String
generateLLVM (Prog stmts) = 
  let (code, _, _) = foldl generateStmt ("", 0, []) stmts
  in "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\n" ++
     "declare i32 @printf(i8*, ...) #1\n\n" ++
     "define i32 @main() {\n" ++
     code ++
     "  ret i32 0\n" ++
     "}\n"

generateStmt :: (String, Int, VarMap) -> AbsInstant.Stmt -> (String, Int, VarMap)
generateStmt (code, reg, varMap) (SAss (Ident ident) exp) =
  let (expCode, regAfterExp) = generateExp reg exp
      allocCode = if isAllocated ident varMap
                  then ""
                  else "  %" ++ ident ++ " = alloca i32\n"
      storeCode = "  store i32 %r" ++ show (regAfterExp - 1) ++ ", i32* %" ++ ident ++ "\n"
      newVarMap = if isAllocated ident varMap then varMap else ident : varMap
  in (code ++ allocCode ++ expCode ++ storeCode, regAfterExp, newVarMap)
generateStmt (code, reg, varMap) (SExp exp) =
  let (expCode, newReg) = generateExp reg exp
  in (code ++ expCode ++ "  call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %r" ++ show (newReg - 1) ++ ")\n", newReg, varMap)

generateExp :: Int -> AbsInstant.Exp -> (String, Int)
generateExp reg (ExpAdd e1 e2) =
  let (code1, reg1) = generateExp reg e1
      (code2, reg2) = generateExp reg1 e2
      additionCode = "  %r" ++ show reg2 ++ " = add i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (code1 ++ code2 ++ additionCode, reg2 + 1)
generateExp reg (ExpSub e1 e2) =
  let (code1, reg1) = generateExp reg e1
      (code2, reg2) = generateExp reg1 e2
      subtractionCode = "  %r" ++ show reg2 ++ " = sub i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (code1 ++ code2 ++ subtractionCode, reg2 + 1)
generateExp reg (ExpMul e1 e2) =
  let (code1, reg1) = generateExp reg e1
      (code2, reg2) = generateExp reg1 e2
      multiplicationCode = "  %r" ++ show reg2 ++ " = mul i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (code1 ++ code2 ++ multiplicationCode, reg2 + 1)
generateExp reg (ExpDiv e1 e2) =
  let (code1, reg1) = generateExp reg e1
      (code2, reg2) = generateExp reg1 e2
      divisionCode = "  %r" ++ show reg2 ++ " = sdiv i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (code1 ++ code2 ++ divisionCode, reg2 + 1)
generateExp reg (ExpLit n) =
  ("  %r" ++ show reg ++ " = add i32 0, " ++ show n ++ "\n", reg + 1)
generateExp reg (ExpVar (Ident ident)) =
  let loadCode = "  %r" ++ show reg ++ " = load i32, i32* %" ++ ident ++ "\n"
  in (loadCode, reg + 1)
