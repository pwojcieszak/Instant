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
      storeCode = "  store i32 " ++ show (l) ++ ", i32* %" ++ ident ++ "\n"
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
generateExp code reg (ExpAdd (ExpLit l1) (ExpLit l2)) =
  let addCode = "  %r" ++ show reg ++ " = add i32 " ++ show l1 ++ ", " ++ show l2 ++ "\n"
  in (addCode : code, reg + 1)
generateExp code reg (ExpAdd (ExpLit l) e) =
  let (expCode, reg1) = generateExp code reg e
      addCode = "  %r" ++ show reg1 ++ " = add i32 " ++ show l ++ ", %r" ++ show (reg1 - 1) ++ "\n"
  in (addCode : expCode, reg1 + 1)
generateExp code reg (ExpAdd e (ExpLit l)) =
  let (expCode, reg1) = generateExp code reg e
      addCode = "  %r" ++ show reg1 ++ " = add i32 %r" ++ show (reg1 - 1) ++ ", " ++ show l ++ "\n"
  in (addCode : code, reg1 + 1)
generateExp code reg (ExpAdd e1 e2) =
  let (code2, reg2) = generateExp code reg e2
      (code1, reg1) = generateExp code2 reg2 e1
      additionCode = "  %r" ++ show reg2 ++ " = add i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (additionCode : code1, reg2 + 1)

generateExp code reg (ExpSub (ExpLit l1) (ExpLit l2)) =
  let subCode = "  %r" ++ show reg ++ " = sub i32 " ++ show l1 ++ ", " ++ show l2 ++ "\n"
  in (subCode : code, reg + 1)
generateExp code reg (ExpSub (ExpLit l) e) =
  let (code1, reg1) = generateExp code reg e
      subCode = "  %r" ++ show reg1 ++ " = sub i32 " ++ show l ++ ", %r" ++ show (reg1 - 1) ++ "\n"
  in (subCode : code1, reg1 + 1)
generateExp code reg (ExpSub e (ExpLit l)) =
  let (code1, reg1) = generateExp code reg e
      subCode = "  %r" ++ show reg1 ++ " = sub i32 %r" ++ show (reg1 - 1) ++ ", " ++ show l ++ "\n"
  in (subCode : code1, reg1 + 1)
generateExp code reg (ExpSub e1 e2) =
  let (code2, reg2) = generateExp code reg e2
      (code1, reg1) = generateExp code2 reg2 e1
      subCode = "  %r" ++ show reg2 ++ " = sub i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (subCode : code1, reg2 + 1)

generateExp code reg (ExpMul (ExpLit l1) (ExpLit l2)) =
  let mulCode = "  %r" ++ show reg ++ " = mul i32 " ++ show l1 ++ ", " ++ show l2 ++ "\n"
  in (mulCode : code, reg + 1)
generateExp code reg (ExpMul (ExpLit l) e) =
  let (code1, reg1) = generateExp code reg e
      mulCode = "  %r" ++ show reg1 ++ " = mul i32 " ++ show l ++ ", %r" ++ show (reg1 - 1) ++ "\n"
  in (mulCode : code1, reg1 + 1)
generateExp code reg (ExpMul e (ExpLit l)) =
  let (code1, reg1) = generateExp code reg e
      mulCode = "  %r" ++ show reg1 ++ " = mul i32 %r" ++ show (reg1 - 1) ++ ", " ++ show l ++ "\n"
  in (mulCode : code1, reg1 + 1)
generateExp code reg (ExpMul e1 e2) =
  let (code2, reg2) = generateExp code reg e2
      (code1, reg1) = generateExp code2 reg2 e1
      mulCode = "  %r" ++ show reg2 ++ " = mul i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (mulCode : code1, reg2 + 1)

generateExp code reg (ExpDiv (ExpLit l1) (ExpLit l2)) =
  let divCode = "  %r" ++ show reg ++ " = sdiv i32 " ++ show l1 ++ ", " ++ show l2 ++ "\n"
  in (divCode : code, reg + 1)
generateExp code reg (ExpDiv (ExpLit l) e) =
  let (code1, reg1) = generateExp code reg e
      divCode = "  %r" ++ show reg1 ++ " = sdiv i32 " ++ show l ++ ", %r" ++ show (reg1 - 1) ++ "\n"
  in (divCode : code1, reg1 + 1)
generateExp code reg (ExpDiv e (ExpLit l)) =
  let (code1, reg1) = generateExp code reg e
      divCode = "  %r" ++ show reg1 ++ " = sdiv i32 %r" ++ show (reg1 - 1) ++ ", " ++ show l ++ "\n"
  in (divCode : code1, reg1 + 1)
generateExp code reg (ExpDiv e1 e2) =
  let (code2, reg2) = generateExp code reg e2
      (code1, reg1) = generateExp code2 reg2 e1
      divCode = "  %r" ++ show reg2 ++ " = sdiv i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (divCode : code1, reg2 + 1)


generateExp code reg (ExpLit n) =
  let litCode = "  " ++ show n ++ "\n"
  in (litCode : code, reg)

generateExp code reg (ExpVar (Ident ident)) =
  let loadCode = "  %r" ++ show reg ++ " = load i32, i32* %" ++ ident ++ "\n"
  in (loadCode : code, reg + 1)
