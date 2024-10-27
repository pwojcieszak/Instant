module GeneratorLLVM where

import qualified AbsInstant
import AbsInstant (Program(..), Stmt(..), Exp(..), Ident(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

type VarMap = [String]

isAllocated :: String -> VarMap -> Bool
isAllocated ident varMap = ident `elem` varMap

generateLLVM :: AbsInstant.Program -> Text
generateLLVM (Prog stmts) = 
  let (code, _, _) = foldl generateStmt (T.empty, 0, []) stmts
  in (T.append (T.pack "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\n")
             (T.append (T.pack "declare i32 @printf(i8*, ...) #1\n\n")
             (T.append (T.pack "define i32 @main() {\n")
             (T.append code (T.pack "  ret i32 0\n}\n")))))

generateStmt :: (Text, Int, VarMap) -> AbsInstant.Stmt -> (Text, Int, VarMap)
generateStmt (code, reg, varMap) (SAss (Ident ident) (ExpLit l)) =
  let allocCode = if isAllocated ident varMap
                  then T.empty
                  else T.pack ("  %" ++ ident ++ " = alloca i32\n")
      storeCode = T.pack ("  store i32 " ++ show l ++ ", i32* %" ++ ident ++ "\n")
      newVarMap = if isAllocated ident varMap then varMap else ident : varMap
  in (T.append code (T.append allocCode storeCode), reg, newVarMap)

generateStmt (code, reg, varMap) (SAss (Ident ident) exp) =
  let (expCode, regAfterExp) = generateExp reg exp
      allocCode = if isAllocated ident varMap
                  then T.empty
                  else T.pack ("  %" ++ ident ++ " = alloca i32\n")
      storeCode = T.pack ("  store i32 %r" ++ show (regAfterExp - 1) ++ ", i32* %" ++ ident ++ "\n")
      newVarMap = if isAllocated ident varMap then varMap else ident : varMap
  in (T.append code (T.append allocCode (T.append expCode storeCode)), regAfterExp, newVarMap)

generateStmt (code, reg, varMap) (SExp (ExpLit l)) =
  (T.append code 
   (T.pack $ "  call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 " ++ show l ++ ")\n"), 
   reg, varMap)

generateStmt (code, reg, varMap) (SExp exp) =
  let (expCode, newReg) = generateExp reg exp
      printfCall = T.pack $ "  call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %r" ++ show (newReg - 1) ++ ")\n"
  in (T.append code (T.append expCode printfCall), newReg, varMap)

generateExp :: Int -> AbsInstant.Exp -> (Text, Int)
generateExp reg (ExpAdd (ExpLit l1) (ExpLit l2)) =
  (T.pack $ "  %r" ++ show reg ++ " = add i32 " ++ show l1 ++ ", " ++ show l2 ++ "\n", reg + 1)
generateExp reg (ExpAdd (ExpLit l) e) =
  let (code, reg1) = generateExp reg e
  in (T.append code 
      (T.pack $ "  %r" ++ show reg1 ++ " = add i32 " ++ show l ++ ", %r" ++ show (reg1 - 1) ++ "\n"), 
      reg1 + 1)
generateExp reg (ExpAdd e (ExpLit l)) =
  let (code, reg1) = generateExp reg e
  in (T.append code 
      (T.pack $ "  %r" ++ show reg1 ++ " = add i32 %r" ++ show (reg1 - 1) ++ ", " ++ show l ++ "\n"), 
      reg1 + 1)
generateExp reg (ExpAdd e1 e2) =
  let (code1, reg1) = generateExp reg e1
      (code2, reg2) = generateExp reg1 e2
      additionCode = T.pack $ "  %r" ++ show reg2 ++ " = add i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (T.append code1 (T.append code2 additionCode), reg2 + 1)

generateExp reg (ExpSub (ExpLit l1) (ExpLit l2)) =
  (T.pack $ "  %r" ++ show reg ++ " = sub i32 " ++ show l1 ++ ", " ++ show l2 ++ "\n", reg + 1)
generateExp reg (ExpSub (ExpLit l) e) =
  let (code, reg1) = generateExp reg e
  in (T.append code 
      (T.pack $ "  %r" ++ show reg1 ++ " = sub i32 " ++ show l ++ ", %r" ++ show (reg1 - 1) ++ "\n"), 
      reg1 + 1)
generateExp reg (ExpSub e (ExpLit l)) =
  let (code, reg1) = generateExp reg e
  in (T.append code 
      (T.pack $ "  %r" ++ show reg1 ++ " = sub i32 %r" ++ show (reg1 - 1) ++ ", " ++ show l ++ "\n"), 
      reg1 + 1)
generateExp reg (ExpSub e1 e2) =
  let (code1, reg1) = generateExp reg e1
      (code2, reg2) = generateExp reg1 e2
      subtractionCode = T.pack $ "  %r" ++ show reg2 ++ " = sub i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (T.append code1 (T.append code2 subtractionCode), reg2 + 1)

generateExp reg (ExpMul (ExpLit l1) (ExpLit l2)) =
  (T.pack $ "  %r" ++ show reg ++ " = mul i32 " ++ show l1 ++ ", " ++ show l2 ++ "\n", reg + 1)
generateExp reg (ExpMul (ExpLit l) e) =
  let (code, reg1) = generateExp reg e
  in (T.append code 
      (T.pack $ "  %r" ++ show reg1 ++ " = mul i32 " ++ show l ++ ", %r" ++ show (reg1 - 1) ++ "\n"), 
      reg1 + 1)
generateExp reg (ExpMul e (ExpLit l)) =
  let (code, reg1) = generateExp reg e
  in (T.append code 
      (T.pack $ "  %r" ++ show reg1 ++ " = mul i32 %r" ++ show (reg1 - 1) ++ ", " ++ show l ++ "\n"), 
      reg1 + 1)
generateExp reg (ExpMul e1 e2) =
  let (code1, reg1) = generateExp reg e1
      (code2, reg2) = generateExp reg1 e2
      multiplicationCode = T.pack $ "  %r" ++ show reg2 ++ " = mul i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (T.append code1 (T.append code2 multiplicationCode), reg2 + 1)

generateExp reg (ExpDiv (ExpLit l1) (ExpLit l2)) =
  (T.pack $ "  %r" ++ show reg ++ " = sdiv i32 " ++ show l1 ++ ", " ++ show l2 ++ "\n", reg + 1)
generateExp reg (ExpDiv (ExpLit l) e) =
  let (code, reg1) = generateExp reg e
  in (T.append code 
      (T.pack $ "  %r" ++ show reg1 ++ " = sdiv i32 " ++ show l ++ ", %r" ++ show (reg1 - 1) ++ "\n"), 
      reg1 + 1)
generateExp reg (ExpDiv e (ExpLit l)) =
  let (code, reg1) = generateExp reg e
  in (T.append code 
      (T.pack $ "  %r" ++ show reg1 ++ " = sdiv i32 %r" ++ show (reg1 - 1) ++ ", " ++ show l ++ "\n"), 
      reg1 + 1)
generateExp reg (ExpDiv e1 e2) =
  let (code1, reg1) = generateExp reg e1
      (code2, reg2) = generateExp reg1 e2
      divisionCode = T.pack $ "  %r" ++ show reg2 ++ " = sdiv i32 %r" ++ show (reg1 - 1) ++ ", %r" ++ show (reg2 - 1) ++ "\n"
  in (T.append code1 (T.append code2 divisionCode), reg2 + 1)

generateExp reg (ExpLit n) =
  (T.pack $ "  " ++ show n ++ "\n", reg)

generateExp reg (ExpVar (Ident ident)) =
  let loadCode = T.pack $ "  %r" ++ show reg ++ " = load i32, i32* %" ++ ident ++ "\n"
  in (loadCode, reg + 1)
