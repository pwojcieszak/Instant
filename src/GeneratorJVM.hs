module GeneratorJVM where

import qualified AbsInstant
import AbsInstant (Program(..), Stmt(..), Exp(..), Ident(..))

type VarMap = [(String, Int)]

generateJVM :: AbsInstant.Program -> String
generateJVM (Prog stmts) =
  let (code, _, _) = foldl generateStmt ("", 0, []) stmts
  in ".class public baz\n" ++
     ".super java/lang/Object\n\n" ++
     ".method public static main([Ljava/lang/String;)V\n" ++
     ".limit stack 100\n" ++      -- TODO dynamieczne limity
     ".limit locals 100\n" ++
     code ++
     "  return\n" ++
     ".end method\n"

generateStmt :: (String, Int, VarMap) -> AbsInstant.Stmt -> (String, Int, VarMap)
generateStmt (code, localVarIndex, varMap) (SAss (Ident ident) exp) =
  let (expCode, regAfterExp) = generateExp localVarIndex varMap exp
      varIndex = case lookup ident varMap of
                   Just index -> index
                   Nothing    -> localVarIndex
      storeCode = if varIndex <= 3
                  then "  istore_" ++ show varIndex ++ "\n"
                  else "  istore " ++ show varIndex ++ "\n"
      (newVarMap, newLocalVarIndex) = if lookup ident varMap == Nothing
                  then ((ident, varIndex) : varMap, localVarIndex + 1)
                  else (varMap, localVarIndex)
  in (code ++ expCode ++ storeCode, newLocalVarIndex, newVarMap)
generateStmt (code, reg, varMap) (SExp exp) =
  let (expCode, newReg) = generateExp reg varMap exp
  in (code ++ "  getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++ expCode ++
               "  invokevirtual java/io/PrintStream/println(I)V\n", newReg, varMap)

generateExp :: Int -> VarMap -> AbsInstant.Exp -> (String, Int)
generateExp reg varMap (ExpAdd e1 e2) =
  let (code1, reg1) = generateExp reg varMap e1
      (code2, reg2) = generateExp reg1 varMap e2
  in (code1 ++ code2 ++ "  iadd\n", reg2)
generateExp reg varMap (ExpSub e1 e2) =
  let (code1, reg1) = generateExp reg varMap e1
      (code2, reg2) = generateExp reg1 varMap e2
  in (code1 ++ code2 ++ "  isub\n", reg2)
generateExp reg varMap (ExpMul e1 e2) =
  let (code1, reg1) = generateExp reg varMap e1
      (code2, reg2) = generateExp reg1 varMap e2
  in (code1 ++ code2 ++ "  imul\n", reg2)
generateExp reg varMap (ExpDiv e1 e2) =
  let (code1, reg1) = generateExp reg varMap e1
      (code2, reg2) = generateExp reg1 varMap e2
  in (code1 ++ code2 ++ "  idiv\n", reg2)
generateExp reg _ (ExpLit n)
  | n >= -1 && n <= 5 = ("  iconst_" ++ show n ++ "\n", reg)
  | n >= -128 && n <= 127 = ("  bipush " ++ show n ++ "\n", reg)
  | n >= -32768 && n <= 32767 = ("  sipush " ++ show n ++ "\n", reg)
  | otherwise = ("  ldc " ++ show n ++ "\n", reg)
generateExp reg varMap (ExpVar (Ident ident)) =
  case lookup ident varMap of
    Just index
      | index >= 0 && index <= 3 -> ("  iload_" ++ show index ++ "\n", reg)
      | otherwise -> ("  iload " ++ show index ++ "\n", reg)
    Nothing -> error $ "Variable " ++ ident ++ " not found."

