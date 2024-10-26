module GeneratorJVM where

import qualified AbsInstant
import AbsInstant (Program(..), Stmt(..), Exp(..), Ident(..))

type VarMap = [(String, Int)]

updateMaxStackDepth :: Int -> Int -> Int -> Int
updateMaxStackDepth d1 d2 maxDepth =
  let new = max d1 d2
  in if new > maxDepth
     then new
     else maxDepth

generateJVM :: AbsInstant.Program -> String -> String
generateJVM (Prog stmts) baseName =
  let (code, _, varMap, stackDepth, maxStackDepth) = foldl generateStmt ("", 0, [], 0, 0) stmts
  in ".class public " ++ baseName ++ "\n" ++
     ".super java/lang/Object\n\n" ++
     ".method public static main([Ljava/lang/String;)V\n" ++
     ".limit stack " ++ show(maxStackDepth) ++ "\n" ++
     ".limit locals " ++ show((length varMap) + 1) ++ "\n" ++
     code ++
     "  return\n" ++
     ".end method\n"


generateStmt :: (String, Int, VarMap, Int, Int) -> AbsInstant.Stmt -> (String, Int, VarMap, Int, Int)

generateStmt (code, localVarIndex, varMap, stackDepth, maxStackDepth) (SAss (Ident ident) exp) =
  let (expCode, newReg, newStackDepth, newMaxStackDepth) = generateExp localVarIndex varMap (stackDepth + 1) maxStackDepth exp
      varIndex = case lookup ident varMap of
                   Just index -> index
                   Nothing    -> localVarIndex
      storeCode = if varIndex <= 3
                  then "  istore_" ++ show varIndex ++ "\n"
                  else "  istore " ++ show varIndex ++ "\n"
      (newVarMap, newLocalVarIndex) = if lookup ident varMap == Nothing
                  then ((ident, varIndex) : varMap, newReg + 1)
                  else (varMap, newReg)
  in (code ++ expCode ++ storeCode, newLocalVarIndex, newVarMap, newStackDepth - 1, newMaxStackDepth)

generateStmt (code, reg, varMap, stackDepth, maxStackDepth) (SExp exp) =
  let (expCode, newReg, newStackDepth, newMaxStackDepth) = generateExp reg varMap (stackDepth + 2) maxStackDepth exp
  in (code ++ "  getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++ expCode ++
               "  invokevirtual java/io/PrintStream/println(I)V\n", newReg, varMap, newStackDepth - 2, newMaxStackDepth)


generateExp :: Int -> VarMap -> Int -> Int -> AbsInstant.Exp -> (String, Int, Int, Int)

generateExp reg varMap stackDepth maxStackDepth (ExpAdd e1 e2) =
  let (code1, reg1, stackDepth1, maxStackDepth1) = generateExp reg varMap (stackDepth + 1) maxStackDepth e1
      (code2, reg2, stackDepth2, maxStackDepth2) = generateExp reg1 varMap (stackDepth + 1) maxStackDepth e2
      newMaxStackDepth = updateMaxStackDepth stackDepth1 stackDepth2 maxStackDepth
  in (code1 ++ code2 ++ "  iadd\n", reg2, stackDepth - 1, newMaxStackDepth)

generateExp reg varMap stackDepth maxStackDepth (ExpSub e1 e2) =
  let (code1, reg1, stackDepth1, maxStackDepth1) = generateExp reg varMap (stackDepth + 1) maxStackDepth e1
      (code2, reg2, stackDepth2, maxStackDepth2) = generateExp reg1 varMap (stackDepth + 1) maxStackDepth e2
      newMaxStackDepth = updateMaxStackDepth stackDepth1 stackDepth2 maxStackDepth
  in (code1 ++ code2 ++ "  isub\n", reg2, stackDepth - 1, newMaxStackDepth)

generateExp reg varMap stackDepth maxStackDepth (ExpMul e1 e2) =
  let (code1, reg1, stackDepth1, maxStackDepth1) = generateExp reg varMap (stackDepth + 1) maxStackDepth e1
      (code2, reg2, stackDepth2, maxStackDepth2) = generateExp reg1 varMap (stackDepth + 1) maxStackDepth e2
      newMaxStackDepth = updateMaxStackDepth stackDepth1 stackDepth2 maxStackDepth
  in (code1 ++ code2 ++ "  imul\n", reg2, stackDepth - 1, newMaxStackDepth)

generateExp reg varMap stackDepth maxStackDepth (ExpDiv e1 e2) =
  let (code1, reg1, stackDepth1, maxStackDepth1) = generateExp reg varMap (stackDepth + 1) maxStackDepth e1
      (code2, reg2, stackDepth2, maxStackDepth2) = generateExp reg1 varMap (stackDepth + 1) maxStackDepth e2
      newMaxStackDepth = updateMaxStackDepth stackDepth1 stackDepth2 maxStackDepth
  in (code1 ++ code2 ++ "  idiv\n", reg2, stackDepth - 1, newMaxStackDepth)

generateExp reg _ stackDepth maxStackDepth (ExpLit n) =
  let newMaxStackDepth = max stackDepth maxStackDepth
  in case () of
    _ | n >= -1 && n <= 5    -> ("  iconst_" ++ show n ++ "\n", reg, stackDepth, newMaxStackDepth)
      | n >= -128 && n <= 127 -> ("  bipush " ++ show n ++ "\n", reg, stackDepth, newMaxStackDepth)
      | n >= -32768 && n <= 32767 -> ("  sipush " ++ show n ++ "\n", reg, stackDepth, newMaxStackDepth)
      | otherwise             -> ("  ldc " ++ show n ++ "\n", reg, stackDepth, newMaxStackDepth)


generateExp reg varMap stackDepth maxStackDepth (ExpVar (Ident ident)) =
  let newMaxStackDepth = max stackDepth maxStackDepth
  in case lookup ident varMap of
    Just index
      | index >= 0 && index <= 3 -> ("  iload_" ++ show index ++ "\n", reg, stackDepth, newMaxStackDepth)
      | otherwise -> ("  iload " ++ show index ++ "\n", reg, stackDepth, newMaxStackDepth)
    Nothing -> error $ "Variable " ++ ident ++ " not found."

