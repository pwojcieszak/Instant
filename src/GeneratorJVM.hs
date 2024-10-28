module GeneratorJVM where

import qualified AbsInstant
import AbsInstant (Program(..), Stmt(..), Exp(..), Ident(..))

type VarMap = [(String, Int)]

estimateDepth :: AbsInstant.Exp -> Int
estimateDepth (ExpLit _) = 1
estimateDepth (ExpVar _) = 1
estimateDepth (ExpAdd e1 e2) = 1 + max (estimateDepth e1) (estimateDepth e2)
estimateDepth (ExpMul e1 e2) = 1 + max (estimateDepth e1) (estimateDepth e2)
estimateDepth (ExpSub e1 e2) = 1 + max (estimateDepth e1) (estimateDepth e2)
estimateDepth (ExpDiv e1 e2) = 1 + max (estimateDepth e1) (estimateDepth e2)

isSimpleExp :: Exp -> Bool
isSimpleExp (ExpLit _) = True
isSimpleExp (ExpVar _) = True
isSimpleExp _ = False

generateJVM :: AbsInstant.Program -> String -> String
generateJVM (Prog stmts) baseName =
  let (code, _, varMap, maxStackSize) = foldl generateStmt ([], 1, [], 0) stmts
  in ".class public " ++ baseName ++ "\n" ++
     ".super java/lang/Object\n\n" ++
     ".method public static main([Ljava/lang/String;)V\n" ++
     ".limit stack " ++ show(maxStackSize) ++ "\n" ++
     ".limit locals " ++ show((length varMap) + 1) ++ "\n" ++
     concat (reverse code) ++
     "  return\n" ++
     ".end method\n"


generateStmt :: ([String], Int, VarMap, Int) -> AbsInstant.Stmt -> ([String], Int, VarMap, Int)
generateStmt (code, localVarIndex, varMap, maxStackSize) (SAss (Ident ident) exp) =
  let (expCode, newReg, _, newMaxStackSize) = generateExp code localVarIndex varMap 1 maxStackSize exp
      varIndex = case lookup ident varMap of
                   Just index -> index
                   Nothing    -> localVarIndex
      storeCode = if varIndex <= 3
                  then "  istore_" ++ show varIndex ++ "\n"
                  else "  istore " ++ show varIndex ++ "\n"
      (newVarMap, newLocalVarIndex) = if lookup ident varMap == Nothing
                  then ((ident, varIndex) : varMap, newReg + 1)
                  else (varMap, newReg)
  in (storeCode : expCode, newLocalVarIndex, newVarMap, newMaxStackSize)

generateStmt (code, reg, varMap, maxStackSize) (SExp exp) =
  let printHead = "  getstatic java/lang/System/out Ljava/io/PrintStream;\n"
      (expCode, newReg, _, newMaxStackSize) = 
          if isSimpleExp exp 
          then generateExp (printHead : code) reg varMap 2 maxStackSize exp
          else generateExp code reg varMap 1 maxStackSize exp
      printTail = "  invokevirtual java/io/PrintStream/println(I)V\n"
      finalCode = if isSimpleExp exp 
                  then printTail : expCode
                  else printTail : (printHead ++ "  swap\n") : expCode
  in (finalCode, newReg, varMap, newMaxStackSize)


generateExp :: [String] -> Int -> VarMap -> Int -> Int -> AbsInstant.Exp -> ([String], Int, Int, Int)
generateExp code reg varMap stackSize maxStackSize (ExpAdd e1 e2) = generateBinaryOp code reg varMap stackSize maxStackSize e1 e2 "iadd"
generateExp code reg varMap stackSize maxStackSize (ExpSub e1 e2) = generateBinaryOp code reg varMap stackSize maxStackSize e1 e2 "isub"
generateExp code reg varMap stackSize maxStackSize (ExpMul e1 e2) = generateBinaryOp code reg varMap stackSize maxStackSize e1 e2 "imul"
generateExp code reg varMap stackSize maxStackSize (ExpDiv e1 e2) = generateBinaryOp code reg varMap stackSize maxStackSize e1 e2 "idiv"
generateExp code reg _ stackSize maxStackSize (ExpLit n) =
  let newMaxStackSize = max stackSize maxStackSize
      litCode = case () of
        _ | n >= -1 && n <= 5    -> "  iconst_" ++ show n ++ "\n"
          | n >= -128 && n <= 127 -> "  bipush " ++ show n ++ "\n"
          | n >= -32768 && n <= 32767 -> "  sipush " ++ show n ++ "\n"
          | otherwise             -> "  ldc " ++ show n ++ "\n"
  in (litCode : code, reg, stackSize, newMaxStackSize)

generateExp code reg varMap stackSize maxStackSize (ExpVar (Ident ident)) =
  let newMaxStackSize = max stackSize maxStackSize
      varCode = case lookup ident varMap of
        Just index
          | index >= 0 && index <= 3 -> "  iload_" ++ show index ++ "\n"
          | otherwise -> "  iload " ++ show index ++ "\n"
        Nothing -> error $ "Variable " ++ ident ++ " not found."
  in (varCode : code, reg, stackSize, newMaxStackSize)

generateBinaryOp :: [String] -> Int -> VarMap -> Int -> Int -> AbsInstant.Exp -> AbsInstant.Exp -> String -> ([String], Int, Int, Int)
generateBinaryOp code reg varMap stackSize maxStackSize e1 e2 op =
  let (complexFirst, simpleSecond, reverseOrder) =
          if estimateDepth e1 >= estimateDepth e2
          then (e1, e2, False)  
          else (e2, e1, op `elem` ["isub", "idiv"])
      (code1, reg1, stackSize1, maxStackSize1) = generateExp code reg varMap stackSize maxStackSize complexFirst
      (code2, reg2, stackSize2, maxStackSize2) = generateExp code1 reg1 varMap (stackSize + 1) maxStackSize simpleSecond
      swapInstr = if reverseOrder then "  swap\n" else ""
      operationCode = swapInstr ++ "  " ++ op ++ "\n"
      newMaxStackSize = max maxStackSize1 maxStackSize2
  in (operationCode : code2, reg2, stackSize - 1, newMaxStackSize)
