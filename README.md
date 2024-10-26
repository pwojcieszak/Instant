% MRJP - Zadanie 1
% Piotr Wojcieszak
% 2024-10-31

Zadanie 1
=========

ChatGPT pomógł mi z napisaniem Makefile i jak zapisać coś takiego 

let newMaxStackDepth = max stackDepth maxStackDepth
  in case () of
    _ | n >= -1 && n <= 5    -> ("  iconst_" ++ show n ++ "\n", reg, stackDepth, newMaxStackDepth)
      | n >= -128 && n <= 127 -> ("  bipush " ++ show n ++ "\n", reg, stackDepth, newMaxStackDepth)
      | n >= -32768 && n <= 32767 -> ("  sipush " ++ show n ++ "\n", reg, stackDepth, newMaxStackDepth)
      | otherwise             -> ("  ldc " ++ show n ++ "\n", reg, stackDepth, newMaxStackDepth)

Limit locals w JVM liczę po rozmiarze tablicy zmiennych lokalnych + 1 ponieważ metodą Main przyjmuje tablicę String

