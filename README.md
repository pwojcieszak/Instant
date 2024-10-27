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

Limit stack liczę tak: jeżeli expression to już jeden punkt. wywołując generateStmt zwiększam o '1' bo wiem że będę potrzebowaął miejsca na wynik. Następnie rekruencyjnie przechodząc drzewo wiem, że w lewych dzieciach dojdę finalnie do liczby (albo zmiennej) która będzie operandem. Zajmuje ona miejsce przeznaczone dla wyniku. Stos może się pogłębić tylko przez prawe poddrzewo. Dlatego przy każdym zejściu w prawo zwiększam aktualną wartość stosu o 1.

