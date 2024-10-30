% MRJP - Zadanie 1  
% Piotr Wojcieszak  
% 2024-10-31  

# Kompilator Instant
Projekt kompilatora dla języka Instant do JVM i LLVM.

## Kompilacja i uruchamianie programu
Projekt kompilujemy przy pomocy Makefile używając komendy `make`. W korzeniu projektu pojawią się pliki wykonywalne insc_jvm i insc_llvm do kompilacji odpowiednio do JVM i LLVM.

W celu uruchomienia programu należy podać na wejściu programu plik z wyrażeniami zgodnymi z gramatyką Instant. Przykład uruchomienia:

`./insc_jvm ./examples/test01.ins`

`./insc_llvm ./examples/test01.ins`

Po uruchomieniu, w katalogu, w którym znajduje się plik wejściowy wygenerowane zostaną pliki:
- .j, .class dla insc_jvm
- .ll, .bc dla insc_llvm 

## Używane narzędzia i biblioteki
 - BNFC - 2.9.5
 - GHC - 9.4.7
 - LLVM - 18.1.3
 - Java - openjdk 17.0.12 2024-07-16
 - Jasmin - 2.3

## Struktura katalogów
```
.
 ├── lib
 │   └── jasmin.jar
 ├── Makefile
 ├── README.md
 └── src
     ├── GeneratorJVM.hs
     ├── GeneratorLLVM.hs
     ├── MainJVM.hs
     ├── MainLLVM.hs
     └── parser
         ├── AbsInstant.hs
         ├── Instant.cf
         ├── LexInstant.hs
         ├── LexInstant.x
         ├── ParInstant.hs
         └── ParInstant.y
```


W katalogu /lib znajduje się jar z kompilatorem języka Jasmin wykorzystywany do wygenerowania pliku .class.

W /src znajdują się przede wszystkim główne pliki uruchomieniowe dla kompilatorów:
  MainJVM.hs, MainLLVM.hs - punkty wejściowe programu przekazujące pliki wejściowe do parsera, wywołujące generator, tworzące pliki wyjśćiowe,
  GeneratorJVM.hs, GeneratorLLVM.hs - kompilatory AST w kod JVM/LLVM.

W /src znajdziemy również folder /parser z parserem wygenerowanym przez BNFC na podstawie zawierającego się tam pliku Instant.cf. 

Po zbudowaniu w korzeniu projektu pojawią się pliki wykonywalne insc_jvm i insc_llvm. Po uruchomieniu programu w folderze z plikiem wejściowym zostaną wygenerowane pliki .j i .class lub .ll i .bc.

## Optymalizacje
### Kolejność liczenia podwyrażeń
JVM. Zawsze liczę podwyrażenie o większym zapotrzebowaniu stosowym jako pierwsze.

### Eliminacja zbędnych swap
JVM. Problem jest skutkiem zmiany kolejności liczenia podwyrażeń. Jeśli węzłem jest operacja o istotnej kolejności operatorów jak odejmowanie i dzielenie, wyniki na stosie zamieniam komendą `swap`. Dla dodawania i mnożenia nie ma to znaczenia, dlatego wtedy komendy tej nie wykonuję.

### Wybór instrukcji
JVM. W programie korzystam ze skróconych instrukcji do wczytywania stałych oraz zmiennych i ich zapisu.

### .limit stack
Dla JVM `.limit stack` liczony jest od dołu. Przechodzę rekrurencyjnie przez drzewo i zatrzymawszy się w liściu zwiększam o 1. W węzłach porównuję rozmiary stosu w obu poddrzewach, wybieram to o większym stosie do obliczenia jako pierwsze i rozmiar stosu węzła ustawiam jako maksimum z drzewa o większym stosie a mniejszym + 1 (bo w momencie przejścia do niego będzie na stosie wynik lewego). Na końcu przejścia przez drzewo porównuję jego wartość z wartością maksymalną stosu dla całego programu. W przypadku przetwarzania prostego (złożonego z literałów albo zmiennych) SExp przed obliczeniami wstawiam na stos pole 'out' dlatego licznik stosu ma na starcie '1'.

### Budowanie napisu wyjściowego
Napis wyjściowy (plik .j lub .ll) buduję poprzez dodawanie napisów na początku tablicy napisu wyjściowego (oprócz nagłówka i stopki typowej dla generowanego pliku). Na końcu tablicę odwracam i łącze jej elementy. Robię tak ponieważ dodawanie elementu na początek tablicy i ich jednorazowe odwrócenie jest znacznie tańsze niż dodawanie ich na końcu.

## Zapożyczenia
Część kodu w MainJVM.hs i MainLLVM.hs jest z wygenerowanego przez BNFC pliku testowego parsera. Plik ten był dla mnie punktem startowym, który zmodyfikowałem do swoich potrzeb. Podobnie sprawa ma się z Makefile.