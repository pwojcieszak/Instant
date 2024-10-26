# Makefile for building the program

GHC        = ghc
HAPPY      = happy
ALEX       = alex

# List of source files in the src directory
src_files = src/parser/AbsInstant.hs src/parser/LexInstant.hs src/parser/ParInstant.hs src/parser/PrintInstant.hs src/parser/SkelInstant.hs 

# List of goals not corresponding to file names
.PHONY : all clean

# Default goal
all : insc_llvm  insc_jvm

# Build the source files from Instant.cf using bnfc
src/parser/AbsInstant.hs src/parser/LexInstant.x src/parser/ParInstant.y src/parser/PrintInstant.hs : src/Instant.cf
	bnfc --haskell src/Instant.cf

# Rules for building generated .hs files from .y and .x files
src/parser/%.hs : src/parser/%.y
	${HAPPY} $< -o $@

src/parser/%.hs : src/parser/%.x
	${ALEX} $< -o $@

# Build the insc_llvm executable
insc_llvm : $(src_files) src/GeneratorLLVM.hs src/MainLLVM.hs
	${GHC} -o $@ $^

insc_jvm : $(src_files) src/GeneratorJVM.hs src/MainJVM.hs
	${GHC} -o $@ $^

# Rules for cleaning generated files
clean :
	-rm -f src/*.hi src/*.o src/*.log src/parser/*.hi src/parser/*.o src/parser/*.log foo/bar/* insc_jvm insc_llvm
