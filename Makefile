# Makefile for building the program

GHC        = ghc
HAPPY      = happy
ALEX       = alex

# List of source files in the src directory
src_files = src/AbsInstant.hs src/LexInstant.hs src/ParInstant.hs src/PrintInstant.hs src/MainLLVM.hs src/SkelInstant.hs src/GeneratorLLVM.hs

# List of goals not corresponding to file names
.PHONY : all clean distclean

# Default goal
all : insc_llvm

# Build the source files from Instant.cf using bnfc
src/AbsInstant.hs src/LexInstant.x src/ParInstant.y src/PrintInstant.hs : src/Instant.cf
	bnfc --haskell src/Instant.cf

# Rules for building generated .hs files from .y and .x files
src/%.hs : src/%.y
	${HAPPY} $< -o $@

src/%.hs : src/%.x
	${ALEX} $< -o $@

# Build the insc_llvm executable
insc_llvm : $(src_files)
	${GHC} -o $@ $^

# Rules for cleaning generated files
clean :
	-rm -f src/*.hi src/*.o src/*.log
