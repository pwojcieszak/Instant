# Makefile for building the program
GHC        = ghc

# List of source files in the src directory
src_files = src/parser/AbsInstant.hs src/parser/LexInstant.hs src/parser/ParInstant.hs

# List of goals not corresponding to file names
.PHONY : all clean

# Default goal
all : insc_llvm  insc_jvm

# Build the insc_llvm and insc_jvm executable
insc_llvm : $(src_files) src/GeneratorLLVM.hs src/MainLLVM.hs
	${GHC} -o $@ $^

insc_jvm : $(src_files) src/GeneratorJVM.hs src/MainJVM.hs
	${GHC} -o $@ $^

# Rules for cleaning generated files
clean:
	-rm -f insc_jvm insc_llvm
	find . -type f \( -name "*.hi" -o -name "*.o" -o -name "*.log" -o -name "*.j" -o -name "*.class" -o -name "*.ll" -o -name "*.bc" \) -exec rm -f {} +
