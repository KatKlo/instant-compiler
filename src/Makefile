GHC        = ghc

JVM_COMPILER_NAME = insc_jvm
LLVM_COMPILER_NAME = insc_llvm

JVM_COMPILER_SOURCE = InstantJvmCompiler.hs
LLVM_COMPILER_SOURCE = InstantLlvmCompiler.hs

.PHONY : all clean

all : $(JVM_COMPILER_NAME) $(LLVM_COMPILER_NAME)

$(JVM_COMPILER_NAME) : $(JVM_COMPILER_SOURCE)
	${GHC} $< -o ../$@

$(LLVM_COMPILER_NAME) : $(LLVM_COMPILER_SOURCE)
	${GHC} $< -o ../$@

clean :
	find . \( -name '*.hi' -o -name '*.o' \) -type f -exec rm -f {} \;
