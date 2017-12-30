# latte

## Features
The compiler supports the complete Latte language without extensions.
LLVM IR and x86_64 backends are available.

### Optimisations
The code is transformed to SSA first. The following optimisations are applied:
- removal of unreachable blocks,
- removal of unused assignments,
- removal of unreachable instructions inside blocks,
- copy propagation (thanks to transformation to SSA),
- compile time calculation of constant expressions,
- removal of trivial phi instructions.

## Design

### Library functions
`lattelib.c` defines library functions and auxiliary builtins like
string concatenation.

### Name mangling
Functions (apart from `main`) are appended a prefix `latte_`, so that they
do not conflict with functions from the standard library.

## Project structure
The following files are of particular interest:
- CompileLatte - translates Latte AST to LLVM,
- CompilerErr - listing of possible compiler errors,
- CompilerState - monads used in the compiler,
- LLVM - definition of LLVM IR AST and pretty printing,
- TransLLVM - transformations on LLVM including
  optimisations and mem2reg algorithm.
- X86_64 - translates LLVM to x86_64.

Moreover, `lattelib.c` contains Latte library functions like `printInt`.
