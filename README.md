# latte

## Features
The compiler supports the complete Latte language including arrays.
LLVM IR and x86_64 backends are available.

### Optimisations
The code is transformed to SSA first. The following optimisations are applied:
- removal of unreachable blocks,
- removal of unused assignments,
- removal of unreachable instructions inside blocks,
- copy propagation (thanks to transformation to SSA),
- compile time calculation of constant expressions,
- removal of trivial phi instructions.

### Register allocation
x86_64 backend stores commonly used values in processor registers.
The implemented algorithm bases on SSA. Interference graph of SSA registers is
built and coloured. Registers with the same colour are assigned to the same
register or address in memory.

Moreover, if a function argument is passed by register and register allocation
algorithm decides to keep it in register (rather than on the stack), it will
preserve its original register. Similarly, if a function argument is passed
on stack and the algorithm decides to keep it in memory, the argument's colour
is granted the argument's place in memory.

Before function calls, all caller preserved registers are backed up on stack
and restored after the called function returns.

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
