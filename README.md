# latte

## Design

### Library functions
`lattelib.c` defines library functions and auxiliary builtins like
string concatenation.

### Name mangling
Functions (apart from `main`) are appended a prefix `latte_`, so that they
do not conflict with functions from the standard library.
