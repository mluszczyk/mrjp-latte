set -xe

LATTELIB="lattelib.c"

TEST_TEMPLATE="${TMPDIR}latteXXX"
TEST_DIR=`mktemp -d "$TEST_TEMPLATE"`

input_file="$1"

BASENAME=`basename "$input_file" .lat`
LLFILE="$TEST_DIR/${BASENAME}.ll"
CLANG_OUT="$TEST_DIR/${BASENAME}.out"
LLVM_ANS="$TEST_DIR/${BASENAME}.llans"
CORRECT_ANS="examples/my_good/${BASENAME}.output"
stack exec compile "$input_file" > $LLFILE
clang "$LLFILE" "$LATTELIB" -o "$CLANG_OUT"
$CLANG_OUT

# rm -rf "$TEST_DIR"
