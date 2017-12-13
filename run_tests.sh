set -xe

LATTELIB="lattelib.c"

TEST_TEMPLATE="${TMPDIR}latteXXX"
TEST_DIR=`mktemp -d "$TEST_TEMPLATE"`

# consider adding 
for input_file in examples/my_good/*.lat ../lattests/good/*.lat ../mrjp-tests/good/basic/*.lat; do
  LAT_DIR=`dirname "$input_file"`
  BASENAME=`basename "$input_file" .lat`
  LLFILE="$TEST_DIR/${BASENAME}.ll"
  CLANG_OUT="$TEST_DIR/${BASENAME}.out"
  LLVM_ANS="$TEST_DIR/${BASENAME}.llans"
  CORRECT_ANS="${LAT_DIR}/${BASENAME}.output"
  stack exec compile "$input_file" > "$LLFILE"
  clang -Wall -Werror "$LLFILE" "$LATTELIB" -o "$CLANG_OUT"
  LLVM_IN="${LAT_DIR}/${BASENAME}.input"
  if [ ! -f "${LLVM_IN}" ]; then
    LLVM_IN="/dev/null"
  fi
  $CLANG_OUT < "$LLVM_IN" > "$LLVM_ANS"
  diff -q $CORRECT_ANS $LLVM_ANS
done

for input_file in examples/my_compiler_err/*.lat ../lattests/bad/*.lat; do
  BASENAME=`basename "$input_file" .lat`
  LLFILE="$TEST_DIR/${BASENAME}.ll"
  CERR_FILE="$TEST_DIR/${BASENAME}.compiler-error"
  stack exec compile "$input_file" > "${LLFILE}" 2> "${CERR_FILE}" && { echo "should fail"; exit 1; }
done

# rm -rf "$TEST_DIR"
