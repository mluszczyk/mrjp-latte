set -xe

TEST_TEMPLATE="${TMPDIR}latteXXX"
TEST_DIR=`mktemp -d "$TEST_TEMPLATE"`

for input_file in examples/my_good/*.lat; do
  BASENAME=`basename "$input_file" .lat`
  LLFILE="$TEST_DIR/${BASENAME}.ll"
  CLANG_OUT="$TEST_DIR/${BASENAME}.out"
  LLVM_ANS="$TEST_DIR/${BASENAME}.llans"
  CORRECT_ANS="examples/my_good/${BASENAME}.output"
  stack exec compile "$input_file" > $LLFILE
  clang "$LLFILE" -o "$CLANG_OUT"
  $CLANG_OUT > $LLVM_ANS
  diff -q $CORRECT_ANS $LLVM_ANS
done

# rm -rf "$TEST_DIR"
