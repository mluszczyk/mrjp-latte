set -xe

LATTELIB="lattelib.c"

TEST_TEMPLATE="${TMPDIR}latteXXX"
TEST_DIR=`mktemp -d "$TEST_TEMPLATE"`

for input_file in examples/my_good/*.lat ../lattests/good/*.lat ../mrjp-tests/good/basic/*.lat ../mrjp-tests/good/arrays/*.lat; do
  LAT_DIR=`dirname "$input_file"`
  BASENAME=`basename "$input_file" .lat`
  SFILE="$TEST_DIR/${BASENAME}.S"
  CLANG_OUT="$TEST_DIR/${BASENAME}.out"
  PROG_ANS="$TEST_DIR/${BASENAME}.ans"
  CORRECT_ANS="${LAT_DIR}/${BASENAME}.output"
  stack exec compile -- --x86_64 "$input_file" > "$SFILE"
  clang -Wall -Werror "$SFILE" "$LATTELIB" -o "$CLANG_OUT"
  PROG_IN="${LAT_DIR}/${BASENAME}.input"
  if [ ! -f "${PROG_IN}" ]; then
    PROG_IN="/dev/null"
  fi
  $CLANG_OUT < "${PROG_IN}" > "${PROG_ANS}"
  diff -q $CORRECT_ANS $PROG_ANS
done

for input_file in examples/my_compiler_err/*.lat ../lattests/bad/*.lat ../mrjp-tests/bad/semantic/*; do
  BASENAME=`basename "$input_file" .lat`
  LLFILE="$TEST_DIR/${BASENAME}.ll"
  CERR_FILE="$TEST_DIR/${BASENAME}.compiler-error"
  stack exec compile -- --x86_64 "$input_file" > "${LLFILE}" 2> "${CERR_FILE}" && { echo "should fail"; exit 1; }
done

exit 0

# rm -rf "$TEST_DIR"
