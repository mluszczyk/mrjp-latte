set -xe

LATTELIB="lattelib.c"

TEST_TEMPLATE="${TMPDIR}latteXXX"
TEST_DIR=`mktemp -d "$TEST_TEMPLATE"`

input_file="$1"

BASENAME=`basename "$input_file" .lat`
SFILE="$TEST_DIR/${BASENAME}.S"
CLANG_OUT="$TEST_DIR/${BASENAME}.out"
MY_ANS="$TEST_DIR/${BASENAME}.myans"
CORRECT_ANS="examples/my_good/${BASENAME}.output"
stack exec compile -- --x86_64 "$input_file" > $SFILE
clang -Wall -Werror "$SFILE" "$LATTELIB" -o "$CLANG_OUT"
$CLANG_OUT

# rm -rf "$TEST_DIR"
