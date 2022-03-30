#!/bin/bash

set -euo pipefail

NAME="$1"
cd "$(dirname $0)"

if [ -f "./$NAME.S" ]; then
  SRC="./$NAME.S"
elif [ -f "./$NAME.c" ]; then
  SRC="./$NAME.c"
else
  echo "No source file found"
  exit 1
fi
clang -O1 -target bpf -Xclang -target-feature -Xclang +alu32 -c "$SRC"
llvm-objdump -d "./$NAME.o" > "./$NAME.dump"
llvm-objcopy -O binary "$NAME.o" "$NAME.bin"
xxd -i "$NAME.bin" > "$NAME.hex"
