#!/bin/bash

set -euo pipefail

NAME="$1"
cd "$(dirname $0)"
clang -O1 -target bpf -Xclang -target-feature -Xclang +alu32 -c "./$NAME.c"
llvm-objdump -d "./$NAME.o" > "./$NAME.dump"
llvm-objcopy -O binary "$NAME.o" "$NAME.bin"
xxd -i "$NAME.bin" > "$NAME.hex"
