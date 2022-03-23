#!/bin/bash

NAME="$1"
cd "$(dirname $0)"
clang -O1 -target bpf -c "./$NAME.c"
llvm-objdump -d "./$NAME.o" > "./$NAME.dump"
llvm-objcopy -O binary "$NAME.o" "$NAME.bin"
