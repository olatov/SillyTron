#!/usr/bin/env bash
set -euo pipefail

# Build tools and run comparison + logical tests
./build.sh

echo "Compiling examples/compare.c"
bin/sillyc examples/compare.c
echo "Assembling examples/compare.sasm"
bin/sillyasm examples/compare.sasm
echo "Running examples/compare.bin"
bin/sillytron examples/compare.bin

echo "\nCompiling examples/logical.c"
bin/sillyc examples/logical.c
echo "Assembling examples/logical.sasm"
bin/sillyasm examples/logical.sasm
echo "Running examples/logical.bin"
bin/sillytron examples/logical.bin

echo "All tests executed."
