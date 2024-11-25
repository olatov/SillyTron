#!/usr/bin/env sh

fpc -O4 -XXs -o"bin/sillytron" src/sillytron.lpr
fpc -O4 -XXs -o"bin/sillyasm" src/sillyasm.lpr
fpc -O4 -XXs -o"bin/sillyasmtonasm" src/sillyasmtonasm.lpr
