#!/usr/bin/env sh

rm bin/*.o bin/*.ppu
fpc -O4 -XXs -o"bin/sillytron" src/sillytron.lpr
fpc -O4 -XXs -o"bin/sillyasm" src/sillyasm.lpr
fpc -O4 -XXs -FF -o"bin/sillyasmtogas" src/sillyasmtogas.lpr
