# SillyAsm To NASM

## Translation tool

A simple tool is available that converts Sillytron Assembly Language (SillyAsm) to Netwide Assembler (NASM) source file  for `linux/amd64` platform. This allows building native binary executables from SillyAsm sources, which can execute on a Linux system, not requiring the SillyTron runtime machine.

## Usage:

```
sillyasmtonasm <input_file.sasm> [-v]
```

This fill produce input_filename.asm file in the same directory.

The `-v` flag will enable verbose mode, which includes the original SASM lines into the resulting NASM code as comments.

Then you can use the NASM tool and a linker available in your system to build an executable binary (use an apt command to install NASM if you are on a Debian based system):

```
sudo apt install nasm
```

## Example:

```
$ bin/sillyasmtonasm examples/hello.sasm -v
Written examples/hello.asm (711 bytes).

$ nasm -f elf64 -o hello.o examples/hello.asm

$ ld -o hello hello.o

$ file hello  # (this step is optional)
hello: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, not stripped

$ ./hello
Hello from SillyTron!
```
