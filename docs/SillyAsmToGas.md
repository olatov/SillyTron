# SillyAsm To GNU Assembler

## Translation tool

A simple tool is available that can convert Sillytron Assembly Language (SillyAsm) to GNU Assembler (GAS) source file  for `linux/amd64` platform. This allows building native binary executables from SillyAsm sources, which can execute on a Linux system, not requiring the SillyTron runtime to be available. The binary has no external dependencies (not even `libc`) so in theory can run in any Linux environment of said architecture.

## Note
This works with `linux/amd64` platform only. You can still run the translator tool in other systems, but the generated assembly code is platform specific, therefore can only be used to create executables for 64-bit Intel/AMD Linux systems.

## Usage

```
sillyasmtogas <input_file.sasm> [-v]
```

This fill create `input_filename.s` file in the same directory as the original source, which can then be used to build a native binary executable.

The `-v` flag will enable verbose mode, this makes the translator to include the original SASM lines into the resulting GAS code in form of comments, which can be useful for debugging.

Ensure you have `as` (the GNU assembler) and `ld` (the GNU linker) installed. These are part of the GNU `binutils` package. 

Install on Ubuntu/Debian:

```
sudo apt update
sudo apt install build-essential
```

## Example

```
$ bin/sillyasmtogas examples/hello.sasm -v
Written examples/hello.s (902 bytes).

$ as -o hello.o examples/hello.s

$ ld -o hello hello.o

$ file hello  # (this step is just for information)
hello: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, not stripped

$ ./hello
Hello from SillyTron!
```
