SillyTron
=========

SillyTron is a toy virtual machine written in Object Pascal.

It has a simple assembler.

The virtual machine is bytecode-oriented, 16-bit integer based (that means it supports -32768..32767 range), has a few opcodes to load and store values, add and subtract, jump and call functions, and print strings and characters.

The assembler is also written in Pascal, and uses simplistic approach to parse the source code.

# Requirements

- [Free Pascal Compiler](https://www.freepascal.org/) version 3.3+ is required. Note that version 3.3 of the compiler is still in development, therefore must use the trunk version. Easiest is to pull `freepascal/fpc:trunk-full` image from Docker Hub (`build-docker.sh` script will do it for you).

# Usage

1. Use `build-docker.sh` or `build.sh` (if you have appropriate Free Pascal compiler installed in your system) to build SillyTron and SillyAsm tools.

```bash
./build-docker.sh
```
This will create `sillytron` and `sillyasm` stand-alone executables the `bin` directory.

2. Build a program using SillyAsm.

```bash
bin/sillyasm examples/sample.sasm
```
This will produce a compiled binary called `sample.bin`.

3. Run the program using SillyTron.

```bash
bin/sillytron examples/sample.bin
```
