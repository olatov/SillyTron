# Sillytron Assembly Language Reference

## Introduction

SillyTron is a toy virtual machine written in Object Pascal.
It has a simple assembly language (SillyAsm), providing support for basic integer arithmetic, data manipilation, control flow, and other functions.

Note that SillyAsm is case insensitive (so `ADD $COUNTER`, `add $counter` and `Add $Counter` are all the same operation).

### Declaring variables

Variables are user-defined names that can be used to refer to values in memory.

Syntax:
```
var <name> <type> <value>
```
`name` is an arbitrary aplhanumeric name.
`type` defines the type of the variable, can be either `integer` or `string`.
`value` is the initial value of the variable.

Examples:
```
var Counter integer 0
var Message string 'Hello from SillyTron'
```

### Declaring labels

Labels are user-defined names that can be used to jump to a specific instruction.
Label consists of an arbitrary aplhanumeric name followed by a colon.

Syntax:
```
<name>:
```
Example:
```
Main:
...
Loop001:
...
ExitXYZ:
...
```

### Comments

Comments can be used in the source code to describe the purpose of the instruction.
Comments are started with double slash sequence (`//`) and are ignored by the assembler.

Syntax: 
```
// This is a comment
```

Example:
```
// This is a comment
Add 5
```

## Arithmetic

### Add

Add a number to the accumulator.
Sets or clears Zero and Sign flags according to the result of the operation.

Syntax:
```
Add <const | var>
```

Example:
```
Add 1234    // Add 1234 to the accumulator
Add $foo    // Add the value of $foo variable to the accumulator
```
### Subtract

Subtract a number from the accumulator.
Sets or clears Zero and Sign flags according to the result of the operation.

Syntax:
```
Subtract <const | var>
```

Example:
```
Subtract 1245    // Subtract 1234 from the accumulator
Subtract $foo    // Subtract the value of $foo from the accumulator
```

### Inc

Increase a value of a register (Accumulator or Index) by 1.
When used with Accumulator, sets or clears Zero and Sign flags according to the result of the operation.

Syntax:
```
Inc <Acc | Idx>
```

Example:
```
Inc Acc  // Increment the Acumulator
Inc Idx  // Increment the Index
```

### Dec

Decrease a value of a register (Accumulator or Index) by 1.
When used with Accumulator, sets or clears Zero and Sign flags according to the result of the operation.

Syntax:
```
Dec <Acc | Idx>
```

Example:
```
Dec Acc // Decrement the Acumulator
Dec Idx // Decrement the Index
```

### Negate

Negate a value of Accumulator.
Sets or clears Zero and Sign flags according to the result of the operation.

Syntax:
```
Negate
```

Example:
```
Load 1234  // Load 1234 into Accumulator
Negate     // The value is -1234 now
```

## Data manipulation

### Load

Copy a value (const, Index register, address, or memory) into Accumulator.

Syntax:
```
Load <const | Idx | @Idx | $var | @addr>
```

Example:
```
Load 1234  // Load the value 1234 into the Accumulator
Load Idx   // Load the value of the Index register into the Accumulator
Load @Idx  // Load the value stored in memory at address in the Index register into the Accumulator
Load $foo  // Load the value of $foo into the Accumulator
Load @bar  // Load the value of the address @bar into the Accumulator
```

### Store

Copy a value from Accumulator into Index register, or memory.

Syntax:
```
Store <Idx | @Idx | $var>
```

Example:
```
Store Idx  // Store the value in the Accumulator into the Index register
Store @Idx // Store the value in the Accumulator into the memory address in the Index register
Store $foo // Store the value in the Accumulator into the variable $foo
```

### Push

Place a value from Accumulator on top of the stack. This decreases the Stack Pointer register value by 1.

Syntax:
```
Push
```

Example:
```
Push
...
Pop
```

### Pop

Retrieve a value from top the stack into Accumulator. This increases the Stack Pointer register value by 1.

Syntax:
```
Pop
```

Example:
```
Push
...
Pop
```

## Control flow

### Jump

Unconditional jump to a label.

Syntax:
```
Jump <label>
```

Example:
```
Jump Main
...
Main:
    ...
```

### JumpIfZero

Jump to a label if the Zero flag is set_, otherwise proceed to the next instruction.

Syntax:
```
JumpIfZero <label>
```

Example:
```
JumpIfZero Exit
...
Exit:
    ...
```

### JuimpIfNotZero

Jump to a label if the Zero flag is not set, otherwise proceed to the next instruction.

Syntax:
```
JumpIfNotZero <label>
```

Example:
```
JumpIfNotZero Exit
...
Exit:
    ...
```

### JumpIfSign

Jump to a label if the Sign flag is set, otherwise proceed to the next instruction.

Syntax:
```
JumpIfSign <label>
```

Example:
```
JumpIfSign Exit
...
Exit:
    ...
```

### JumpIfNotSign

Jump to a label if the Sign flag is not set, otherwise proceed to the next instruction.

Syntax:
```
JumpIfNotSign <label>
```

Example:
```
JumpIfNotSign Exit
...
Exit:
    ...
```

### Call

Call a subroutine. The return address is saved in the stack, therefore, the execution can be resumed when the subroutine returns. This decreases the Stack Pointer register value by 1.


Syntax:
```
Call <label>
```

Example:
```
Call Proc
...
Proc:
    ...
    Return
```

### Return

Return from a subroutine. The return address is extracted from the stack. This increases the Stack Pointer register value by 1.


Syntax:
```
Return
```

Example:
```
Call Proc
...
Proc:
    ...
    Return
```

### Halt

Stops execution of the program by raising the Halt flag.

Syntax:
```
Halt
```

Example:
```
Halt
```

## Misc

### Nop

Do nothing (empty cycle).

Syntax:
```
Nop
```

Example:
```
Nop
```

### Sleep

Pause execution of a number of milliseconds specified in Accumulator.

Syntax:
```
Sleep
```

Example:

```
Load 1500
Sleep   // Pause for 1.5 seconds
```

## Printing

### PrintChar

Print a character specified in Accumulator.

Syntax:
```
PrintChar
```

Example:
```
Load 65
PrintChar  // Prints the character 'A' (ASCII code 65).
```

### PrintInteger

Print an integer specified in Accumulator, followed by a new line symbol.

Syntax:
```
PrintInteger
```

Example:
```
Load 1234
PrintInteger  // Prints '1234' and moves ouput to the new line.
```

### PrintString

Print a string by a reference specified in Accumulator, followed by a new line symbol.
The printing starts with the first character that the Accumulator points to in memory, and ends when a zero value word is encountered (null terminated strings).

Syntax:
```
PrintString
```

Example:
```
var Message string 'Hello from SillyTron'
...
Load @Message  // Loads the address (pointer) to Message string into Accumulator
PrintString    // Prints 'Hello from SillyTron' and moves ouput to the new line.
```
