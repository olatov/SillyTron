__Std__InputInteger:
    # Print prompt
    movq $'?', %rax
    call __Std__PrintChar
    movq $' ', %rax
    call __Std__PrintChar

    # Syscall: read(0, buffer, 16)
    movl $0, %edi           # File descriptor: stdin
    leaq __Std__InputBuf(%rip), %rsi # Address of buffer
    movl $16, %edx          # Max bytes to read
    movl $0, %eax           # Syscall number: read
    syscall

    # Convert decimal string in buffer to integer
    movw $0, %ax            # Clear AX (result)
    leaq __Std__InputBuf(%rip), %rsi # RSI points to buffer

    # Check for optional negative sign
    movzbw (%rsi), %cx      # Load first character into CX
    cmpb $'-', %cl          # Check if it`s a negative sign
    jne __Std__InputInteger__ConvertLoop        # If not, proceed with conversion
    movb $1, %dl            # Set sign flag (DL = 1 means negative)
    incq %rsi               # Skip the '-' character

__Std__InputInteger__ConvertLoop:
    movzbw (%rsi), %cx      # Load next byte from buffer into CX
    cmpb $10, %cl           # Check for newline (ASCII 10)
    je __Std__InputInteger__DoneConversion      # Exit loop if newline is reached
    subb $48, %cl           # Convert ASCII digit to binary (subtract '0')
    cmpb $9, %cl            # Ensure valid digit (0-9)
    ja __Std__InputInteger  # Repeat on invalid input
    imulw $10, %ax          # Multiply current result by 10
    addw %cx, %ax           # Add the new digit
    incq %rsi               # Advance to the next character
    jmp __Std__InputInteger__ConvertLoop        # Repeat

__Std__InputInteger__DoneConversion:
    # Apply negative sign if needed
    cmpb $1, %dl            # Check if sign flag is set
    jne __Std__InputInteger__ReturnResult       # If not negative, skip negation
    negw %ax                # Negate AX for negative numbers
__Std__InputInteger__ReturnResult:
    ret                     # Return to caller with result in AX
