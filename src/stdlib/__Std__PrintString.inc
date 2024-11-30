__Std__PrintString:
  pushq %rax
  movq %rax, %rsi
__Std__PrintString__Loop:
  xorq %rax, %rax
  movb (%rsi), %al
  testb %al, %al
  jz __Std__PrintString__Exit
  pushq %rsi
  call __Std__PrintChar
  popq %rsi
  incq %rsi
  jmp __Std__PrintString__Loop
__Std__PrintString__Exit:
  movq $10, %rax
  call __Std__PrintChar
  popq %rax
  ret
