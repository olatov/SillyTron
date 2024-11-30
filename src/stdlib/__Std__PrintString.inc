__Std__PrintString:
  push %rax
  movq %rax, %rsi
__Std__PrintString__Loop:
  xor %rax, %rax
  movb (%rsi), %al
  test %al, %al
  jz __Std__PrintString__Exit
  push %rsi
  call __Std__PrintChar
  pop %rsi
  inc %rsi
  jmp __Std__PrintString__Loop
__Std__PrintString__Exit:
  movq $10, %rax
  call __Std__PrintChar
  pop %rax
  ret
