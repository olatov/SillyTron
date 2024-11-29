__Std__PrintString:
  push rax
  mov rsi, rax
__Std__PrintString__Loop:
  xor rax, rax
  mov al, [rsi]
  test al, al
  jz __Std__PrintString__Exit
  push rsi
  call __Std__PrintChar
  pop rsi
  inc rsi
  jmp __Std__PrintString__Loop
__Std__PrintString__Exit:
  mov rax, 10
  call __Std__PrintChar
  pop rax
  ret
