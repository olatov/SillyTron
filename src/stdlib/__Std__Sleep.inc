__Std__Sleep:
  push rax
  lea rdi, [__Std__TimeSpec]
  xor rsi, rsi
  xor rcx, rcx
  mov cx, ax
__Std__Sleep__Loop:
  push rcx
  mov rax, 35
  syscall
  pop rcx
  loop __Std__Sleep__Loop
  pop rax
  ret
