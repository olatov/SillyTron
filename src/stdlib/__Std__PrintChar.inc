__Std__PrintChar:
  push rax
  mov [__Std__CharBuf], al
  mov rax, 1
  mov rdi, rax
  mov rsi, __Std__CharBuf
  mov rdx, rax
  syscall
  pop rax
  ret
