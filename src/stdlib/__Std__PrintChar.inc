__Std__PrintChar:
  push %rax
  movb %al, __Std__CharBuf(%rip)
  movq $1, %rax
  movq %rax, %rdi
  leaq __Std__CharBuf(%rip), %rsi
  movq %rax, %rdx
  syscall
  pop %rax
  ret
