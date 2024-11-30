__Std__PrintChar:
  pushq %rax
  movb %al, __Std__CharBuf(%rip)
  movq $1, %rax
  movq %rax, %rdi
  leaq __Std__CharBuf(%rip), %rsi
  movq %rax, %rdx
  syscall
  popq %rax
  ret
