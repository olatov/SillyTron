__Std__Sleep:
  push %rax
  lea __Std__TimeSpec(%rip), %rdi
  xor %rsi, %rsi
  xor %rcx, %rcx
  movw %ax, %cx
__Std__Sleep__Loop:
  push %rcx
  movq $35, %rax
  syscall
  pop %rcx
  loop __Std__Sleep__Loop
  pop %rax
  ret
