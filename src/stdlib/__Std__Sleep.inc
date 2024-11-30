__Std__Sleep:
  pushq %rax
  lea __Std__TimeSpec(%rip), %rdi
  xorq %rsi, %rsi
  xorq %rcx, %rcx
  movw %ax, %cx
__Std__Sleep__Loop:
  pushq %rcx
  movq $35, %rax
  syscall
  popq %rcx
  loop __Std__Sleep__Loop
  popq %rax
  ret
