__Std__PrintInteger:
  pushq %rax
  pushq %rbx
  testw %ax, %ax
  jns __Std__PrintInteger__ProcessNumber
  negw %ax
  pushq %rax
  movb $'-', %al
  call __Std__PrintChar
  popq %rax
__Std__PrintInteger__ProcessNumber:
  xorq %rcx, %rcx
__Std__PrintInteger__ReverseDigits:
  xorw %dx, %dx
  movw $10, %bx
  divw %bx
  pushq %rdx
  incw %cx
  testw %ax, %ax
  jnz __Std__PrintInteger__ReverseDigits
__Std__PrintInteger__PrintDigits:
  popq %rdx
  movb %dl, %al
  addb $'0', %al
  push %rcx
  call __Std__PrintChar
  popq %rcx
  loop __Std__PrintInteger__PrintDigits
  movb $10, %al
  call __Std__PrintChar
  popq %rbx
  popq %rax
  ret
