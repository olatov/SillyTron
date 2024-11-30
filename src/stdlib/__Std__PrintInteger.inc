__Std__PrintInteger:
  push %rax
  push %rbx
  test %ax, %ax
  jns __Std__PrintInteger__ProcessNumber
  neg %ax
  push %rax
  movb $'-', %al
  call __Std__PrintChar
  pop %rax
__Std__PrintInteger__ProcessNumber:
  xor %rcx, %rcx
__Std__PrintInteger__ReverseDigits:
  xor %dx, %dx
  movw $10, %bx
  div %bx
  push %rdx
  inc %cx
  test %ax, %ax
  jnz __Std__PrintInteger__ReverseDigits
__Std__PrintInteger__PrintDigits:
  pop %rdx
  movb %dl, %al
  addb $'0', %al
  push %rcx
  call __Std__PrintChar
  pop %rcx
  loop __Std__PrintInteger__PrintDigits
  movb $10, %al
  call __Std__PrintChar
  pop %rbx
  pop %rax
  ret
