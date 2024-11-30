__Std__PrintInteger:
  push rax
  push rbx
  test ax, ax
  jns __Std__PrintInteger__ProcessNumber
  neg ax
  push rax
  mov al, '-'
  call __Std__PrintChar
  pop rax
__Std__PrintInteger__ProcessNumber:
  xor rcx, rcx
__Std__PrintInteger__ReverseDigits:
  xor dx, dx
  mov bx, 10
  div bx
  push rdx
  inc cx
  test ax, ax
  jnz __Std__PrintInteger__ReverseDigits
__Std__PrintInteger__PrintDigits:
  pop rdx
  mov al, dl
  add al, '0'
  push rcx
  call __Std__PrintChar
  pop rcx
  loop __Std__PrintInteger__PrintDigits
  mov al, 10
  call __Std__PrintChar
  pop rbx
  pop rax
  ret
