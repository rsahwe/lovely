bits 64
global _start

section .rodata

  x#0: dq 9
  main#3: dq FUN#0

section .text

_start:
  jmp FUN#0

FUN#0:
  enter 0, 0
  push qword [x#0]
  add qword [rbp - 8], 2
  push qword [rbp - 8]
  push qword [rbp - 16]
  mov rax, 60
  mov rdi, [rbp - 24]
  syscall