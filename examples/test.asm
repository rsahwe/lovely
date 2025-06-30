global _start

section .rodata

  x#0: dq 9
  main#1: dq FUN#0

section .text

_start:
  jmp FUN#0

FUN#0:
  mov rax, 60
  mov rdi, x#0
  syscall