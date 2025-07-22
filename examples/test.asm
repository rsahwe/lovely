bits 64
global _start

section .rodata

  main#2: dq FUN#0

section .text

_start:
  ; goto FUN#0
  jmp FUN#0


FUN#0:
  enter 0, 0

  ; y#0 := 3
  push qword 3

  ; t1 := 9 - 3
  mov rax, 9
  sub rax, 3
  push qword rax

  ; t2 := t1 * 4
  mov rax, [rbp - 16]
  imul rax, 4
  push qword rax

  ; t3 := t2 / y#0
  mov rax, [rbp - 24]
  cqo
  idiv qword [rbp - 8]
  push qword rax

  ; t4 := t3 + 3
  mov rax, [rbp - 32]
  add rax, 3
  push qword rax

  ; x#1 := t4
  push qword [rbp - 40]

  ; exit x#1
  mov rax, 60
  mov rdi, [rbp - 48]
  syscall