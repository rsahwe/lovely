bits 64
global _start

section .rodata

  main#1: dq FUN#0

section .text

_start:
  ; goto FUN#0
  jmp FUN#0


FUN#0:
  enter 0, 0
  ; t1 := 9 - 3
  mov rax, 9
  sub rax, 3
  push qword rax

  ; t2 := t1 * 4
  mov rax, [rbp - 8]
  imul rax, 4
  push qword rax

  ; t3 := t2 / 3
  mov rax, [rbp - 16]
  mov rcx, 3
  idiv rcx
  push qword rax

  ; t4 := t3 + 3
  mov rax, [rbp - 24]
  add rax, 3
  push qword rax

  ; x#0 := t4
  push qword [rbp - 32]

  ; exit x#0
  mov rax, 60
  mov rdi, [rbp - 40]
  syscall