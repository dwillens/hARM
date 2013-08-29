/*
 * 5. (15 Points) Write a program in your assembly language to read an
 *    enter-terminated (i.e., line-feed) sequence of characters from the
 *    input device.  The string should be stored in memory after being
 *    read.
 */

_init:
   MOV R13 #0x8000
   B L main
   B _init

main:
   SUB R13 R13 #0x104
   STR R14 R13 #0x100
   MOV R0 R13
   B L gets
   LDR R14 R13 #0x100
   ADD R13 R13 #0x104
   B R14


gets:
   SUB R13 R13 #0x8
   STR R14 R13
   STR R8 R13 #0x4
   MOV R8 R0

gets_loop:
   B L getchar
   TEQ R0 #0xA
   MOV EQ R0 #0
   STR BYTE R0 R8
   LDR EQ R8 R13 #0x4
   LDR EQ R14 R13
   ADD EQ R13 R13 #0x8
   B EQ R14
   ADD R8 R8 #1
   B gets_loop


getchar:
   MOV R1 #0x00FF00

getchar_loop:
   LDR BYTE R0 R1
   TST R0 #0x1
   LDR NE BYTE R0 R1 #0x2
   MOV NE R2 #0x1
   STR NE BYTE R2 R1
   B NE R14
   TST R0 #0x4
   LDR NE BYTE R0 R1 #0x4
   MOV NE R2 #0x4
   STR NE BYTE R2 R1
   B NE R14
   B getchar_loop