/* 
 * 3. (10 Points) Write a program in your assembly language to input a
 *    single character from the input device.  The character will be
 *    returned in a register or in memory as appropriate for your
 *    architecture.
 */

_init:
   MOV R13 #0x8000
   B L main
   SWI #-1

main:
   B L getchar_echo
   B main

getchar_echo:
   SUB R13 R13 #0x8
   STR R14 R13
   STR R8 R13 #0x4

   B L getchar
   MOV R8 R0
   B L putchar
   MOV R0 R8

   LDR R8 R13 #0x4
   LDR R14 R13
   ADD R13 R13 #0x8
   B R14

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

putchar:
   MOV R1 #0xFF00
   MOV R2 #0xA

putchar_loop:
   TST R2 #0xA
   B EQ R14

   LDR BYTE R3 R1
   AND R3 R3 R2
   TST R3 #0x2
   STR NE BYTE R0 R1 #0x2
   MOV NE R0 #0x2
   STR NE BYTE R0 R1 #0x0
   BIC NE R2 R2 #0x2

   TST R3 #0x8
   STR NE BYTE R0 R1 #0x4
   MOV NE R0 #0x8
   STR NE BYTE R0 R1 #0x0
   BIC NE R2 R2 #0x8

   B putchar_loop
