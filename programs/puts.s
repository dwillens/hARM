/*
 * 4. (15 Points) Write a program in your assembly language to output a
 *    sequence of characters (i.e., a string) stored in memory on the
 *    output device.
 */

_init:
   MOV R13 #0x8000
   B L main
   B _init

main:
   SUB R13 R13 #0x4
   STR R14 R13
   .la R0 str
   B L puts
   LDR R14 R13
   ADD R13 R13 #0x4
   B R14


str:
   .string "Hello, world!!\n\0"


puts:
   SUB R13 R13 #0x8
   STR R14 R13
   STR R8 R13 #0x4
   MOV R8 R0

puts_loop:
   LDR BYTE R0 R8
   TST R0 R0
   LDR EQ R8 R13 #0x4
   LDR EQ R14 R13
   B EQ R14
   B L putchar
   ADD R8 R8 #1
   B puts_loop


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
   BIC NE R2 R2 #0x2

   TST R3 #0x8
   STR NE BYTE R0 R1 #0x4
   BIC NE R2 R2 #0x8

   B putchar_loop
