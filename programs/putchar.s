/*
 * 2. (10 Points) Write a program in your assembly language to output a
 *    single character on the output device.  The character may be stored
 *    in a register or in memory as appropriate for your architecture.
 */

main:
   MOV R0 #65
   B L putchar
   B main

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
