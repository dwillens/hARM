/* 
 * 3. (10 Points) Write a program in your assembly language to input a
 *    single character from the input device.  The character will be
 *    returned in a register or in memory as appropriate for your
 *    architecture.
 */

main:
   B L getchar
   MOV R4 R0
   B main

getchar:
   MOV R1 #0x00FF00

getchar_loop:
   LDR BYTE R0 R1
   TST R0 #0x1
   LDR EQ BYTE R0 R1 #0x2
   B EQ R14
   TST R0 #0x4
   LDR EQ BYTE R0 R1 #0x4
   B EQ R14
   B getchar_loop
