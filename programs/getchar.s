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
