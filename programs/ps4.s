/*
 * 9. (20 Points) Write a program in your assembly language to prompt
 *    (i.e. output a string to request input) a user to enter two signed
 *    decimal 2-byte (16-bit) integers and to multiply one by the other
 *    and then to output the resulting 2-byte integer product in decimal.
 */

_init:
   MOV R13 #0x8000
   B L main
   B _init

main:
   SUB R13 R13 #0x8
   STR R14 R13
   STR R8 R13 #0x4

   .la R0 prompt
   B L puts

   B L getint
   MOV R8 R0

   B L getint

   MOV R1 R8
   B L product

   B L putint

   MOV R0 #0xA
   B L putchar

   LDR R8 R13 #0x4
   LDR R14 R13
   ADD R13 R13 #0x8
   B R14

prompt:
   .string "\n\nEnter two numbers: \n\0"


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
   ADD EQ R13 R13 #0x8
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
   MOV NE R0 #0x2
   STR NE BYTE R0 R1 #0x0
   BIC NE R2 R2 #0x2

   TST R3 #0x8
   STR NE BYTE R0 R1 #0x4
   MOV NE R0 #0x8
   STR NE BYTE R0 R1 #0x0
   BIC NE R2 R2 #0x8

   B putchar_loop


getint:
   SUB R13 R13 #0xC
   STR R14 R13
   STR R8 R13 #0x4
   STR R9 R13 #0x8
   MOV R8 #0x0
   MOV R9 #0x0

   B L getchar
   CMP R0 #0x2D
   MOV EQ R9 #0x1
   B EQ L getchar

getint_loop:
   SUB S R0 R0 #0x30
   B HS getint_exit
   CMP R0 #0xA
   B LS getint_exit

   ADD R1 R8 R8
   ADD R8 R1 R1
   ADD R8 R8 R8
   ADD R8 R8 R1
   ADD R8 R8 R0

   B L getchar
   B getint_loop

getint_exit:
   TST R9 R9
   RSB NE R0 R8 #0
   MOV EQ R0 R8

   LDR R9 R13 #0x8
   LDR R8 R13 #0x4
   LDR R14 R13
   ADD R13 R13 #0xC
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


product:
   MOV R2 #0
   MOV R3 #0

   TST R0 R0
   B EQ R14
   RSB MI R0 R0 #0 
   MOV MI R3 #1

product_loop:
   TST R0 #0x1
   MOV EQ R0 R0 LSR #0x1
   MOV EQ R1 R1 LSL #0x1
   B EQ product_loop

   ADD R2 R2 R1
   BIC S R0 R0 #0x1
   B NE product_loop

   TST R3 R3
   RSB NE R0 R2 #0
   MOV EQ R0 R2
   B R14


putint:
   SUB R13 R13 #0x10
   STR R14 R13
   STR R8 R13 #0x4
   STR R9 R13 #0x8
   STR R10 R13 #0xC
   MOV R9 #9
   MOV R10     #0x05F00000
   ADD R10 R10 #0x0005E000
   ADD R10 R10 #0x00000100

   MOV S R8 R0
   RSB MI R8 R0 #0
   MOV MI R0 #0x2D
   B MI L putchar

   MOV R0 #0x30
putint_loop:
   CMP R8 R10
   SUB LS R8 R8 R10
   ADD LS R0 R0 #1
   B LS putint_loop

   B L putchar

   ADD R0 R8 R8
   ADD R8 R0 R0
   ADD R8 R8 R8
   ADD R8 R8 R0

   SUB S R9 R9 #1
   MOV NE R0 #0x30
   B NE putint_loop

   LDR R10 R13 #0xC
   LDR R9 R13 #0x8
   LDR R8 R13 #0x4
   LDR R14 R13
   ADD R13 R13 #0x10
   B R14
