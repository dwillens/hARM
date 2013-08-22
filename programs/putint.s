/*
 * 6. (25 Points) Write a program in your assembly language to output a
 *    signed decimal integer on the output device.  Prior to outputting
 *    the value of the number, it will be stored as a signed 2-byte
 *    (16-bit) integer in two's-complement representation.  The integer
 *    may be stored in a register or in memory as appropriate for your
 *    architecture.
 */

_init:
   MOV R13 #0x8000
   B L main
   B _init

main:
   SUB R13 R13 #0x4
   STR R14 R13
   MOV R0    #0x34000000
   ADD R0 R0 #0x003E0000
   ADD R0 R0 #0x0000FC00
   ADD R0 R0 #0x000000EA
   B L putint
   LDR R14 R13
   B R14


/*
void putint(int n) {
   unsigned pos = 9;
   unsigned m = 100 * 1000 * 1000;
   unsigned char d = '0';
   if (n < 0) {
      n = 0 - n;
      putchar('-');
   }
   do {
      while (m <= n) {
         n = n - m;
         d++;
      }
      putchar(d);
      {
         int k = n + n;
         n = k + k;
         n = n + n;
         n = n + k;
         d = '0';
         pos = pos - 1;
      }
   } while (pos != 0);
}
*/

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
   MOV R0 #0x30

   MOV S R8 R0
   RSB MI R8 R0 #0
   MOV MI R0 #0x2D
   B MI L putchar

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
   B R14

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
