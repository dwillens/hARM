/*
 * 7. (25 Points) Write a program in your assembly language to input a
 *    signed decimal integer from the input device.  The value of the
 *    number should be converted into a signed 2-byte (16-bit) integer in
 *    two's complement representation.  The integer will be returned in a
 *    register or in memory as appropriate for your architecture.
 */

_init:
   MOV R13 #0x8000
   B L main
   SWI #-1

main:
   SUB R13 R13 #0x4
   STR R14 R13
   B L getint
   B L putint
   MOV R0 #0xA
   B L putchar
   LDR R14 R13
   B R14

/*
int getint(void) {
   unsigned char d, neg = 0;
   int n = 0;

   d = getchar();
   if (d == '-') {
      neg = 1;
      d = getchar();
   }
   while (d >= '0' && d <= '9') {
      int k = n + n;
      n = k + k;
      n = n + n;
      n = n + k;
      d = d - '0';
      n = n + d;
      d = getchar();
   }
   if (neg != 0) {
      return 0 - n;
   }
   return n;
}
*/

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
