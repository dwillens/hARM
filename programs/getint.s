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
   B _init

main:
   SUB R13 R13 #0x4
   STR R14 R13
   B L getint
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
   B LO getint_exit
   CMP R0 #0x9
   B HI getint_exit

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
   LDR EQ BYTE R0 R1 #0x2
   B EQ R14
   TST R0 #0x4
   LDR EQ BYTE R0 R1 #0x4
   B EQ R14
   B getchar_loop
