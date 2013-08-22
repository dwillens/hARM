/*
 * 8. (50 Points) Write a program in your assembly language to multiply
 *    two 16-bit signed integers (the multiplicand multiplied by the
 *    multiplier) and return the resulting 16-bit integer product.
 */

_init:
   MOV R13 #0x8000
   B L main
   B _init

main:
   SUB R13 R13 #0x4
   STR R14 R13
   MOV R0 #0xA
   MVN R1 #0xF
   B L product
   LDR R14 R13
   B R14

/*
int product(int n, int m) {
   int p = 0;
   unsigned char neg = 0;
   if (n == 0) {
      return 0;
   }
   if (n < 0) {
      n = 0 - n;
      neg = 1;
   }
   do {
      while ((n & 0x1) == 0) {
         n = n >> 1;
         m = m << 1;
      }
      p = p + m;
      n = n & ~0x1;      
   } while (n != 0);
   if (neg != 0) {
      return 0 - p;
   }
   return p;
}
*/

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
   