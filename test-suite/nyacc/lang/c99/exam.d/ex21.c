int sscanf (const char *__restrict __s, const char *__restrict __format, ...)
   __asm__ ( "" "__isoc99_sscanf");

int foo1() {
  __asm__ (
      "idivl  %[divsrc]"
      : "=a" (quotient), "=d" (rem)
      : "d" (hi), "a" (lo), [divsrc] "rm" (divisor)
      :
      );
}

int foo2() {
  __asm (
      "idivl  %[divsrc]"
      : "=a" (quotient), "=d" (rem)
      : "d" (hi), "a" (lo), [divsrc] "rm" (divisor)
      :
      );
}

int foo3() {
  asm (
      "idivl  %[divsrc]"
      : "=a" (quotient), "=d" (rem)
      : "d" (hi), "a" (lo), [divsrc] "rm" (divisor)
      :
      );
}

