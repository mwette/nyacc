/* 
 * compile
gcc -fpic -shared fheck.c -o fheck.so
 */


#include <stdio.h>

void fh_check_dvec(double *v, int n) {
  for (int i = 0; i < n; i++) {
    printf(" %12.5e", v[i]);
  }
  printf("\n");
}


  
