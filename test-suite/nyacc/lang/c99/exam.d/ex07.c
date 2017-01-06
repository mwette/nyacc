//#include "inc.h"
int r = 1;
typedef struct {
  int x;
#if 0
  int y;
#endif
  int z;
} foo_t;
int foo () {
  int r = 1;
  #include "ex07.i"
  return r;
} 
