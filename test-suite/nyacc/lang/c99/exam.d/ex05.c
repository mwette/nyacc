// C99 run-through

int x;
int x,y; 
int x=1, y, z = 3;

struct struct1 {
  int x;
  double d;
};

union {
  int x;
  double d;
} u1;

typedef struct {
  int x;
  struct struct1 z;
} tdef1_t;

typedef struct zzz {
  int x;
  tdef1_t z;
} tdef2_t;

int foo(int x, int /* hello */);

int foo(char *x) {
  int j;

  for (int i = 0; i < 32; i++) {
    j = 1;
    while (j < 3) {
      j += i;
    }
  }
  if (i > 2) { return 4; } else { return 9; } 
}


/* --- last line --- */
