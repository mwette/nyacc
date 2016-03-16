
int foo(int k) {
  switch (k) {
  case 1: k = 11; break;
  case 2: k = 12; break;
  default: k = 0; break;
  }
  return k;
}
