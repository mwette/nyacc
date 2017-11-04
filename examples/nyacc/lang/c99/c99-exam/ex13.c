typedef signed int t;

typedef int plain;

struct tag {
  unsigned t:4;
  const t:5;
  plain r:5;
};

int foo() {
  t f(t(t));
  long t;
  return 1;
}
