#define assert(x) ((x) ? (void)0 : assert_fail (#x))
int foo() {
  assert(boo);
}
