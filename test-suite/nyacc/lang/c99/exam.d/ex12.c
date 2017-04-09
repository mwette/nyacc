#define foo1( X , Y ) ((X)*(Y))
#define foo(A,B,...)  foo1(A,B)+bar(0, A, B, __VA_ARGS__)
int x = foo(1, 2, 3, 4, foo1(2,3));

#define fix1(x) #x
static char* y = fix1(bar);

#define fix2(X) ex12_ ## X
int z = fix2(abc2);
