#define foo1(X, Y) (X+Y)
int x = foo1(1,2);
/*
#define foo(A, B, ...)  foo1(A,B)+bar(__FILE__, A, B, __VA_ARGS__)

int x = foo(1, 2, 3, 4, foo1(2,3));
#define fix1(x) #x
char *abc1 = fix1(bar);

#define fix2(X) _ ## X
int fix(abc2) = 1;
*/
