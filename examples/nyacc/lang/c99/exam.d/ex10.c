// bug C99-001
#define ABC 123 /* this is a var */
#if ABC > 100
# error "bla"
#endif

