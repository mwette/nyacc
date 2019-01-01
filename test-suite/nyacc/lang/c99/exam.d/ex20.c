/* ex20.c -- test of attribute-specifiers */

struct __packed__ case01 { int x; double y; };

struct __packed__ case02;

typedef struct __packed__ { int *ip; double x; } case03;

typedef int case04 __attribute__ ((__deprecated__));

extern double case05(int x) 
   __attribute__ ((__nothrow__, __leaf__))
   __attribute__ ((__const__));

void (__attribute__((noreturn)) ****case06) (void);
char *__attribute__((aligned(8))) *case07;

int case08 __attribute__ ((aligned (16))) = 0;

__attribute__((noreturn)) void case09 (void),
  __attribute__((format(printf, 1, 2))) case10 (const char *, ...),
  case11 (void);

int case12 __attribute__((io(0x123)));

struct case13  __attribute__ ((vector_size (16))) case14;

struct case14 { char a; int x[2] __attribute__ ((packed)); };

struct case15 { int x[2] __attribute__ ((aligned (8))); };

short case16[3] __attribute__ ((aligned (__BIGGEST_ALIGNMENT__)));

struct event { int events; void *data; } __attribute__ ((__packed__));

typedef struct {
  long long __max_align_ll 
      __attribute__((__aligned__(__alignof__(long long))));
  long double __max_align_ld 
      __attribute__((__aligned__(__alignof__(long double))));
} max_align_t;

int sscanf (const char *__restrict __s, const char *__restrict __format, ...)
   __asm__ ( "" "__isoc99_sscanf") __attribute__ ((__nothrow__ , __leaf__));

/* --- last line --- */
