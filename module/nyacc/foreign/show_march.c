/* show_march.c - generate machine info for arch-info.scm file
 *
 * usage:
 *   $ gcc show_march.c
 *   $ ./a.out > march.`uname -m`
 *
 * M.Wette - Jan 2026
 */
//#define USE_ISOC11
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <uchar.h>
#include <complex.h>
#include <sys/utsname.h>

size_t sizeof_type_named(const char *arg) {
  size_t size;
  
  if (strcmp("void*", arg) == 0) {
    size = sizeof(void*);
  } else if (strcmp("char", arg) == 0) {
    size = sizeof(char);
  } else if (strcmp("signed-char", arg) == 0) {
    size = sizeof(signed char);
  } else if (strcmp("unsigned-char", arg) == 0) {
    size = sizeof(unsigned char);
  } else if (strcmp("short", arg) == 0) {
    size = sizeof(short);
  } else if (strcmp("unsigned-short", arg) == 0) {
    size = sizeof(unsigned short);
  } else if (strcmp("int", arg) == 0) {
    size = sizeof(int);
  } else if (strcmp("unsigned", arg) == 0) {
    size = sizeof(unsigned);
  } else if (strcmp("long", arg) == 0) {
    size = sizeof(long);
  } else if (strcmp("unsigned-long", arg) == 0) {
    size = sizeof(unsigned long);
  } else if (strcmp("long-long", arg) == 0) {
    size = sizeof(long long);
  } else if (strcmp("unsigned-long-long", arg) == 0) {
    size = sizeof(unsigned long long);
  } else if (strcmp("float", arg) == 0) {
    size = sizeof(float);
  } else if (strcmp("double", arg) == 0) {
    size = sizeof(double);
  } else if (strcmp("int8_t", arg) == 0) {
    size = sizeof(int8_t);
  } else if (strcmp("uint8_t", arg) == 0) {
    size = sizeof(uint8_t);
  } else if (strcmp("int16_t", arg) == 0) {
    size = sizeof(int16_t);
  } else if (strcmp("uint16_t", arg) == 0) {
    size = sizeof(uint16_t);
  } else if (strcmp("int32_t", arg) == 0) {
    size = sizeof(int32_t);
  } else if (strcmp("uint32_t", arg) == 0) {
    size = sizeof(uint32_t);
  } else if (strcmp("int64_t", arg) == 0) {
    size = sizeof(int64_t);
  } else if (strcmp("uint64_t", arg) == 0) {
    size = sizeof(uint64_t);
  } else if (strcmp("size_t", arg) == 0) {
    size = sizeof(size_t);
  } else if (strcmp("ssize_t", arg) == 0) {
    size = sizeof(ssize_t);
  } else if (strcmp("ptrdiff_t", arg) == 0) {
    size = sizeof(ptrdiff_t);
  } else if (strcmp("intptr_t", arg) == 0) {
    size = sizeof(intptr_t);
  } else if (strcmp("uintptr_t", arg) == 0) {
    size = sizeof(uintptr_t);
  } else if (strcmp("_Bool", arg) == 0) {
    size = sizeof(_Bool);
  } else if (strcmp("bool", arg) == 0) {
    size = sizeof(_Bool);
  } else if (strcmp("wchar_t", arg) == 0) {
    size = sizeof(wchar_t);
  } else if (strcmp("char16_t", arg) == 0) {
    size = sizeof(char16_t);
  } else if (strcmp("char32_t", arg) == 0) {
    size = sizeof(char32_t);
#ifdef __SIZEOF_LONG_DOUBLE__
  } else if (strcmp("long-double", arg) == 0) {
    size = sizeof(long double);
#endif
#ifdef __FLT16_MIN_EXP__
  } else if (strcmp("_Float16", arg) == 0) {
    size = sizeof(_Float16);
#endif
#ifdef __FLT128_MIN_EXP__
  } else if (strcmp("_Float128", arg) == 0) {
    size = sizeof(_Float128);
#endif
#ifdef __STDC_IEC_559_COMPLEX__x
  } else if (strcmp("float-_Complex", arg) == 0) {
    size = sizeof(float _Complex);
  } else if (strcmp("double-_Complex", arg) == 0) {
    size = sizeof(double _Complex);
  } else if (strcmp("long-double-_Complex", arg) == 0) {
    size = sizeof(long double _Complex);
#endif
#ifdef __SIZEOF_INT128__
  } else if (strcmp("__int128", arg) == 0) {
    size = sizeof(__int128);
  } else if (strcmp("unsigned-__int128", arg) == 0) {
    size = sizeof(unsigned __int128);
#endif
  } else if (strcmp("unsigned-int", arg) == 0) {
    size = sizeof(unsigned int);
  } else {
    //printf("   ;; sizeof missed %s\n", arg);
    size = 0;
  }
  return size;
}

#define IS_SIGNED(TYPE) (((TYPE)-1) < 0)

char kindof_type_named(const char *arg) {
  char kind;

  if (strcmp("void*", arg) == 0) {
    kind = IS_SIGNED(void*) ? 's' : 'u';
  } else if (strcmp("char", arg) == 0) {
    kind = IS_SIGNED(char) ? 's' : 'u';
  } else if (strcmp("signed-char", arg) == 0) {
    kind = IS_SIGNED(signed char) ? 's' : 'u';
  } else if (strcmp("unsigned-char", arg) == 0) {
    kind = IS_SIGNED(unsigned char) ? 's' : 'u';
  } else if (strcmp("short", arg) == 0) {
    kind = IS_SIGNED(short) ? 's' : 'u';
  } else if (strcmp("unsigned-short", arg) == 0) {
    kind = IS_SIGNED(unsigned short) ? 's' : 'u';
  } else if (strcmp("int", arg) == 0) {
    kind = IS_SIGNED(int) ? 's' : 'u';
  } else if (strcmp("unsigned", arg) == 0) {
    kind = IS_SIGNED(unsigned) ? 's' : 'u';
  } else if (strcmp("long", arg) == 0) {
    kind = IS_SIGNED(long) ? 's' : 'u';
  } else if (strcmp("unsigned-long", arg) == 0) {
    kind = IS_SIGNED(unsigned long) ? 's' : 'u';
  } else if (strcmp("long-long", arg) == 0) {
    kind = IS_SIGNED(long long) ? 's' : 'u';
  } else if (strcmp("unsigned-long-long", arg) == 0) {
    kind = IS_SIGNED(unsigned long long) ? 's' : 'u';
  } else if (strcmp("float", arg) == 0) {
    kind = 'f';
  } else if (strcmp("double", arg) == 0) {
    kind = 'f';
  } else if (strcmp("int8_t", arg) == 0) {
    kind = IS_SIGNED(int8_t) ? 's' : 'u';
  } else if (strcmp("uint8_t", arg) == 0) {
    kind = IS_SIGNED(uint8_t) ? 's' : 'u';
  } else if (strcmp("int16_t", arg) == 0) {
    kind = IS_SIGNED(int16_t) ? 's' : 'u';
  } else if (strcmp("uint16_t", arg) == 0) {
    kind = IS_SIGNED(uint16_t) ? 's' : 'u';
  } else if (strcmp("int32_t", arg) == 0) {
    kind = IS_SIGNED(int32_t) ? 's' : 'u';
  } else if (strcmp("uint32_t", arg) == 0) {
    kind = IS_SIGNED(uint32_t) ? 's' : 'u';
  } else if (strcmp("int64_t", arg) == 0) {
    kind = IS_SIGNED(int64_t) ? 's' : 'u';
  } else if (strcmp("uint64_t", arg) == 0) {
    kind = IS_SIGNED(uint64_t) ? 's' : 'u';
  } else if (strcmp("size_t", arg) == 0) {
    kind = IS_SIGNED(size_t) ? 's' : 'u';
  } else if (strcmp("ssize_t", arg) == 0) {
    kind = IS_SIGNED(ssize_t) ? 's' : 'u';
  } else if (strcmp("ptrdiff_t", arg) == 0) {
    kind = IS_SIGNED(ptrdiff_t) ? 's' : 'u';
  } else if (strcmp("intptr_t", arg) == 0) {
    kind = IS_SIGNED(intptr_t) ? 's' : 'u';
  } else if (strcmp("uintptr_t", arg) == 0) {
    kind = IS_SIGNED(uintptr_t) ? 's' : 'u';
  } else if (strcmp("_Bool", arg) == 0) {
    kind = IS_SIGNED(_Bool) ? 's' : 'u';
  } else if (strcmp("bool", arg) == 0) {
    kind = IS_SIGNED(_Bool) ? 's' : 'u';
  } else if (strcmp("wchar_t", arg) == 0) {
    kind = IS_SIGNED(wchar_t) ? 's' : 'u';
  } else if (strcmp("char16_t", arg) == 0) {
    kind = IS_SIGNED(char16_t) ? 's' : 'u';
  } else if (strcmp("char32_t", arg) == 0) {
    kind = IS_SIGNED(char32_t) ? 's' : 'u';
  } else if (strcmp("long-double", arg) == 0) {
    kind = 'f';
  } else if (strcmp("_Float16", arg) == 0) {
    kind = 'f';
  } else if (strcmp("_Float128", arg) == 0) {
    kind = 'f';
  } else if (strcmp("float-_Complex", arg) == 0) {
    kind = 'c';
  } else if (strcmp("double-_Complex", arg) == 0) {
    kind = 'c';
  } else if (strcmp("long-double-_Complex", arg) == 0) {
    kind = 'c';
  } else if (strcmp("__int128", arg) == 0) {
    kind = 's';
  } else if (strcmp("unsigned-__int128", arg) == 0) {
    kind = 'u';
  } else if (strcmp("unsigned-int", arg) == 0) {
    kind = 'u';
  } else {
    printf("   ;; kindof missed %s\n", arg);
    kind = '!';
  }
  return kind;
}

const char * mtypeof_type_named(const char *arg) {
  static char buf[128];
  size_t siz = sizeof_type_named(arg);
  char kind = kindof_type_named(arg);
  char nd;
  
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  nd = 'b';
#else
  nd = 'l';
#endif

  if (siz == 0) {
    snprintf(buf,128, "#f");
  } else if (siz == 1) {
    snprintf(buf,128, "%c%ld", kind, 8*siz);
  } else if (kind == 'c') {
    snprintf(buf,128, "%c%ld%ce", kind, 8*siz/2, nd);
  } else {
    snprintf(buf,128, "%c%ld%ce", kind, 8*siz, nd);
  }
  return (char*) buf;
}

size_t alignof_type_named(const char *arg) {
  size_t almt;
  
  if (strcmp("void*", arg) == 0) {
    almt = __alignof__(void*);
  } else if (strcmp("char", arg) == 0) {
    almt = __alignof__(char);
  } else if (strcmp("signed-char", arg) == 0) {
    almt = __alignof__(signed char);
  } else if (strcmp("unsigned-char", arg) == 0) {
    almt = __alignof__(unsigned char);
  } else if (strcmp("short", arg) == 0) {
    almt = __alignof__(short);
  } else if (strcmp("unsigned-short", arg) == 0) {
    almt = __alignof__(unsigned short);
  } else if (strcmp("int", arg) == 0) {
    almt = __alignof__(int);
  } else if (strcmp("unsigned", arg) == 0) {
    almt = __alignof__(unsigned);
  } else if (strcmp("long", arg) == 0) {
    almt = __alignof__(long);
  } else if (strcmp("unsigned-long", arg) == 0) {
    almt = __alignof__(unsigned long);
  } else if (strcmp("long-long", arg) == 0) {
    almt = __alignof__(long long);
  } else if (strcmp("unsigned-long-long", arg) == 0) {
    almt = __alignof__(unsigned long long);
  } else if (strcmp("float", arg) == 0) {
    almt = __alignof__(float);
  } else if (strcmp("double", arg) == 0) {
    almt = __alignof__(double);
  } else if (strcmp("int8_t", arg) == 0) {
    almt = __alignof__(int8_t);
  } else if (strcmp("uint8_t", arg) == 0) {
    almt = __alignof__(uint8_t);
  } else if (strcmp("int16_t", arg) == 0) {
    almt = __alignof__(int16_t);
  } else if (strcmp("uint16_t", arg) == 0) {
    almt = __alignof__(uint16_t);
  } else if (strcmp("int32_t", arg) == 0) {
    almt = __alignof__(int32_t);
  } else if (strcmp("uint32_t", arg) == 0) {
    almt = __alignof__(uint32_t);
  } else if (strcmp("int64_t", arg) == 0) {
    almt = __alignof__(int64_t);
  } else if (strcmp("uint64_t", arg) == 0) {
    almt = __alignof__(uint64_t);
  } else if (strcmp("size_t", arg) == 0) {
    almt = __alignof__(size_t);
  } else if (strcmp("ssize_t", arg) == 0) {
    almt = __alignof__(ssize_t);
  } else if (strcmp("ptrdiff_t", arg) == 0) {
    almt = __alignof__(ptrdiff_t);
  } else if (strcmp("intptr_t", arg) == 0) {
    almt = __alignof__(intptr_t);
  } else if (strcmp("uintptr_t", arg) == 0) {
    almt = __alignof__(uintptr_t);
  } else if (strcmp("_Bool", arg) == 0) {
    almt = __alignof__(_Bool);
  } else if (strcmp("bool", arg) == 0) {
    almt = __alignof__(_Bool);
  } else if (strcmp("wchar_t", arg) == 0) {
    almt = __alignof__(wchar_t);
  } else if (strcmp("char16_t", arg) == 0) {
    almt = __alignof__(char16_t);
  } else if (strcmp("char32_t", arg) == 0) {
    almt = __alignof__(char32_t);
#ifdef __SIZEOF_LONG_DOUBLE__
  } else if (strcmp("long-double", arg) == 0) {
    almt = __alignof__(long double);
#endif
#ifdef __FLT16_MIN_EXP__
  } else if (strcmp("_Float16", arg) == 0) {
    almt = __alignof__(_Float16);
#endif
#ifdef __FLT128_MIN_EXP__
  } else if (strcmp("_Float128", arg) == 0) {
    almt = __alignof__(_Float128);
#endif
#ifdef __STDC_IEC_559_COMPLEX__
  } else if (strcmp("float-_Complex", arg) == 0) {
    almt = __alignof__(float _Complex);
  } else if (strcmp("double-_Complex", arg) == 0) {
    almt = __alignof__(double _Complex);
  } else if (strcmp("long-double-_Complex", arg) == 0) {
    almt = __alignof__(long double _Complex);
#endif
#ifdef __SIZEOF_INT128__
  } else if (strcmp("__int128", arg) == 0) {
    almt = __alignof__(__int128);
  } else if (strcmp("unsigned-__int128", arg) == 0) {
    almt = __alignof__(unsigned __int128);
#endif
  } else if (strcmp("unsigned-int", arg) == 0) {
    almt = __alignof__(unsigned int);
  } else {
    printf("   ;; alignof missed %s\n", arg);
    almt = 0;
  }
  return almt;
}

const char *types[] = {
  "void*", "char", "signed char", "unsigned char", "short", "unsigned short",
  "int", "unsigned", "long", "unsigned long", "long long", "unsigned long long",
  "float", "double", "int8_t", "uint8_t", "int16_t", "uint16_t", "int32_t",
  "uint32_t", "int64_t", "uint64_t", "size_t", "ssize_t", "ptrdiff_t",
  "intptr_t", "uintptr_t", "_Bool", "bool", "wchar_t", "char16_t", "char32_t",
  "long double", "_Float16", "_Float128", "float _Complex", "double _Complex",
  "long double _Complex", "__int128", "unsigned __int128", "unsigned int",
};

const char *tspc[] = {
  "\n   ", " ", " ", "\n   ", " ", "\n   ", " ", "\n   ", " ", "\n   ", 
  " ", "\n   ", " ", "\n   ", " ", " ", " ", "\n   ", " ", " ", " ", "\n   ", 
  " ", "\n   ", " ", " ", "\n   ", " ", "\n   ", " ", " ", "\n   ", 
  " ", " ", "\n   ", " ", "\n   ", "\n   ", " ", "\n   ", "",
  "\n", "\n", "\n", 
};

const char *almts[] = {
  "int8_t", "uint8_t", "int16_t", "uint16_t", "int32_t", "uint32_t",
  "int64_t", "uint64_t", "float", "double", "long double", "_Float16",
  "float _Complex", "double _Complex", "long double _Complex",
  "__int128", "unsigned __int128",
};

char *symform(const char* name) {  
int ix;
  static char buf[128];

  for (ix = 0; *name; ix++) {
    buf[ix] = *name == ' ' ?  '-': *name;
    name++;
  }
  buf[ix] = '\0';
  return (char *)buf;
}

int main(int argc, char *argv[]) {
  char *symname;
  struct utsname utsn;
  char machname[64];

  if (uname(&utsn) != 0) abort();
  //printf("sysname=%s\n", utsn.sysname); => "Linux"
  strncpy(machname, utsn.machine, 64);

  printf("(define mtype-map/%s\n", machname);
  printf(" '(");
  for (int ix = 0; ix < sizeof(types)/sizeof(char*); ix++) {
    symname = symform(types[ix]);
    printf("(%s . %s)%s", symname, mtypeof_type_named(symname), tspc[ix]);
  }
  printf("))\n");
  printf("\n");
  printf("(define align-map/%s\n", machname);
  printf(" '(");
  for (int ix = 0; ix < sizeof(almts)/sizeof(char*); ix++) {
    symname = symform(almts[ix]);
    if (strcmp(mtypeof_type_named(symname), "#f") != 0) {
      if (ix > 0) printf("\n   ");
      printf("(%s . %ld)",
	     mtypeof_type_named(symname), alignof_type_named(symname));
    }
  }
  printf("))\n\n");
}


/* --- last line --- */
