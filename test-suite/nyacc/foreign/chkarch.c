/*
  CFLAGS = -g -I/opt/local/include/guile/3.0 -Wunused
  LIBS = -L/opt/local/lib -Wl,--rpath=/opt/local/lib -lguile-3.0
  gcc -shared -fPIC -o chkarch.so $CFLAGS $LIBS
*/

#include <stdint.h>
#include <string.h>
#include <wchar.h>
#include <uchar.h>
#include <libguile.h>

SCM_DEFINE(scm_arch_sizeof, "arch-sizeof", 1, 0, 0,
	   (SCM typesym),
           "")
#define FUNC_NAME s_scm_arch_sizeof
{
  size_t size;
  char *arg;

  if (!scm_is_symbol(typesym)) {
    return SCM_BOOL_F;
  }
  arg = scm_to_locale_string(scm_symbol_to_string(typesym));
  
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
#if 0
  } else if (strcmp("long-double", arg) == 0) {
    size = sizeof(long double);
  } else if (strcmp("_Float16", arg) == 0) {
    size = sizeof(_Float16);
  } else if (strcmp("_Float128", arg) == 0) {
    size = sizeof(_Float128);
  } else if (strcmp("float-_Complex", arg) == 0) {
    size = sizeof(float _Complex);
  } else if (strcmp("double-_Complex", arg) == 0) {
    size = sizeof(double _Complex);
  } else if (strcmp("long-double-_Complex", arg) == 0) {
    size = sizeof(long double _Complex);
  } else if (strcmp("__int128", arg) == 0) {
    size = sizeof(__int128);
  } else if (strcmp("unsigned-__int128", arg) == 0) {
    size = sizeof(unsigned __int128);
#endif
  } else {
    size = 0;
  }
  return scm_from_size_t(size);
}
#undef FUNC_NAME


SCM_DEFINE(scm_arch_alignof, "arch-alignof", 1, 0, 0,
	   (SCM typesym),
           "")
#define FUNC_NAME s_scm_arch_alignof
{
  size_t almt;
  char *arg;

  if (!scm_is_symbol(typesym)) {
    return SCM_BOOL_F;
  }
  arg = scm_to_locale_string(scm_symbol_to_string(typesym));
  
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
#if 0
  } else if (strcmp("long-double", arg) == 0) {
    almt = __alignof__(long double);
  } else if (strcmp("_Float16", arg) == 0) {
    almt = __alignof__(_Float16);
  } else if (strcmp("_Float128", arg) == 0) {
    almt = __alignof__(_Float128);
  } else if (strcmp("float-_Complex", arg) == 0) {
    size = __alignof__(float _Complex);
  } else if (strcmp("double-_Complex", arg) == 0) {
    size = __alignof__(double _Complex);
  } else if (strcmp("long-double-_Complex", arg) == 0) {
    size = __alignof__(long double _Complex);
  } else if (strcmp("__int128", arg) == 0) {
    size = __alignof__(__int128);
  } else if (strcmp("unsigned-__int128", arg) == 0) {
    size = __alignof__(unsigned __int128);
#endif
  } else {
    almt = 0;
  }
  return scm_from_size_t(almt);
}
#undef FUNC_NAME


void chkarch_init() {
  scm_c_define_gsubr(s_scm_arch_sizeof, 1,0,0, (scm_t_subr)scm_arch_sizeof);
  scm_c_define_gsubr(s_scm_arch_alignof, 1,0,0, (scm_t_subr)scm_arch_alignof);
}

/* --- last line --- */
