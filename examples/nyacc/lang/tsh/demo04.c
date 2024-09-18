// demo04.c - tsh C function

// TshVal:
// int float uniform-array list error


// rust type system: 1i8, defaults are i32 and f64
// @a{ 1.2 2.3 ... } == [array 1.2 2.3 ... ]
// @l{ 1.2 abc ... } == [list 1.2 abc ... ]
// @e"value not good"
// @e123

// coerce implies constraints
// tsh arrays are fixed uniform @a{ 1.2 2.3 3.4 }, @a{ 1u8 }
// tsh lists are not @l{ 1.2 }
// tsh integers => i32 ??
// tsh doubles => f64

for FFI only

typedef struct {
  enum _key;
  union {
    int8_t i8;
    uint8_t u8;
    int16_t i16;
    uint16_t u16;
    int32_t i32;
    uint32_t u32;
    int64_t i64;
    float f32;
    double f64;
    char *str;
    TshErr *err;
  };
} TshVal;

TshVal vsum(TshInterp *tsh, int argc, TshVal argv[]) {
  
}
