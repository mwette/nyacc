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

typedef enum {
  TSH_T_I8,
  TSH_T_U8,
  TSH_T_I16,
  TSH_T_U16,
  TSH_T_I32,
  TSH_T_U32,
  TSH_T_I64,
  TSH_T_U64,
  TSH_T_F32,
  TSH_T_F64,
  TSH_T_STR,
} tsh_type_t;

typedef struct {
  tsh_type_t key;
  union {
    int8_t i8;
    uint8_t u8;
    int16_t i16;
    uint16_t u16;
    int32_t i32;
    uint32_t u32;
    int64_t i64;
    uint64_t u64;
    float f32;
    double f64;
    char *str;
    //tsh_err_t *err;
  } val;
} tsh_val_t;

tsh_val_t vsum(tsh_interp_t *tsh, int argc, tsh_val_t *argv[]) {
}

