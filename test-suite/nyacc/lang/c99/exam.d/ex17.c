// provided by Jan Nieuwenhuizen
// worked in 0.80.4, broken in 0.82.1

#define DEF_BWL(x) DEF(TOK_ASM_ ## x, #x)

#define DEF_BWLX DEF_BWL

enum tcc_token {
    TOK_LAST
#define DEF(id, str) ,id
DEF_BWLX(mov)
#undef DEF
};

