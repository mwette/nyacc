#define ELF32_ST_INFO(bind, type)	(((bind) << 4) + ((type) & 0xf))
#define ELFW(type) ELF##32##_##type
int x = ELFW(ST_INFO)(1, 0);
