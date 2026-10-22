#include <stdint.h>

typedef struct {
    uint64_t words[49152];
} Large;

void casa_extern_take_large(Large value) { (void)value; }
