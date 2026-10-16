#include <stdint.h>

int8_t scalar_read_i8(const int8_t *value) { return *value; }
void scalar_write_i8(int8_t *value, int8_t replacement) { *value = replacement; }

int16_t scalar_read_i16(const int16_t *value) { return *value; }
void scalar_write_i16(int16_t *value, int16_t replacement) { *value = replacement; }

int32_t scalar_read_i32(const int32_t *value) { return *value; }
void scalar_write_i32(int32_t *value, int32_t replacement) { *value = replacement; }

int64_t scalar_read_i64(const int64_t *value) { return *value; }
void scalar_write_i64(int64_t *value, int64_t replacement) { *value = replacement; }

uint8_t scalar_read_u8(const uint8_t *value) { return *value; }
void scalar_write_u8(uint8_t *value, uint8_t replacement) { *value = replacement; }

uint16_t scalar_read_u16(const uint16_t *value) { return *value; }
void scalar_write_u16(uint16_t *value, uint16_t replacement) { *value = replacement; }

uint32_t scalar_read_u32(const uint32_t *value) { return *value; }
void scalar_write_u32(uint32_t *value, uint32_t replacement) { *value = replacement; }

uint64_t scalar_read_u64(const uint64_t *value) { return *value; }
void scalar_write_u64(uint64_t *value, uint64_t replacement) { *value = replacement; }

float scalar_read_f32(const float *value) { return *value; }
void scalar_write_f32(float *value, float replacement) { *value = replacement; }

double scalar_read_f64(const double *value) { return *value; }
void scalar_write_f64(double *value, double replacement) { *value = replacement; }

_Bool scalar_read_bool(const _Bool *value) { return *value; }
void scalar_write_bool(_Bool *value, _Bool replacement) { *value = replacement; }

int32_t scalar_sum_eight(const int32_t *a, const int32_t *b, const int32_t *c, const int32_t *d, const int32_t *e, const int32_t *f, const int32_t *g, const int32_t *h) {
    return *a + *b + *c + *d + *e + *f + *g + *h;
}

void *scalar_read_ptr(void *const *value) { return *value; }
void scalar_write_ptr(void **value, void *replacement) { *value = replacement; }
