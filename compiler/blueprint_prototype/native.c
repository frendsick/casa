#include <stdint.h>

struct Pair { int64_t first, second; };
int64_t pair_value(struct Pair value) { return value.first * 10 + value.second; }
struct Pair pair_return(int64_t value) { return (struct Pair){value, value + 1}; }
int64_t sum_seven(int64_t a, int64_t b, int64_t c, int64_t d,
                  int64_t e, int64_t f, int64_t g) {
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g;
}
