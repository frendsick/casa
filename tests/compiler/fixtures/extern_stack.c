#include <stdint.h>

int64_t casa_extern_stack_integers(int64_t first, int64_t second,
                                   int64_t third, int64_t fourth,
                                   int64_t fifth, int64_t sixth,
                                   int64_t seventh, int64_t eighth) {
    return first * 10000000 + second * 1000000 + third * 100000 +
           fourth * 10000 + fifth * 1000 + sixth * 100 + seventh * 10 + eighth;
}

int64_t casa_extern_stack_floats(double first, double second, double third,
                                 double fourth, double fifth, double sixth,
                                 double seventh, double eighth, double ninth,
                                 double tenth) {
    return first == 10.0 && second == 9.0 && third == 8.0 && fourth == 7.0 &&
           fifth == 6.0 && sixth == 5.0 && seventh == 4.0 && eighth == 3.0 &&
           ninth == 2.0 && tenth == 1.0;
}
