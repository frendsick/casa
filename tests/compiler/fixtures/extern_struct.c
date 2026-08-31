#include <stddef.h>
#include <stdint.h>

typedef struct {
    uint16_t value;
    _Bool flag;
} Tiny;

typedef struct {
    uint8_t tag;
    Tiny tiny;
    uint16_t samples[3];
    float weight;
    void *address;
    _Bool active;
} Packet;

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
} Color;

typedef struct {
    float x;
    float y;
} Vector2;

typedef struct {
    float x;
    float y;
    float width;
    float height;
} Rectangle;

typedef struct {
    uint64_t code;
    double weight;
} Mixed;

typedef struct {
    double ratio;
    uint64_t code;
} MixedReverse;

typedef struct {
    int64_t first;
    int64_t second;
} IntPair;

typedef struct {
    void *data;
    int32_t width;
    int32_t height;
    int32_t mipmaps;
    int32_t format;
} Image;

typedef struct {
    uint32_t id;
    int32_t width;
    int32_t height;
    int32_t mipmaps;
    int32_t format;
} Texture2D;

typedef struct {
    Vector2 offset;
    Vector2 target;
    float rotation;
    float zoom;
} Camera2D;

typedef struct {
    uint8_t tag;
    uint64_t value;
    uint16_t code;
} PaddedLarge;

_Static_assert(sizeof(Tiny) == 4, "Tiny size");
_Static_assert(_Alignof(Tiny) == 2, "Tiny alignment");
_Static_assert(offsetof(Tiny, value) == 0, "Tiny value offset");
_Static_assert(offsetof(Tiny, flag) == 2, "Tiny flag offset");
_Static_assert(sizeof(Packet) == 32, "Packet size");
_Static_assert(_Alignof(Packet) == 8, "Packet alignment");
_Static_assert(offsetof(Packet, tag) == 0, "Packet tag offset");
_Static_assert(offsetof(Packet, tiny) == 2, "Packet tiny offset");
_Static_assert(offsetof(Packet, samples) == 6, "Packet samples offset");
_Static_assert(sizeof(((Packet *)0)->samples) == 6, "Packet samples size");
_Static_assert(offsetof(Packet, weight) == 12, "Packet weight offset");
_Static_assert(offsetof(Packet, address) == 16, "Packet address offset");
_Static_assert(offsetof(Packet, active) == 24, "Packet active offset");
_Static_assert(sizeof(Color) == 4, "Color size");
_Static_assert(sizeof(Vector2) == 8, "Vector2 size");
_Static_assert(sizeof(Rectangle) == 16, "Rectangle size");
_Static_assert(sizeof(Mixed) == 16, "Mixed size");
_Static_assert(sizeof(MixedReverse) == 16, "MixedReverse size");
_Static_assert(sizeof(IntPair) == 16, "IntPair size");
_Static_assert(sizeof(Image) == 24, "Image size");
_Static_assert(_Alignof(Image) == 8, "Image alignment");
_Static_assert(offsetof(Image, width) == 8, "Image width offset");
_Static_assert(offsetof(Image, format) == 20, "Image format offset");
_Static_assert(sizeof(Texture2D) == 20, "Texture2D size");
_Static_assert(_Alignof(Texture2D) == 4, "Texture2D alignment");
_Static_assert(offsetof(Texture2D, format) == 16, "Texture2D format offset");
_Static_assert(sizeof(Camera2D) == 24, "Camera2D size");
_Static_assert(_Alignof(Camera2D) == 4, "Camera2D alignment");
_Static_assert(offsetof(Camera2D, target) == 8, "Camera2D target offset");
_Static_assert(offsetof(Camera2D, rotation) == 16, "Camera2D rotation offset");
_Static_assert(offsetof(Camera2D, zoom) == 20, "Camera2D zoom offset");
_Static_assert(sizeof(PaddedLarge) == 24, "PaddedLarge size");
_Static_assert(_Alignof(PaddedLarge) == 8, "PaddedLarge alignment");
_Static_assert(offsetof(PaddedLarge, value) == 8, "PaddedLarge value offset");
_Static_assert(offsetof(PaddedLarge, code) == 16, "PaddedLarge code offset");

_Bool casa_extern_struct_is_initial(const Packet *value) {
    return value->tag == 7 && value->tiny.value == 513 && value->tiny.flag &&
           value->samples[0] == 10 && value->samples[1] == 20 &&
           value->samples[2] == 30 && value->weight == 1.5f &&
           value->address == NULL && !value->active;
}

_Bool casa_extern_struct_has_casa_mutation(const Packet *value) {
    return value->tag == 8 && value->tiny.value == 514 && !value->tiny.flag &&
           value->samples[0] == 10 && value->samples[1] == 20 &&
           value->samples[2] == 30 && value->weight == 2.5f &&
           value->address == NULL && value->active;
}

void casa_extern_struct_mutate(Packet *value) {
    value->tag = 9;
    value->tiny.value = 1025;
    value->tiny.flag = 1;
    value->samples[0] = 11;
    value->samples[1] = 22;
    value->samples[2] = 33;
    value->weight = 3.5f;
    value->address = (void *)(uintptr_t)0x1234;
    value->active = 0;
}

_Bool casa_extern_struct_has_native_mutation(const Packet *value) {
    return value->tag == 9 && value->tiny.value == 1025 && value->tiny.flag &&
           value->samples[0] == 11 && value->samples[1] == 22 &&
           value->samples[2] == 33 && value->weight == 3.5f &&
           value->address == (void *)(uintptr_t)0x1234 && !value->active;
}

Color casa_extern_color(Color value) {
    Color result = {(uint8_t)(value.r + 1), (uint8_t)(value.g + 2),
                    (uint8_t)(value.b + 3), (uint8_t)(value.a + 4)};
    return result;
}

Vector2 casa_extern_vector(Vector2 value) {
    Vector2 result = {value.x + 1.0f, value.y + 2.0f};
    return result;
}

Rectangle casa_extern_rectangle(Rectangle value) {
    Rectangle result = {value.x + 1.0f, value.y + 2.0f,
                        value.width + 3.0f, value.height + 4.0f};
    return result;
}

Mixed casa_extern_mixed(Mixed value) {
    Mixed result = {value.code + 5, value.weight + 0.5};
    return result;
}

MixedReverse casa_extern_mixed_reverse(MixedReverse value) {
    MixedReverse result = {value.ratio + 0.25, value.code + 7};
    return result;
}

_Bool casa_extern_interleaved(int64_t prefix, Color color, double scale,
                              Vector2 vector, Mixed mixed) {
    return prefix == 41 && color.r == 1 && color.g == 2 && color.b == 3 &&
           color.a == 4 && scale == 6.5 && vector.x == 1.5f &&
           vector.y == 2.5f && mixed.code == 10 && mixed.weight == 1.5;
}

_Bool casa_extern_integer_aggregate_spill(int64_t first, int64_t second,
                                          int64_t third, int64_t fourth,
                                          int64_t fifth, IntPair pair,
                                          int64_t last) {
    return first == 1 && second == 2 && third == 3 && fourth == 4 &&
           fifth == 5 && pair.first == 71 && pair.second == 72 && last == 6;
}

_Bool casa_extern_sse_aggregate_spill(double first, double second,
                                      double third, double fourth,
                                      double fifth, double sixth, double seventh,
                                      Rectangle rectangle, double last) {
    return first == 1.0 && second == 2.0 && third == 3.0 && fourth == 4.0 &&
           fifth == 5.0 && sixth == 6.0 && seventh == 7.0 &&
           rectangle.x == 1.0f && rectangle.y == 2.0f &&
           rectangle.width == 3.0f && rectangle.height == 4.0f && last == 8.0;
}

_Bool casa_extern_mixed_aggregate_spill(
    int64_t first, int64_t second, int64_t third, int64_t fourth, int64_t fifth,
    double f1, double f2, double f3, double f4, double f5, double f6, double f7,
    double f8, Mixed mixed, int64_t last) {
    return first == 1 && second == 2 && third == 3 && fourth == 4 &&
           fifth == 5 && f1 == 1.0 && f2 == 2.0 && f3 == 3.0 && f4 == 4.0 &&
           f5 == 5.0 && f6 == 6.0 && f7 == 7.0 && f8 == 8.0 &&
           mixed.code == 10 && mixed.weight == 1.5 && last == 6;
}

_Bool casa_extern_memory_parameters(Image image, Texture2D texture,
                                    Camera2D camera, PaddedLarge padded,
                                    int64_t tail) {
    return image.data == NULL && image.width == 640 && image.height == 480 &&
           image.mipmaps == 3 && image.format == 7 && texture.id == 42 &&
           texture.width == 320 && texture.height == 240 &&
           texture.mipmaps == 2 && texture.format == 9 &&
           camera.offset.x == 1.0f && camera.offset.y == 2.0f &&
           camera.target.x == 3.0f && camera.target.y == 4.0f &&
           camera.rotation == 0.5f && camera.zoom == 2.0f && padded.tag == 7 &&
           padded.value == 9001 && padded.code == 513 && tail == 99;
}

PaddedLarge casa_extern_padded(PaddedLarge value) {
    PaddedLarge result = {(uint8_t)(value.tag + 1), value.value + 2,
                          (uint16_t)(value.code + 3)};
    return result;
}

Image casa_extern_memory_mixed(int64_t first, Color color, double scale,
                               Image image, int64_t second, Rectangle rectangle,
                               int64_t third, int64_t fourth, int64_t fifth,
                               Texture2D texture, double last) {
    if (first != 1 || color.r != 1 || color.g != 2 || color.b != 3 ||
        color.a != 4 || scale != 6.5 || image.data != NULL ||
        image.width != 640 || image.height != 480 || image.mipmaps != 3 ||
        image.format != 7 || second != 2 || rectangle.x != 1.0f ||
        rectangle.y != 2.0f || rectangle.width != 3.0f ||
        rectangle.height != 4.0f || third != 3 || fourth != 4 || fifth != 5 ||
        texture.id != 42 || texture.width != 320 || texture.height != 240 ||
        texture.mipmaps != 2 || texture.format != 9 || last != 9.5) {
        Image failure = {(void *)(uintptr_t)1, -1, -1, -1, -1};
        return failure;
    }
    Image result = {image.data, image.width + 1, image.height + 2,
                    image.mipmaps + 3, image.format + 4};
    return result;
}
