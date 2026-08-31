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
