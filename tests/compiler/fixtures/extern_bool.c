_Bool casa_bool_parameter_is_false(_Bool value) {
    return !value;
}

_Bool casa_bool_parameter_is_true(_Bool value) {
    return value;
}

static unsigned int loop_calls;

_Bool casa_bool_loop_next(void) {
    return loop_calls++ == 0;
}

/* The System V ABI defines a bool return in %al. Upper %rax bits are unspecified. */
__asm__(
    ".text\n"
    ".globl casa_bool_dirty_false\n"
    ".type casa_bool_dirty_false, @function\n"
    "casa_bool_dirty_false:\n"
    "movabs $0x7f00000000000000, %rax\n"
    "ret\n"
    ".size casa_bool_dirty_false, .-casa_bool_dirty_false\n"
);
