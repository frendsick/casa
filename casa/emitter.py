from typing import assert_never

from casa.common import Bytecode, Inst, InstKind, Program

INT32_MIN = -(1 << 31)
INT32_MAX = (1 << 31) - 1
RETURN_STACK_SIZE = 65536
HEAP_SIZE = 1048576


class Emitter:
    def __init__(self, program: Program):
        self.program = program
        self._label_counter = 0
        self.asm: list[str] = []

    def _uid(self) -> int:
        self._label_counter += 1
        return self._label_counter

    def _line(self, text: str) -> None:
        self.asm.append(text)

    def _indent(self, text: str) -> None:
        self.asm.append(f"    {text}")

    @staticmethod
    def _sanitize_name(name: str) -> str:
        return name.replace("::", "__")

    @staticmethod
    def _escape_string(s: str) -> str:
        result = []
        for c in s:
            if c == "\\":
                result.append("\\\\")
            elif c == '"':
                result.append('\\"')
            elif c == "\n":
                result.append("\\n")
            elif c == "\t":
                result.append("\\t")
            elif c == "\0":
                result.append("\\0")
            elif c == "\r":
                result.append("\\r")
            else:
                result.append(c)
        return "".join(result)

    def emit(self) -> str:
        self._emit_bss()
        self._emit_data()
        self._emit_text()
        return "\n".join(self.asm) + "\n"

    def _emit_bss(self) -> None:
        self._line(".section .bss")
        p = self.program
        self._line(f"globals: .skip {p.globals_count * 8}")
        self._line(f"constants: .skip {p.constants_count * 8}")
        self._line(f"return_stack: .skip {RETURN_STACK_SIZE}")
        self._line(f"heap: .skip {HEAP_SIZE}")
        self._line("heap_ptr: .skip 8")
        self._line("print_buf: .skip 32")
        self._line("str_alloc_ptr: .skip 8")
        self._line("")

    def _emit_data(self) -> None:
        self._line(".section .data")
        for i, s in enumerate(self.program.strings):
            escaped = self._escape_string(s)
            self._line(".align 8")
            self._line(f".quad {len(s)}")
            self._line(f'str_{i}: .ascii "{escaped}"')
        self._line("")

    def _emit_text(self) -> None:
        self._line(".section .text")
        self._emit_helpers()
        self._emit_functions()
        self._emit_start()

    def _emit_helpers(self) -> None:
        # print_int: value in %rdi, returns via r14
        self._line("print_int:")
        self._indent("movq %rdi, %rax")
        self._indent("leaq print_buf+32(%rip), %rsi")
        # Zero check
        self._indent("testq %rax, %rax")
        self._indent("jnz .Lpi_nonzero")
        self._indent("decq %rsi")
        self._indent("movb $48, (%rsi)")
        self._indent("jmp .Lpi_write")
        # Non-zero: check sign (flags still valid from testq above)
        self._line(".Lpi_nonzero:")
        self._indent("movq %rax, %r15")
        self._indent("jns .Lpi_abs")
        self._indent("negq %rax")
        # Convert absolute value to decimal digits
        self._line(".Lpi_abs:")
        self._indent("movq $10, %rcx")
        self._line(".Lpi_digit_loop:")
        self._indent("decq %rsi")
        self._indent("xorq %rdx, %rdx")
        self._indent("divq %rcx")
        self._indent("addb $48, %dl")
        self._indent("movb %dl, (%rsi)")
        self._indent("testq %rax, %rax")
        self._indent("jnz .Lpi_digit_loop")
        # Prepend minus sign if negative
        self._indent("testq %r15, %r15")
        self._indent("jns .Lpi_write")
        self._indent("decq %rsi")
        self._indent("movb $45, (%rsi)")
        # Write to stdout
        self._line(".Lpi_write:")
        self._indent("leaq print_buf+32(%rip), %rdx")
        self._indent("subq %rsi, %rdx")
        self._indent("movq $1, %rax")
        self._indent("movq $1, %rdi")
        self._indent("syscall")
        self._indent("subq $8, %r14")
        self._indent("jmpq *(%r14)")
        self._line("")

        # print_str: string pointer in %rdi, returns via r14
        self._line("print_str:")
        self._indent("movq -8(%rdi), %rdx")
        self._indent("movq %rdi, %rsi")
        self._indent("movq $1, %rdi")
        self._indent("movq $1, %rax")
        self._indent("syscall")
        self._indent("subq $8, %r14")
        self._indent("jmpq *(%r14)")
        self._line("")

        # str_concat: %rdi = part count (N), N string pointers on data stack
        self._line("str_concat:")
        self._indent("movq %rdi, %r8")
        self._indent("movq %rsp, %r9")
        # Calculate total length
        self._indent("xorq %r10, %r10")
        self._indent("xorq %rcx, %rcx")
        self._line(".Lsc_len_loop:")
        self._indent("cmpq %r8, %rcx")
        self._indent("jge .Lsc_len_done")
        self._indent("movq (%r9, %rcx, 8), %rax")
        self._indent("addq -8(%rax), %r10")
        self._indent("incq %rcx")
        self._indent("jmp .Lsc_len_loop")
        self._line(".Lsc_len_done:")
        # Allocate total_length + 8 via brk
        self._indent("movq str_alloc_ptr(%rip), %r11")
        self._indent("leaq 8(%r11, %r10), %rdi")
        self._indent("pushq %r11")
        self._indent("movq $12, %rax")
        self._indent("syscall")
        self._indent("popq %r11")
        self._indent("movq %rax, str_alloc_ptr(%rip)")
        # Write total length at alloc_base
        self._indent("movq %r10, (%r11)")
        # Result string data starts at alloc_base + 8
        self._indent("leaq 8(%r11), %rdi")
        # Copy each part (bottom to top = reverse stack order)
        self._indent("movq %r8, %rcx")
        self._indent("decq %rcx")
        self._line(".Lsc_copy_loop:")
        self._indent("cmpq $0, %rcx")
        self._indent("jl .Lsc_copy_done")
        self._indent("movq (%r9, %rcx, 8), %rsi")
        self._indent("movq -8(%rsi), %rdx")
        self._indent("pushq %rcx")
        self._indent("movq %rdx, %rcx")
        self._indent("rep movsb")
        self._indent("popq %rcx")
        self._indent("decq %rcx")
        self._indent("jmp .Lsc_copy_loop")
        self._line(".Lsc_copy_done:")
        # Remove N pointers from stack, push result
        self._indent("leaq (%r9, %r8, 8), %rsp")
        self._indent("leaq 8(%r11), %rax")
        self._indent("pushq %rax")
        # Return via return stack
        self._indent("subq $8, %r14")
        self._indent("jmpq *(%r14)")
        self._line("")

    def _emit_functions(self) -> None:
        for name, bytecode in self.program.functions.items():
            label = self._sanitize_name(name)
            self._line(f"fn_{label}:")
            self._emit_bytecode(bytecode, is_global=False)
            self._line("")

    def _emit_start(self) -> None:
        self._line(".globl _start")
        self._line("_start:")
        self._indent("leaq return_stack(%rip), %r14")
        # Initialize string allocation pointer via brk(0)
        self._indent("movq $12, %rax")
        self._indent("xorq %rdi, %rdi")
        self._indent("syscall")
        self._indent("movq %rax, str_alloc_ptr(%rip)")
        self._emit_bytecode(self.program.bytecode, is_global=True)
        self._indent("movq $60, %rax")
        self._indent("xorq %rdi, %rdi")
        self._indent("syscall")
        self._line("")

    def _emit_bytecode(self, bytecode: Bytecode, is_global: bool) -> None:
        for inst in bytecode:
            self._emit_inst(inst, is_global)

    def _emit_comparison(self, setcc: str) -> None:
        self._indent("popq %rax")
        self._indent("cmpq (%rsp), %rax")
        self._indent(f"{setcc} %al")
        self._indent("movzbq %al, %rax")
        self._indent("movq %rax, (%rsp)")

    def _emit_inst(self, inst: Inst, is_global: bool) -> None:
        assert len(InstKind) == 45, "Exhaustive handling for `InstKind`"
        kind = inst.kind
        match kind:
            # === Stack ===
            case InstKind.PUSH:
                val = inst.int_arg
                if INT32_MIN <= val <= INT32_MAX:
                    self._indent(f"pushq ${val}")
                else:
                    self._indent(f"movabsq ${val}, %rax")
                    self._indent("pushq %rax")
            case InstKind.PUSH_STR:
                self._indent(f"leaq str_{inst.int_arg}(%rip), %rax")
                self._indent("pushq %rax")
            case InstKind.DROP:
                self._indent("addq $8, %rsp")
            case InstKind.DUP:
                self._indent("pushq (%rsp)")
            case InstKind.SWAP:
                self._indent("popq %rax")
                self._indent("popq %rbx")
                self._indent("pushq %rax")
                self._indent("pushq %rbx")
            case InstKind.OVER:
                self._indent("pushq 8(%rsp)")
            case InstKind.ROT:
                self._indent("popq %rax")
                self._indent("popq %rbx")
                self._indent("popq %rcx")
                self._indent("pushq %rbx")
                self._indent("pushq %rax")
                self._indent("pushq %rcx")

            # === Arithmetic ===
            case InstKind.ADD:
                self._indent("popq %rax")
                self._indent("addq %rax, (%rsp)")
            case InstKind.SUB:
                self._indent("popq %rax")
                self._indent("subq %rax, (%rsp)")
            case InstKind.MUL:
                self._indent("popq %rax")
                self._indent("imulq (%rsp), %rax")
                self._indent("movq %rax, (%rsp)")
            case InstKind.DIV:
                self._indent("popq %rbx")
                self._indent("popq %rax")
                self._indent("cqto")
                self._indent("idivq %rbx")
                self._indent("pushq %rax")
            case InstKind.MOD:
                self._indent("popq %rbx")
                self._indent("popq %rax")
                self._indent("cqto")
                self._indent("idivq %rbx")
                self._indent("pushq %rdx")

            # === Bitshift ===
            case InstKind.SHL:
                self._indent("popq %rcx")
                self._indent("shlq %cl, (%rsp)")
            case InstKind.SHR:
                self._indent("popq %rcx")
                self._indent("sarq %cl, (%rsp)")

            # === Boolean ===
            case InstKind.AND:
                self._indent("popq %rax")
                self._indent("testq %rax, %rax")
                self._indent("setne %al")
                self._indent("cmpq $0, (%rsp)")
                self._indent("setne %cl")
                self._indent("andb %cl, %al")
                self._indent("movzbq %al, %rax")
                self._indent("movq %rax, (%rsp)")
            case InstKind.OR:
                self._indent("popq %rax")
                self._indent("orq %rax, (%rsp)")
                self._indent("setne %al")
                self._indent("movzbq %al, %rax")
                self._indent("movq %rax, (%rsp)")
            case InstKind.NOT:
                self._indent("cmpq $0, (%rsp)")
                self._indent("sete %al")
                self._indent("movzbq %al, %rax")
                self._indent("movq %rax, (%rsp)")

            # === Comparison ===
            case InstKind.EQ:
                self._emit_comparison("sete")
            case InstKind.NE:
                self._emit_comparison("setne")
            case InstKind.LT:
                self._emit_comparison("setl")
            case InstKind.LE:
                self._emit_comparison("setle")
            case InstKind.GT:
                self._emit_comparison("setg")
            case InstKind.GE:
                self._emit_comparison("setge")

            # === Control flow ===
            case InstKind.LABEL:
                self._line(f".L{inst.int_arg}:")
            case InstKind.JUMP:
                self._indent(f"jmp .L{inst.int_arg}")
            case InstKind.JUMP_NE:
                self._indent("popq %rax")
                self._indent("testq %rax, %rax")
                self._indent(f"jz .L{inst.int_arg}")

            # === Globals ===
            case InstKind.GLOBALS_INIT:
                pass
            case InstKind.GLOBAL_GET:
                idx = inst.int_arg
                self._indent(f"movq globals+{idx * 8}(%rip), %rax")
                self._indent("pushq %rax")
            case InstKind.GLOBAL_SET:
                idx = inst.int_arg
                self._indent("popq %rax")
                self._indent(f"movq %rax, globals+{idx * 8}(%rip)")

            # === Locals (on return stack via %r14) ===
            case InstKind.LOCALS_INIT:
                n = inst.int_arg
                if n > 0:
                    self._indent("movq %r14, %rdi")
                    self._indent(f"movq ${n}, %rcx")
                    self._indent("xorq %rax, %rax")
                    self._indent("rep stosq")
                    self._indent(f"addq ${n * 8}, %r14")
            case InstKind.LOCALS_UNINIT:
                n = inst.int_arg
                if n > 0:
                    self._indent(f"subq ${n * 8}, %r14")
            case InstKind.LOCAL_GET:
                idx = inst.int_arg
                offset = (idx + 1) * 8
                self._indent(f"movq -{offset}(%r14), %rax")
                self._indent("pushq %rax")
            case InstKind.LOCAL_SET:
                idx = inst.int_arg
                offset = (idx + 1) * 8
                self._indent("popq %rax")
                self._indent(f"movq %rax, -{offset}(%r14)")

            # === Constants (closure captures) ===
            case InstKind.CONSTANT_LOAD:
                idx = inst.int_arg
                self._indent(f"movq constants+{idx * 8}(%rip), %rax")
                self._indent("pushq %rax")
            case InstKind.CONSTANT_STORE:
                idx = inst.int_arg
                self._indent("popq %rax")
                self._indent(f"movq %rax, constants+{idx * 8}(%rip)")

            # === Memory (heap) ===
            case InstKind.HEAP_ALLOC:
                self._indent("popq %rax")
                self._indent("movq heap_ptr(%rip), %rbx")
                self._indent("pushq %rbx")
                self._indent("addq %rax, %rbx")
                self._indent("movq %rbx, heap_ptr(%rip)")
            case InstKind.LOAD:
                self._indent("popq %rax")
                self._indent("leaq heap(%rip), %rbx")
                self._indent("movq (%rbx, %rax, 8), %rax")
                self._indent("pushq %rax")
            case InstKind.STORE:
                self._indent("popq %rax")
                self._indent("popq %rbx")
                self._indent("leaq heap(%rip), %rcx")
                self._indent("movq %rbx, (%rcx, %rax, 8)")

            # === Functions ===
            case InstKind.FN_CALL:
                name = inst.str_arg
                label = self._sanitize_name(name)
                uid = self._uid()
                self._indent(f"leaq .Lret_{uid}(%rip), %rax")
                self._indent("movq %rax, (%r14)")
                self._indent("addq $8, %r14")
                self._indent(f"jmp fn_{label}")
                self._line(f".Lret_{uid}:")
            case InstKind.FN_EXEC:
                uid = self._uid()
                self._indent(f"leaq .Lret_{uid}(%rip), %rax")
                self._indent("movq %rax, (%r14)")
                self._indent("addq $8, %r14")
                self._indent("popq %rax")
                self._indent("jmpq *%rax")
                self._line(f".Lret_{uid}:")
            case InstKind.FN_PUSH:
                name = inst.str_arg
                label = self._sanitize_name(name)
                self._indent(f"leaq fn_{label}(%rip), %rax")
                self._indent("pushq %rax")
            case InstKind.FN_RETURN:
                if is_global:
                    self._indent("movq $60, %rax")
                    self._indent("xorq %rdi, %rdi")
                    self._indent("syscall")
                else:
                    self._indent("subq $8, %r14")
                    self._indent("jmpq *(%r14)")

            # === IO ===
            case InstKind.PRINT_INT:
                uid = self._uid()
                self._indent("popq %rdi")
                self._indent(f"leaq .Lprint_done_{uid}(%rip), %rax")
                self._indent("movq %rax, (%r14)")
                self._indent("addq $8, %r14")
                self._indent("jmp print_int")
                self._line(f".Lprint_done_{uid}:")
            case InstKind.PRINT_STR:
                uid = self._uid()
                self._indent("popq %rdi")
                self._indent(f"leaq .Lprint_done_{uid}(%rip), %rax")
                self._indent("movq %rax, (%r14)")
                self._indent("addq $8, %r14")
                self._indent("jmp print_str")
                self._line(f".Lprint_done_{uid}:")

            # === F-strings ===
            case InstKind.FSTRING_CONCAT:
                uid = self._uid()
                count = inst.int_arg
                self._indent(f"movq ${count}, %rdi")
                self._indent(f"leaq .Lconcat_done_{uid}(%rip), %rax")
                self._indent("movq %rax, (%r14)")
                self._indent("addq $8, %r14")
                self._indent("jmp str_concat")
                self._line(f".Lconcat_done_{uid}:")

            case _:
                assert_never(kind)


def emit_program(program: Program) -> str:
    emitter = Emitter(program)
    return emitter.emit()
