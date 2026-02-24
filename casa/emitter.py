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
        self._line("")

    def _emit_data(self) -> None:
        self._line(".section .data")
        for i, s in enumerate(self.program.strings):
            escaped = self._escape_string(s)
            self._line(".align 8")
            self._line(f"str_{i}:")
            self._line(f".quad {len(s)}")
            self._line(f'.asciz "{escaped}"')
        self._line('bool_true: .ascii "true"')
        self._line('bool_false: .ascii "false"')
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

        # print_str: string pointer in %rdi (points to length prefix), returns via r14
        self._line("print_str:")
        self._indent("movq (%rdi), %rdx")
        self._indent("leaq 8(%rdi), %rsi")
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
        self._indent("addq (%rax), %r10")
        self._indent("incq %rcx")
        self._indent("jmp .Lsc_len_loop")
        self._line(".Lsc_len_done:")
        # Allocate total_length + 9 on heap (8 for length, 1 for null)
        self._indent("leaq heap(%rip), %r11")
        self._indent("movq heap_ptr(%rip), %r12")
        self._indent("leaq (%r11, %r12), %r13")
        self._indent("leaq 9(%r10, %r12), %rax")
        self._indent("movq %rax, heap_ptr(%rip)")
        # Write total length at base
        self._indent("movq %r10, (%r13)")
        # Result data starts at base + 8
        self._indent("leaq 8(%r13), %rdi")
        # Copy each part (bottom to top = reverse stack order)
        self._indent("movq %r8, %rcx")
        self._indent("decq %rcx")
        self._line(".Lsc_copy_loop:")
        self._indent("cmpq $0, %rcx")
        self._indent("jl .Lsc_copy_done")
        self._indent("movq (%r9, %rcx, 8), %rsi")
        self._indent("movq (%rsi), %rdx")
        self._indent("addq $8, %rsi")
        self._indent("pushq %rcx")
        self._indent("movq %rdx, %rcx")
        self._indent("rep movsb")
        self._indent("popq %rcx")
        self._indent("decq %rcx")
        self._indent("jmp .Lsc_copy_loop")
        self._line(".Lsc_copy_done:")
        # Write null terminator
        self._indent("movb $0, (%rdi)")
        # Remove N pointers from stack, push result
        self._indent("leaq (%r9, %r8, 8), %rsp")
        self._indent("pushq %r13")
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
        assert len(InstKind) == 66, "Exhaustive handling for `InstKind`"
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

            # === Bitwise ===
            case InstKind.BIT_AND:
                self._indent("popq %rax")
                self._indent("andq %rax, (%rsp)")
            case InstKind.BIT_OR:
                self._indent("popq %rax")
                self._indent("orq %rax, (%rsp)")
            case InstKind.BIT_XOR:
                self._indent("popq %rax")
                self._indent("xorq %rax, (%rsp)")
            case InstKind.BIT_NOT:
                self._indent("notq (%rsp)")

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
                self._indent("leaq heap(%rip), %rbx")
                self._indent("movq heap_ptr(%rip), %rcx")
                self._indent("leaq (%rbx,%rcx), %rdx")
                self._indent("pushq %rdx")
                self._indent("addq %rax, %rcx")
                self._indent("movq %rcx, heap_ptr(%rip)")
            case InstKind.LOAD8:
                self._indent("popq %rax")
                self._indent("movzbl (%rax), %eax")
                self._indent("pushq %rax")
            case InstKind.LOAD16:
                self._indent("popq %rax")
                self._indent("movzwl (%rax), %eax")
                self._indent("pushq %rax")
            case InstKind.LOAD32:
                self._indent("popq %rax")
                self._indent("movl (%rax), %eax")
                self._indent("pushq %rax")
            case InstKind.LOAD64:
                self._indent("popq %rax")
                self._indent("movq (%rax), %rax")
                self._indent("pushq %rax")
            case InstKind.STORE8:
                self._indent("popq %rax")
                self._indent("popq %rbx")
                self._indent("movb %bl, (%rax)")
            case InstKind.STORE16:
                self._indent("popq %rax")
                self._indent("popq %rbx")
                self._indent("movw %bx, (%rax)")
            case InstKind.STORE32:
                self._indent("popq %rax")
                self._indent("popq %rbx")
                self._indent("movl %ebx, (%rax)")
            case InstKind.STORE64:
                self._indent("popq %rax")
                self._indent("popq %rbx")
                self._indent("movq %rbx, (%rax)")

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
            case InstKind.PRINT_BOOL:
                uid = self._uid()
                self._indent("popq %rax")
                self._indent("testq %rax, %rax")
                self._indent(f"jz .Lpb_false_{uid}")
                self._indent("leaq bool_true(%rip), %rsi")
                self._indent("movq $4, %rdx")
                self._indent(f"jmp .Lpb_write_{uid}")
                self._line(f".Lpb_false_{uid}:")
                self._indent("leaq bool_false(%rip), %rsi")
                self._indent("movq $5, %rdx")
                self._line(f".Lpb_write_{uid}:")
                self._indent("movq $1, %rdi")
                self._indent("movq $1, %rax")
                self._indent("syscall")
            case InstKind.PRINT_STR:
                uid = self._uid()
                self._indent("popq %rdi")
                self._indent(f"leaq .Lprint_done_{uid}(%rip), %rax")
                self._indent("movq %rax, (%r14)")
                self._indent("addq $8, %r14")
                self._indent("jmp print_str")
                self._line(f".Lprint_done_{uid}:")
            case InstKind.PUSH_CHAR:
                val = inst.int_arg
                self._indent(f"pushq ${val}")
            case InstKind.PRINT_CHAR:
                self._indent("popq %rax")
                self._indent("movb %al, -1(%rsp)")
                self._indent("leaq -1(%rsp), %rsi")
                self._indent("movq $1, %rdx")
                self._indent("movq $1, %rdi")
                self._indent("movq $1, %rax")
                self._indent("syscall")
            case InstKind.PRINT_CSTR:
                uid = self._uid()
                self._indent("popq %rsi")
                self._indent("movq %rsi, %rdi")
                self._line(f".Lpc_scan_{uid}:")
                self._indent("cmpb $0, (%rsi)")
                self._indent(f"je .Lpc_done_{uid}")
                self._indent("incq %rsi")
                self._indent(f"jmp .Lpc_scan_{uid}")
                self._line(f".Lpc_done_{uid}:")
                self._indent("movq %rsi, %rdx")
                self._indent("subq %rdi, %rdx")
                self._indent("movq %rdi, %rsi")
                self._indent("movq $1, %rdi")
                self._indent("movq $1, %rax")
                self._indent("syscall")

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

            # === Syscalls ===
            case InstKind.SYSCALL0:
                self._emit_syscall(0)
            case InstKind.SYSCALL1:
                self._emit_syscall(1)
            case InstKind.SYSCALL2:
                self._emit_syscall(2)
            case InstKind.SYSCALL3:
                self._emit_syscall(3)
            case InstKind.SYSCALL4:
                self._emit_syscall(4)
            case InstKind.SYSCALL5:
                self._emit_syscall(5)
            case InstKind.SYSCALL6:
                self._emit_syscall(6)

            case _:
                assert_never(kind)

    def _emit_syscall(self, arg_count: int) -> None:
        regs = ["%rdi", "%rsi", "%rdx", "%r10", "%r8", "%r9"]
        self._indent("popq %rax")
        for i in range(arg_count):
            self._indent(f"popq {regs[i]}")
        self._indent("syscall")
        self._indent("pushq %rax")


def emit_program(program: Program) -> str:
    emitter = Emitter(program)
    return emitter.emit()
