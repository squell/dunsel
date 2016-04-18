/* GF(2^8) parallel multiplication using bitslicing */

.intel_syntax noprefix
.altmacro

.text

POLY = 0x169

# shift reg; xor poly into dst if carry

.macro gf op reg, dst, poly, tmp=ecx
    op reg, 1
    sbb tmp, tmp
    and tmp, poly
    xor dst, tmp
.endm

# bitsliced-version

# implicit argument: rdi
.macro slice_shl poly
    mov sil, dil
    shr rdi, 8
    mov ch, poly
    .rept 8
    rol rdi, 8
    gf shr ch, dil, sil, dl
    .endr
.endm

# x = rdi
# y = rsi
# z = rax
.macro slice_mul poly
    xor rax, rax
    .rept 8
    .rept 8
    rol rdi, 8
    mov dl, dil
    and dl, sil
    xor al, dl
    ror rax, 8
    .endr
    slice_shl poly
    shr rsi, 8
    .endr
.endm

# C glue

.globl bitslice_mul_s
bitslice_mul_s:
    push rdx
    mov rdi, [rdi]
    mov rsi, [rsi]
    bswap rdi # invert byte order for easy access to MSB of X
    slice_mul POLY
    pop rdx
    mov [rdx], rax
    ret
