/* GF(2^8) parallel multiplication using bitslicing */

.intel_syntax noprefix
.altmacro

.text

POLY = 0x169

.macro maybe op reg, arg
.if arg
op reg, arg
.endif
.endm

# bitsliced-version

# implicit argument: rdi
.macro slice_shl poly
local i pos
i=0
pos=1
    mov sil, dil
    xor dil, dil
    .rept 8
    .if poly and (1 << i)
    maybe rol rdi, (i+1-pos)*8
    xor dil, sil
    pos=i+1
    .endif
    i=i+1
    .endr
    maybe rol rdi, (8-pos)*8
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
