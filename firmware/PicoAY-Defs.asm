; ------------------------------------------------------------------------------
; DEFINES
; ------------------------------------------------------------------------------

    .equ    bit0    = 0
    .equ    bit1    = 1
    .equ    bit2    = 2
    .equ    bit3    = 3
    .equ    bit4    = 4
    .equ    bit5    = 5
    .equ    bit6    = 6
    .equ    bit7    = 7

    #define M(bit)  (1 << bit)
    #define L(addr) (addr + 0)
    #define H(addr) (addr + 1)
    #define P(addr) (2 * addr)
