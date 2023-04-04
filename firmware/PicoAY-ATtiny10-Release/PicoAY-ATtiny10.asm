;                                                                              ;
;                                   ATtiny10                                   ;
;                                   +------+                                   ;
;                 (PWM_OUT_L) PB0 --|*1  6 |-- PB3 (/RESET)                    ;
;                             GND --| 2  5 |-- VCC                             ;
;                 (PWM_OUT_R) PB1 --| 3  4 |-- PB2 (UART_RX)                   ;
;                                   +------+                                   ;
;                                                                              ;

; ------------------------------------------------------------------------------
; DEFINES
; ------------------------------------------------------------------------------

    .include "../PicoAY-Defs.asm"

    .equ    F_CPU       = 8000000
    .equ    F_PSG       = 1750000

; ------------------------------------------------------------------------------
; MCU SPECIFIC MACRO
; ------------------------------------------------------------------------------

    .equ    UART_INT_ADDR = INT0addr
    .equ    TIMER_OVF_REG = TIFR0
    .equ    TIMER_OVF_FLG = TOV0
    .equ    PWM_CHANNEL_A = OCR0AL
    .equ    PWM_CHANNEL_B = OCR0BL

.macro setup_cpu_clock
    ; Setup main CPU clock to 8 Mhz without prescaler --------------------------
    ldi     AL, 0xD8                ; Write correct signature to Configuration
    out     CCP, AL                 ; Change Protection register and set clock
    out     CLKPSR, ZERO            ; division factor to 1 for 8 MHz
.endm

.macro setup_pgm_access
    ; Setup access to data in FLASH using mapping into SRAM --------------------
    ldi     ZH, high(MAPPED_FLASH_START)
.endm

.macro setup_uart_rx_interrupt
    ; Setup external interrupt INT0 on PB2 for UART RX -------------------------
    cbi     DDRB,  PORTB2           ; Set PORTB2 as input
    sbi     PUEB,  PUEB2            ; Enable pull-up resistor on PORTB2
    cbi     EICRA, ISC00            ; Falling edge of INT0 generates an
    sbi     EICRA, ISC01            ; interrupt request
    sbi     EIMSK, INT0             ; Allow INT0 ISR execution
.endm

.macro setup_8bit_pwm_timer
    ; Setup Timer0 for Fast PWM 8-bit with 0xFF top ----------------------------
    sbi     DDRB, PORTB0            ; Set PORTB0 and PORTB1 as output
    sbi     DDRB, PORTB1            ; for Fast PWM (OC0A and OC0B)
    ldi     AL, M(WGM00) | M(COM0A1) | M(COM0B1)
    out     TCCR0A, AL              ; Clear OC0A/OC0B on compare match
    ldi     AL, M(WGM02) | M(CS00)        
    out     TCCR0B, AL              ; Fast PWM with no prescaling
.endm

.macro setup_unused_hardware
    ; TODO
.endm

.macro ldp
    ; ZL table base or displasement
    ; @0 destination register
    ; @1 displacement or table base
    add     ZL, @1                  ; Add table base with displacement
    ld      @0, Z                   ; Load indirrect from Z
.endm

; ------------------------------------------------------------------------------
; IMPLEMENTATION
; ------------------------------------------------------------------------------

    .include "../PicoAY-Impl.asm"
