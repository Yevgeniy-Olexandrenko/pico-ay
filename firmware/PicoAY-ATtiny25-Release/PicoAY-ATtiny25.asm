;                                                                              ;
;                                   ATtiny25                                   ;
;                                  +--------+                                  ;
;                   (/RESET) PB5 --|*1    8 |-- VCC                            ;
;                            PB3 --| 2    7 |-- PB2 (UART_RX)                  ;
;                (PWM_OUT_L) PB4 --| 3    6 |-- PB1 (PWM_OUT_L)                ;
;                            GND --| 4    5 |-- PB0                            ;
;                                  +--------+                                  ;
;                                                                              ;

; ------------------------------------------------------------------------------
; DEFINES
; ------------------------------------------------------------------------------

    .include "../PicoAY-Defs.asm"

    .equ    F_CPU       = 16000000
    .equ    F_PSG       = 1750000

; ------------------------------------------------------------------------------
; MCU SPECIFIC MACRO
; ------------------------------------------------------------------------------

    .equ    UART_INT_ADDR = INT0addr
    .equ    TIMER_OVF_REG = TIFR
    .equ    TIMER_OVF_FLG = TOV0
    .equ    PWM_CHANNEL_A = OCR1A
    .equ    PWM_CHANNEL_B = OCR1B

.macro setup_cpu_clock
    ; Do nothing
.endm

.macro setup_pgm_access
    ; Setup access to data in FLASH --------------------------------------------
    ldi     ZH, 0x00
.endm

.macro setup_uart_rx_interrupt
    ; Setup external interrupt INT0 on PB2 for UART RX -------------------------
    cbi     DDRB,  PORTB2           ; Set PORTB2 as input
    sbi     PORTB, PORTB2           ; Enable pull-up resistor on PORTB2
    ldi     AL, M(ISC01)            ; Falling edge of INT0 generates an
    out     MCUCR, AL               ; interrupt request
    ldi     AL, M(INT0)             ; Allow INT0 ISR execution
    out     GIMSK, AL               ;
.endm

.macro setup_8bit_pwm_timer
    ; Setup Timer0 for Phase Correct PWM 8-bit with 0xFF top -------------------
    ldi     AL, M(WGM00)            ; This setup generates timer overflow event
    out     TCCR0A, AL              ; at frequency of 31250 Hz, that is used for 
    ldi     AL, M(CS00)             ; sample generation
    out     TCCR0B, AL              ;

    ; Setup Time1 for High Speed PWM 8-bit with 0xFF top -----------------------
    sbi     DDRB, PORTB1            ; Set PORTB1 and PORTB4 as output
    sbi     DDRB, PORTB4            ; for PWM (OC1A and OC1B)
;   ldi     AL, M(PLLE) | M(PCKE)
;   out     PLLCSR, AL
    ldi     AL, M(PWM1A) | M(COM1A1) | M(CS10)
    out     TCCR1, AL
    ldi     AL, M(PWM1B) | M(COM1B1)
    out     GTCCR, AL
    ldi     AL, 0xFF
    out     OCR1C, AL
    ldi     AL, 0x7F
    out     OCR1A, AL
.endm

.macro setup_unused_hardware
    ; TODO
.endm

.macro ldp
    ; ZL table base or displasement
    ; @0 destination register
    ; @1 displacement or table base
    add     ZL, @1                  ; Add table base with displacement
    lpm     @0, Z                   ; Load indirrect from Z
.endm

; ------------------------------------------------------------------------------
; IMPLEMENTATION
; ------------------------------------------------------------------------------

    .include "../PicoAY-Impl.asm"
