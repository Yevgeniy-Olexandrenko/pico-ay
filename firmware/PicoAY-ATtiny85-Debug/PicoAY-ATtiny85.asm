;                                                                              ;
;                                   ATtiny85                                   ;
;                                  +--------+                                  ;
;                   (/RESET) PB5 --|*1    8 |-- VCC                            ;
;                            PB3 --| 2    7 |-- PB2 (UART_RX)                  ;
;                            PB4 --| 3    6 |-- PB1 (PWM_OUT_R)                ;
;                            GND --| 4    5 |-- PB0 (PWM_OUT_L)                ;
;                                  +--------+                                  ;
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
    .equ    TIMER_OVF_REG = TIFR
    .equ    TIMER_OVF_FLG = TOV0
    .equ    PWM_CHANNEL_A = OCR0A
    .equ    PWM_CHANNEL_B = OCR0B

.macro setup_cpu_clock
    ; Do nothing
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
    ; Setup Timer0 for Fast PWM 8-bit with 0xFF top ----------------------------
    sbi     DDRB, PORTB0            ; Set PORTB0 and PORTB1 as output
    sbi     DDRB, PORTB1            ; for Fast PWM (OC0A and OC0B)
    ldi     AL, M(WGM00) | M(WGM01) | M(COM0A1) | M(COM0B1)
    out     TCCR0A, AL              ; Clear OC0A/OC0B on compare match
    ldi     AL, M(CS00)        
    out     TCCR0B, AL              ; Fast PWM with no prescaling
.endm

.macro setup_unused_hardware
    ; TODO
.endm

; ------------------------------------------------------------------------------
; IMPLEMENTATION
; ------------------------------------------------------------------------------

    .include "../PicoAY-Impl.asm"
