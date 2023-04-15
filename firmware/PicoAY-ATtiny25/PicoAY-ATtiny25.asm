;                                                                              ;
;                                   ATtiny25                                   ;
;                                  +--------+                                  ;
;                   (/RESET) PB5 --|*1    8 |-- VCC                            ;
;                            PB3 --| 2    7 |-- PB2 (UART_RX)                  ;
;                (PWM_OUT_R) PB4 --| 3    6 |-- PB1 (PWM_OUT_L)                ;
;                            GND --| 4    5 |-- PB0                            ;
;                                  +--------+                                  ;
;                                                                              ;

; ==============================================================================
; DEFINES
; ==============================================================================

    .equ    F_CPU    = 16000000
    .equ    F_PSG    = 1750000
    .equ    S_CYCLES = 432
    .equ    MAX_AMP  = 170

    .include "../PicoAY.asm"

; ==============================================================================
; FLASH
; ==============================================================================

.cseg
    .org    0x0000
    rjmp    main
    .org    INT0addr
    rjmp    sw_uart_rx_isr

; ==============================================================================
; DATA
; ==============================================================================

    data_reg_mask()
    data_amp_4bit(MAX_AMP)
    data_amp_5bit(MAX_AMP)
    data_envelopes(32)

; ==============================================================================
; CODE
; ==============================================================================

main:
    code_setup_sram()
    code_setup_data_access()

    ; Setup external interrupt INT0 on PB2 for UART RX
    cbi     DDRB,  PORTB2           ; Set PORTB2 as input
    sbi     PORTB, PORTB2           ; Enable pull-up resistor on PORTB2
    ldi     AL, M(ISC01)            ; Falling edge of INT0 generates an
    stio    MCUCR, AL               ; interrupt request
    ldi     AL, M(INT0)             ; Allow INT0 ISR execution
    stio    GIMSK, AL               ;

    ; Setup Timer0 for CTC with OCRA top
    ldi     AL, M(WGM01)            ; CTC mode
    stio    TCCR0A, AL              ;
    ldi     AL, M(CS01)             ; F/8 prescaler
    stio    TCCR0B, AL              ;
    ldi     AL, ((S_CYCLES/8)-1)    ;
    stio    OCR0A, AL               ;

    ; Setup Time1 for High Speed PWM 8-bit with 0xFF top
    sbi     DDRB, PORTB1            ; Set PORTB1 and PORTB4 as output
    sbi     DDRB, PORTB4            ; for PWM (OC1A and OC1B)
;   ldi     AL, M(PLLE) | M(PCKE)
;   stio    PLLCSR, AL
    ldi     AL, M(PWM1A) | M(COM1A1) | M(CS10)
    stio    TCCR1, AL
    ldi     AL, M(PWM1B) | M(COM1B1)
    stio    GTCCR, AL
    ldi     AL, 0xFF
    stio    OCR1C, AL
    ldi     AL, 0x7F
    stio    OCR1A, AL

    ; Setup everything else and start emulation
    code_setup_and_start_emulator()

    ; Software UART implementation
    code_sw_uart_rx_isr(PINB, PORTB2)

loop:
    ; Waiting for timer overflow and samples output
    code_sync_and_out(TIFR, TOV0, OCR1A, OCR1B) ; 6

    ; Update tone, noise and envelope generators
    ldi     AL, U_STEP                          ; 1
    code_update_tone(a_period, a_counter, chA)  ; 30-18
    code_update_tone(b_period, b_counter, chB)  ; 30-18
    code_update_tone(c_period, c_counter, chC)  ; 30-18
    code_reset_envelope()                       ; 16-3
    code_update_noise_envelope(32)              ; 324-116
    code_apply_mixer()                          ; 7

    ; Compute channels samples and stereo/mono output
    ldi     BL, low(P(amp_4bit))                ; 1
    code_compute_envelope_amp(32)               ; 5
    code_compute_channel_amp(a_volume, chA, XL) ; 11-8
    code_compute_channel_amp(b_volume, chB, BH) ; 11-8
    code_compute_channel_amp(c_volume, chC, XH) ; 11-8
    code_compute_output_sample(STEREO_ABC)      ; 3
    rjmp    loop                                ; 2

    ; max cycles: 6+1+3*30+16+324+7+5+1+3*11+3+2=488 (+13%)
    ; min cycles: 6+1+3*18+3+116+7+5+1+3*8+3+2=222   (-48%)
    ; avg cycles: (488+222)/2=355                    (-18%)
    ; ovf period: 355*1.3=462, chosen 432 (8x54)

; ==============================================================================
; SRAM
; ==============================================================================

.dseg
    .org    SRAM_START
    sram_psg_regs_and_state()
