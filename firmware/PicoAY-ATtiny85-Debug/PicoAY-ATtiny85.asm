;                                                                              ;
;                                   ATtiny85                                   ;
;                                  +--------+                                  ;
;                   (/RESET) PB5 --|*1    8 |-- VCC                            ;
;                            PB3 --| 2    7 |-- PB2 (UART_RX)                  ;
;                            PB4 --| 3    6 |-- PB1 (PWM_OUT_R)                ;
;                            GND --| 4    5 |-- PB0 (PWM_OUT_L)                ;
;                                  +--------+                                  ;
;                                                                              ;

; ==============================================================================
; DEFINES
; ==============================================================================

    .equ    F_CPU       = 8000000
    .equ    F_PSG       = 1750000
    .equ    TIMER_TOP   = 0xFF
    .equ    MAX_AMP     = 170

    .include "../PicoAY-Blocks.asm"

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
    out     MCUCR, AL               ; interrupt request
    ldi     AL, M(INT0)             ; Allow INT0 ISR execution
    out     GIMSK, AL               ;

    ; Setup Timer0 for Fast PWM 8-bit with 0xFF top
    sbi     DDRB, PORTB0            ; Set PORTB0 and PORTB1 as output
    sbi     DDRB, PORTB1            ; for Fast PWM (OC0A and OC0B)
    ldi     AL, M(WGM00) | M(WGM01) | M(COM0A1) | M(COM0B1)
    out     TCCR0A, AL              ; Clear OC0A/OC0B on compare match
    ldi     AL, M(CS00)        
    out     TCCR0B, AL              ; Fast PWM with no prescaling

    ; Setup everything else and start emulation
    code_setup_and_start_emulator()

    ; Software UART implementation
    code_sw_uart_rx_isr(PINB, PORTB2)

loop:
    ; Waiting for timer overflow and samples output
    code_sync_and_out(TIFR, TOV0, OCR0A, OCR0B)

    ; Update tone, noise and envelope generators
    ldi     AL, U_STEP                ; 1
    code_update_tone(a_period, a_counter, chA) ; max:24 min:12
    code_update_tone(b_period, b_counter, chB) ; max:24 min:12
    code_update_tone(c_period, c_counter, chC) ; max:24 min:12
    code_reset_envelope()           ; max:12  min:3
    code_update_noise_envelope(32)  ; max:324 min:100
    code_apply_mixer()              ; max:6   min:6

    ; Compute channels samples and stereo/mono output
    ldi     ZL, low(P(amp_5bit))    ; 1   Get envelope amplitude from table
    ldp     AL, e_stp               ; 3   using envelope step as index
    ldi     BL, low(P(amp_4bit))    ; 1
    code_compute_sample(a_volume, chA, XL) ; max:9 min:7
    code_compute_sample(b_volume, chB, BH) ; max:9 min:7
    code_compute_sample(c_volume, chC, XH) ; max:9 min:7
    code_compute_output(STEREO_ABC) ; max:3 min:3
    rjmp    loop                    ; 2   Go to main loop next iteration

; ==============================================================================
; SRAM
; ==============================================================================

.dseg
    .org    SRAM_START
    sram_psg_regs_and_state()
