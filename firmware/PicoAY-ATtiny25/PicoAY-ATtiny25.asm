;                                                                              ;
;                                   ATtiny25                                   ;
;                                  +--------+                                  ;
;                   (/RESET) PB5 --|*1    8 |-- VCC                            ;
;              (STEREO_MODE) PB3 --| 2    7 |-- PB2 (UART_RX)                  ;
;                (PWM_OUT_R) PB4 --| 3    6 |-- PB1 (PWM_OUT_L)                ;
;                            GND --| 4    5 |-- PB0 (CHIP_SELECT)              ;
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
    rjmp    sw_uart_int0_sbit_isr
    .org    ADCCaddr
    rjmp    sw_uart_int0_dbit_isr

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

    ; Setup Timer0 for Phase Correct PWM 8-bit with OCRA top
    ldi     AL, M(WGM00)            ;
    stio    TCCR0A, AL              ;
    ldi     AL, M(WGM02) | M(CS00)  ;
    stio    TCCR0B, AL              ;
    ldi     AL, ((S_CYCLES/2)-1)    ;
    stio    OCR0A, AL               ;

    ; Setup software UART RX
    code_setup_input_pullup(B, 2)
    code_setup_sw_uart_int0(MCUCR, GIMSK)

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

    ; Setup everything else and start generation
    code_setup_input_pullup(B, 0)   ; Setup chip select pin
    code_setup_input_pullup(B, 3)   ; Setup stereo mode pin
    code_setup_and_start_generation()

    ; Software UART implementation
    proc_sw_uart_int0_sbit_isr(GIMSK)
    proc_sw_uart_int0_dbit_isr(B, 2, GIFR, GIMSK)

loop:
    ; Waiting for timer overflow and performing output
    code_sync_and_out(TIFR, TOV0, OCR1A, OCR1B)     ; 6

    ; Update tone, noise and envelope generators
    code_update_tones()                             ; 91-55
    code_reinit_envelope()                          ; 16-3
    code_update_noise_envelope(32)                  ; 269-101
    code_apply_mixer()                              ; 7

    ; Compute amplitudes and stereo output
    code_compute_envelope_amp(32)                   ; 5
    code_compute_channels_amp()                     ; 34-25
    code_compute_output(B, 3)                       ; 8-7

    ; max cycles: 6+91+16+269+7+5+34+8=436 (+01%)
    ; min cycles: 6+55+3+101+7+5+25+7=209  (-52%)
    ; avg cycles: (436+209)/2=323          (-25%)
    ; ovf period: 323*1.3=420, chosen 432

; ==============================================================================
; SRAM
; ==============================================================================

.dseg
    .org    SRAM_START
    sram_psg_regs_and_state()
