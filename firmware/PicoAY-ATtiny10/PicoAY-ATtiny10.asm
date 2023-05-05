;                                                                              ;
;                                   ATtiny10                                   ;
;                                   +------+                                   ;
;                 (PWM_OUT_L) PB0 --|*1  6 |-- PB3 (/RESET)                    ;
;                             GND --| 2  5 |-- VCC                             ;
;                 (PWM_OUT_R) PB1 --| 3  4 |-- PB2 (UART_RX)                   ;
;                                   +------+                                   ;
;                                                                              ;

; ==============================================================================
; DEFINES
; ==============================================================================

    .equ    F_CPU    = 8000000
    .equ    F_PSG    = 1750000
    .equ    S_CYCLES = 292;328 for 12MHz
    .equ    MAX_AMP  = ((S_CYCLES/2)*2/3)

    .include "../PicoAY.asm"

; ==============================================================================
; FLASH
; ==============================================================================

.cseg
    .org    0x0000
    rjmp    main
    .org    INT0addr
    rjmp    sw_uart_int0_sbit_isr
    .org    PCI0addr
    rjmp    osccal_t16_pcint_isr
    .org    ADCCaddr
    rjmp    sw_uart_int0_dbit_isr

; ==============================================================================
; DATA
; ==============================================================================

    data_reg_mask()
    data_amp_4bit(MAX_AMP)
    data_envelopes(16)

; ==============================================================================
; CODE
; ==============================================================================

main:
    code_setup_sram()
    code_setup_data_access()

    ; Setup system clock to 8 Mhz without prescaler
    ldi     AL, 0xD8                ; Write correct signature to Configuration
    stio    CCP, AL                 ; Change Protection register and set clock
    stio    CLKPSR, ZERO            ; division factor to 1 for 8 MHz

    ; Calibrate system clock and setup software UART RX
    code_setup_input_pullup(B, 2)
    code_calibrate_internal_oscillator(PCMSK, 2, PCICR, PCIE0)
    code_setup_sw_uart_int0(EICRA, EIMSK)

    ; Setup Timer0 for Phase Correct PWM with ICR0 as TOP
    sbi     DDRB, PORTB0            ; Set PORTB0 and PORTB1 as output
    sbi     DDRB, PORTB1            ; for PWM (OC0A and OC0B)
    ldi     AL, M(WGM01) | M(COM0A1) | M(COM0B1)
    stio    TCCR0A, AL              ; Clear OC0A/OC0B on compare match
    ldi     AL, M(WGM03) | M(CS00)
    stio    TCCR0B, AL              ; Mode 10 (Phase Correct PWM, no
    ldi     AL, high(S_CYCLES/2-1)  ; prescaling, TOP defined by ICR0)
    stio    ICR0H, AL               ; Set high byte of 16-bit TOP
    ldi     AL, low (S_CYCLES/2-1)  ; then continue with low byte
    stio    ICR0L, AL

    ; Setup everything else and start generation
    code_setup_and_start_generation()

    ; System clock calibration implementation
    proc_osccal_t16_pcint_isr(B, 2, 0)
    proc_osccal_uart_bit_duration(B, 2)

    ; Software UART implementation
    proc_sw_uart_int0_sbit_isr(EIMSK)
    proc_sw_uart_int0_dbit_isr(B, 2, EIFR, EIMSK)

loop:
    ; Waiting for timer overflow and performing output
    code_sync_and_out(TIFR0, TOV0, OCR0AL, OCR0BL)  ; 6

    ; Update tone, noise and envelope generators
    code_update_tones()                             ; 73-37
    code_reinit_envelope()                          ; 12-3
    code_update_noise_envelope(16)                  ; 167-63
    code_apply_mixer()                              ; 6

    ; Compute amplitudes and stereo output
    code_compute_envelope_amp(16)                   ; 4
    code_compute_channels_amp()                     ; 28-22
    code_compute_output_abc()                       ; 5

    ; max cycles: 6+73+12+167+6+4+28+5=301 (+03%)
    ; min cycles: 6+37+3+63+6+4+22+5=146   (-50%)
    ; avg cycles: (301+146)/2=224          (-23%)
    ; ovf period: 224*1.3=291, chosen 292

; ==============================================================================
; SRAM
; ==============================================================================

.dseg
    .org    SRAM_START
    sram_psg_regs_and_state()
