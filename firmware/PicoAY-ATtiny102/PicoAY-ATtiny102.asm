;                                                                              ;
;                                  ATtiny102                                   ;
;                                  +--------+                                  ;
;                            VCC --|*1    8 |-- GND                            ;
;              (STEREO_MODE) PA0 --| 2    7 |-- PB3 (UART_RX)                  ;
;                (PWM_OUT_R) PA1 --| 3    6 |-- PB2 (CHIP_SELECT)              ;
;                   (/RESET) PA2 --| 4    5 |-- PB1 (PWM_OUT_L)                ;
;                                  +--------+                                  ;
;                                                                              ;

; ==============================================================================
; DEFINES
; ==============================================================================

    .equ    F_CPU    = 8000000
    .equ    F_PSG    = 1750000
    .equ    S_CYCLES = 292;328 for 12MHz
    .equ    MAX_AMP  = ((S_CYCLES/2)*2/3)

    .include "../PicoAY.asm"

    ; Workaround for stupid USART definitions in tn102def.inc
    .equ    UBRR9L = UBRRL
    .equ    UBRR9H = UBRRH
    .equ    UCSR9A = UCSRA
    .equ    UCSR9B = UCSRB
    .equ    UCSR9C = UCSRC
    .equ    U2X9   = U2X
    .equ    RXCIE9 = RXCIE
    .equ    RXEN9  = RXEN
    .equ    UCSZ90 = UCSZ0
    .equ    UCSZ91 = UCSZ1
    .equ    UDR9   = UDR

; ==============================================================================
; FLASH
; ==============================================================================

.cseg
    .org    0x0000
    rjmp    main
    .org    PCINT1addr
    rjmp    osccal_t16_pcint_isr
    .org    USART_RXCaddr
    rjmp    hw_uart_data_isr

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

    ; Setup CPU clock calibration and setup hardware UART RX
    code_setup_input_pullup(B, 3)
    code_setup_osccal(PCMSK1, 11, PCICR, PCIE1)
    code_setup_hw_uart(9)

    ; Setup Timer0 for Phase Correct PWM with ICR0 as TOP
    sbi     DDRB, PORTB1            ; Set PORTB1 and PORTA1 as output
    sbi     DDRA, PORTA1            ; for PWM (OC0A and OC0B)
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
    proc_osccal_t16_pcint_isr(B, 3, 0)
    proc_osccal_uart_bit_duration(B, 3)

    ; Hardware UART implementation
    proc_hw_uart_data_isr(9)

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
    code_compute_output(A, 0)                       ; 8-7

    ; max cycles: 6+73+12+167+6+4+28+8=304 (+03%)
    ; min cycles: 6+37+3+63+6+4+22+7=148   (-50%)
    ; avg cycles: (304+148)/2=226          (-23%)
    ; ovf period: 226*1.3=294, chosen 292

; ==============================================================================
; SRAM
; ==============================================================================

.dseg
    .org    SRAM_START
    sram_psg_regs_and_state()
