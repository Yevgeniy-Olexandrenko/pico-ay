;                                                                              ;
; ATtiny10 simulation:                    Arduino Nano                         ;
; --------------------                 +---+--------+---+                      ;
; UART_RX   -> SW_RX / HW_RX           |   |Mini USB|   |                      ;
; PWM_OUT_L -> T1_PWM_A                |   +--------+   |                      ;
; PWM_OUT_R -> T1_PWM_B          PB5 --| D13        D12 |-- PB4                ;
; MCU clock -> Int 8 Mhz             --| 3V3       ~D11 |-- PB3 (T2_PWM_A)     ;
;                                    --| REF       ~D10 |-- PB2 (T1_PWM_B)     ;
; ATtiny25 simulation:           PC0 --| A0         ~D9 |-- PB1 (T1_PWM_A)     ;
; --------------------           PC1 --| A1          D8 |-- PB0                ;
; UART_RX   -> SW_RX / HW_RX     PC2 --| A2          D7 |-- PD7                ;
; PWM_OUT_L -> T2_PWM_A          PC3 --| A3         ~D6 |-- PD6 (T0_PWM_A)     ;
; PWM_OUT_R -> T2_PWM_B          PC4 --| A4         ~D5 |-- PD5 (T0_PWM_B)     ;
; MCU clock -> Ext 16 Mhz        PC5 --| A5          D4 |-- PD4                ;
;                                    --| A6         ~D3 |-- PD3 (T2_PWM_B)     ;
;                                    --| A7          D2 |-- PD2 (SW_RX)        ;
;                                VCC --| 5V         GND |-- GND                ;
;                       (/RESET) PC6 --| RST        RST |-- PC6 (/RESET)       ;
;                                GND --| GND         D0 |-- PD0 (HW_RX)        ;
;                                    --| VIN         D1 |-- PD1                ;
;                                      |   +--------+   |                      ;
;                                      |   |  ICSP  |   |                      ;
;                                      +---+--------+---+                      ;
;                                                                              ;

; ==============================================================================
; DEFINES
; ==============================================================================

#if defined(SIM_T10)
    .equ    F_CPU     = 8000000
    .equ    F_PSG     = 1750000
    .equ    S_CYCLES  = 292
    .equ    MAX_AMP   = 170
    .equ    ENV_STEPS = 16
#elif defined(SIM_T25)
    .equ    F_CPU     = 16000000
    .equ    F_PSG     = 1750000
    .equ    S_CYCLES  = 432
    .equ    MAX_AMP   = 170
    .equ    ENV_STEPS = 32
#endif

    .include "../PicoAY.asm"

; ==============================================================================
; FLASH
; ==============================================================================

.cseg
    .org    0x0000
    rjmp    main
    .org    INT0addr
    rjmp    sw_uart_rx_sbit_isr
    .org    URXCaddr
    rjmp    hw_uart_rx_isr
    .org    ADCCaddr
    rjmp    sw_uart_rx_dbit_isr

; ==============================================================================
; DATA
; ==============================================================================

    data_reg_mask()
    data_amp_4bit(MAX_AMP)
.if ENV_STEPS == 32
    data_amp_5bit(MAX_AMP)
.endif
    data_envelopes(ENV_STEPS)

; ==============================================================================
; CODE
; ==============================================================================

main:
    code_setup_sram()
    code_setup_data_access()

    ; Setup external interrupt INT0 on PD2 (RX) for software UART
    cbi     DDRD,  PORTD2           ; Set PORTD2 as input
    sbi     PORTD, PORTD2           ; Enable pull-up resistor on PORTD2
    ldi     AL, M(ISC01)            ; Falling edge of INT0 generates an
    stio    EICRA, AL               ; interrupt request
    ldi     AL, M(INT0)             ; Allow INT0 ISR execution
    stio    EIMSK, AL               ;

    ; Setup ADC to use as background delay for software UART
    ldi     AL, M(ADEN) | M(ADSC)   ; Enable ADC, set min prescaler and start
    stio    ADCSRA, AL              ; conversion to achieve stable 13 cycles

    ; Setup hardware UART RX
    .equ    UBRR = (int(0.5 + F_CPU / 8.0 / BAUD_RATE) - 1)
    ldi     AL, high(UBRR)          ;
    stio    UBRR0H, AL              ;
    ldi     AL, low(UBRR)           ;
    stio    UBRR0L, AL              ;
    ldi     AL, M(U2X0)             ;
    stio    UCSR0A, AL              ;
    ldi     AL, M(RXCIE0) | M(RXEN0)
    stio    UCSR0B, AL              ;
    ldi     AL, M(UCSZ01) | M(UCSZ00)
    stio    UCSR0C, AL              ;

#if defined(SIM_T10)
    ; Setup Timer1 for Fast PWM 8-bit with ICR1 top
    sbi     DDRB, PORTB1            ; Set PORTB1 and PORTB2 as output
    sbi     DDRB, PORTB2            ; for Fast PWM (OC1A and OC1B)
    ldi     AL, M(WGM11) | M(COM1A1) | M(COM1B1)
    stio    TCCR1A, AL              ; Clear OC1A/OC1B on compare match
    ldi     AL, M(WGM12) | M(WGM13)  | M(CS10)
    stio    TCCR1B, AL              ; Fast PWM with no prescaling with
    ldi     AL, high(S_CYCLES-1)    ; timer top defined by ICR1
    stio    ICR1H, AL               ; Set high byte of 16-bit value
    ldi     AL, low(S_CYCLES-1)     ; then continue with low byte
    stio    ICR1L, AL
#elif defined(SIM_T25)
    ; Setup Timer0 for Phase Correct PWM 8-bit with OCRA top
    ldi     AL, M(WGM00)            ;
    stio    TCCR0A, AL              ;
    ldi     AL, M(WGM02) | M(CS00)  ;
    stio    TCCR0B, AL              ;
    ldi     AL, ((S_CYCLES/2)-1)    ;
    stio    OCR0A, AL               ;

    ; Setup Timer2 for Fast PWM 8-bit with 0xFF top
    sbi     DDRB, PORTB3            ; Set PORTB3 and PORTD3 as output
    sbi     DDRD, PORTD3            ; for Fast PWM (OC2A and OC2B)
    ldi     AL, M(WGM20) | M(WGM21) | M(COM2A1) | M(COM2B1)
    stio    TCCR2A, AL              ; Clear OC2A/OC2B on compare match
    ldi     AL, M(CS20)             ;
    stio    TCCR2B, AL              ; Fast PWM with no prescaling
#endif

    ; Setup everything else and start emulation
sbi DDRB, PORTB0 ; Set PORTB4 as output
sbi DDRB, PORTB4 ; Set PORTB4 as output
sbi DDRB, PORTB5 ; Set PORTB5 as output
    code_setup_and_start_emulator()

    ; Software UART implementation
    code_sw_uart_rx_sbit_isr()
    code_sw_uart_rx_dbit_isr(PIND, PORTD2)

    ; Hardware UART implementation
    code_hw_uart_rx_isr(UDR0)

loop:
    ; Waiting for timer overflow and samples output
#if defined(SIM_T10)
    code_sync_and_out(TIFR1, TOV1, OCR1AL, OCR1BL)  ; 6
#elif defined(SIM_T25)
    code_sync_and_out(TIFR0, TOV0, OCR2A, OCR2B)    ; 6
#endif

    ; Update tone, noise and envelope generators
    ldi     AL, U_STEP                              ; 1
    code_update_tone(a_period, a_counter, chA)      ; 30-18
    code_update_tone(b_period, b_counter, chB)      ; 30-18
    code_update_tone(c_period, c_counter, chC)      ; 30-18
    code_reset_envelope()                           ; 16-3
    code_update_noise_envelope(ENV_STEPS)           ; TODO: 324-116 / ?-?
    code_apply_mixer()                              ; 7

    ; Compute channels samples and stereo/mono output
    ldi     BL, low(P(amp_4bit))                    ; 1
    code_compute_envelope_amp(ENV_STEPS)            ; 5
    code_compute_channel_amp(a_volume, chA, XL)     ; 11-8
    code_compute_channel_amp(b_volume, chB, BH)     ; 11-8
    code_compute_channel_amp(c_volume, chC, XH)     ; 11-8
    code_compute_output_sample(STEREO_ABC)          ; 3
    rjmp    loop                                    ; 2

; ==============================================================================
; SRAM
; ==============================================================================

.dseg
    .org    SRAM_START
    sram_psg_regs_and_state()
