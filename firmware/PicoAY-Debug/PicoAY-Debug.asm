;                                                                              ;
; ATtiny10 simulation:                     Arduino Nano                        ;
; --------------------                  +---+--------+---+                     ;
; UART_RX   -> SW_RX / HW_RX            |   |Mini USB|   |                     ;
; PWM_OUT_L -> T1_PWM_A                 |   +--------+   |                     ;
; PWM_OUT_R -> T1_PWM_B     (LED) PB5 --| D13        D12 |-- PB4 (STEREO_MODE) ;
; MCU clock -> Int 8 Mhz              --| 3V3       ~D11 |-- PB3 (T2_PWM_A)    ;
;                                     --| REF       ~D10 |-- PB2 (T1_PWM_B)    ;
; ATtiny25 simulation:            PC0 --| A0         ~D9 |-- PB1 (T1_PWM_A)    ;
; --------------------            PC1 --| A1          D8 |-- PB0 (CHIP_SELECT) ;
; UART_RX   -> SW_RX / HW_RX      PC2 --| A2          D7 |-- PD7               ;
; PWM_OUT_L -> T2_PWM_A           PC3 --| A3         ~D6 |-- PD6 (T0_PWM_A)    ;
; PWM_OUT_R -> T2_PWM_B           PC4 --| A4         ~D5 |-- PD5 (T0_PWM_B)    ;
; MCU clock -> Ext 16 Mhz         PC5 --| A5          D4 |-- PD4               ;
;                                     --| A6         ~D3 |-- PD3 (T2_PWM_B)    ;
;                                     --| A7          D2 |-- PD2 (SW_RX)       ;
;                                 VCC --| 5V         GND |-- GND               ;
;                        (/RESET) PC6 --| RST        RST |-- PC6 (/RESET)      ;
;                                 GND --| GND         D0 |-- PD0 (HW_RX)       ;
;                                     --| VIN         D1 |-- PD1 (HW_TX)       ;
;                                       |   +--------+   |                     ;
;                                       |   |  ICSP  |   |                     ;
;                                       +---+--------+---+                     ;
;                                                                              ;

; ==============================================================================
; DEFINES
; ==============================================================================

#define INTERNAL_OSCILLATOR     1
#define SOFTWARE_UART_PROBES    0
#define INTOSC_CALIBR_PROBES    1

#if defined(SIM_T10)
    .equ    F_CPU     = 8000000
    .equ    F_PSG     = 1750000
    .equ    S_CYCLES  = 362;292
    .equ    MAX_AMP   = ((S_CYCLES/2)*2/3)
    .equ    ENV_STEPS = 16
#elif defined(SIM_T25)
    .equ    F_CPU     = 16000000
    .equ    F_PSG     = 1750000
    .equ    S_CYCLES  = 432
    .equ    MAX_AMP   = (256*2/3)
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
    rjmp    sw_uart_sbit_isr
#if INTERNAL_OSCILLATOR
    .org    PCI2addr
    rjmp    osccal_t16_pcint_isr
#endif
    .org    URXCaddr
    rjmp    hw_uart_data_isr
    .org    ADCCaddr
    rjmp    sw_uart_dbit_isr

; ==============================================================================
; DATA
; ==============================================================================

    data_reg_mask()
    data_amp_4bit(MAX_AMP)
   .if      (ENV_STEPS == 32)
    data_amp_5bit(MAX_AMP)
   .endif
    data_envelopes(ENV_STEPS)

; ==============================================================================
; CODE
; ==============================================================================

main:
    code_setup_sram()
    code_setup_data_access()

#if !INTERNAL_OSCILLATOR
    ; Setup system clock devider 2 to make 8 MHz from 16 MHz
   .if      (F_CPU == 8000000)
    ldi     AL, M(CLKPCE)           ;
    stio    CLKPR, AL               ;
    ldi     AL, M(CLKPS0)           ;
    stio    CLKPR, AL               ;
   .endif
#endif

#if INTERNAL_OSCILLATOR
#if INTOSC_CALIBR_PROBES
    ; Setup internal oscillator calibration debug probes
    DEF_PROBE(CAL_START_STOP,   C, 0)
    DEF_PROBE(CAL_MEASUREMENT,  C, 1)
    DEF_PROBE(CAL_WRITE_OSCCAL, C, 2)
#endif
    ; Calibrate system clock using UART signal
    code_setup_input_pullup(D, 2)
    code_setup_osccal(PCMSK2, 18, PCICR, PCIE2)
#endif

#if SOFTWARE_UART_PROBES
    ; Setup software UART debug probes
    DEF_PROBE(UART_START, C, 0)
    DEF_PROBE(UART_DELAY, C, 1)
    DEF_PROBE(UART_STORE, C, 2)
#endif

    ; Setup software UART RX
    code_setup_input_pullup(D, 2)
    code_setup_sw_uart(EICRA, EIMSK)

    ; Setup hardware UART RX
    code_setup_hw_uart_u(0)

#if defined(SIM_T10)
    ; Setup Timer1 for Phase Correct PWM with ICR1 as TOP
    sbi     DDRB, PORTB1            ; Set PORTB1 and PORTB2 as output
    sbi     DDRB, PORTB2            ; for PWM (OC1A and OC1B)
    ldi     AL, M(WGM11) | M(COM1A1) | M(COM1B1)
    stio    TCCR1A, AL              ; Clear OC1A/OC1B on compare match
    ldi     AL, M(WGM13) | M(CS10)
    stio    TCCR1B, AL              ; Mode 10 (Phase Correct PWM, no
    ldi     AL, high(S_CYCLES/2-1)  ; prescaling, TOP defined by ICR1)
    stio    ICR1H, AL               ; Set high byte of 16-bit TOP
    ldi     AL, low(S_CYCLES/2-1)   ; then continue with low byte
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

    ; Setup everything else and start generation
    code_setup_input_pullup(B, 0)   ; Setup chip select pin
    code_setup_input_pullup(B, 4)   ; Setup stereo mode pin
    code_setup_and_start_generation()

#if INTERNAL_OSCILLATOR
    ; System clock calibration implementation
    proc_osccal_t16_pcint_isr(D, 2, 1)
    proc_osccal_uart_bit_duration(D, 2)
#endif

    ; Software UART implementation
    proc_sw_uart_sbit_isr(EIMSK)
    proc_sw_uart_dbit_isr(D, 2, EIFR, EIMSK)

    ; Hardware UART implementation
    proc_hw_uart_data_isr_u(0)

loop:
    ; Waiting for timer overflow and performing output
#if defined(SIM_T10)
    code_sync_and_out(TIFR1, TOV1, OCR1AL, OCR1BL)  ; 6
#elif defined(SIM_T25)
    code_sync_and_out(TIFR0, TOV0, OCR2A, OCR2B)    ; 6
#endif

    ; Update tone, noise and envelope generators
    code_update_tones()                             ; 91-55
    code_reinit_envelope()                          ; 16-3
    code_update_noise_envelope(ENV_STEPS)           ; T10:227-87 T25:269-101
    code_apply_mixer()                              ; 7

    ; Compute amplitudes and stereo output
    code_compute_envelope_amp(ENV_STEPS)            ; 5
    code_compute_channels_amp()                     ; 34-25
    code_compute_output(B, 4)                       ; 8-7

    ; T10
    ; max cycles: 6+91+16+227+7+5+34+8=394 (+09%)
    ; min cycles: 6+55+3+87+7+5+25+7=195   (-46%)
    ; avg cycles: (394+195)/2=295          (-19%)
    ; ovf period: 323*1.3=384, chosen 362

    ; T25
    ; max cycles: 6+91+16+269+7+5+34+8=436 (+01%)
    ; min cycles: 6+55+3+101+7+5+25+7=209  (-52%)
    ; avg cycles: (436+209)/2=323          (-25%)
    ; ovf period: 295*1.3=420, chosen 432

; ==============================================================================
; SRAM
; ==============================================================================

.dseg
    .org    SRAM_START
    sram_psg_regs_and_state()
