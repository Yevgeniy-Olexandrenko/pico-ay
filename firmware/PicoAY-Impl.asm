; ------------------------------------------------------------------------------
; DEFINES
; ------------------------------------------------------------------------------

    .equ    MAX_AMP     = 170

    ; config 0 bits:
    ; b0 - disable channel A
    ; b1 - disable channel B
    ; b2 - disable channel C
    ; b3 - disable auto mute
    ; b4 - 0: use default 1.75 MHz PSG clock, 1: use custom PSG clock
    ; b5 - 0: custom 1.00 MHz PSG clock, 1: custom 2.00 MHz clock
    ; b6 - envelope resolution: 0: 5-bit, 1: 4-bit
    ; b7 - stereo mode: 0: ABC, 1: ACB

; ------------------------------------------------------------------------------
; FLASH
; ------------------------------------------------------------------------------
    .cseg

    .org    0x0000
    rjmp    main                    ; RESET handler

    .org    UART_INT_ADDR
    rjmp    sw_uart_rx_isr          ; UART handler

; DATA TABLES  -----------------------------------------------------------------
    data_reg_mask()
    data_amp_4bit(MAX_AMP)
    data_amp_5bit(MAX_AMP)
    data_envelopes(32)

; ENTRY POINT ------------------------------------------------------------------
main:
    code_setup_sram()
    code_setup_data_access()

    ; Setup MCU hardware components
    setup_cpu_clock                 ;
    setup_uart_rx_interrupt         ;
    setup_8bit_pwm_timer            ;
    setup_unused_hardware           ;

    ; Setup everything else
    code_setup_and_start_emulator()

; UART PROTOCOL ----------------------------------------------------------------
    code_sw_uart_rx_isr()

; MAIN LOOP --------------------------------------------------------------------
loop:
    ; MAX CYCLES: 6 + 73 + 336 + 6 + 32 + 5 = 458
    ; MIN CYCLES: 6 + 37 + 103 + 6 + 26 + 5 = 183
    ; AVG CYCLES: (457 + 182) / 2 = 320

    ; [..6|..4] Waiting for timer overflow and outputting samples
    code_sync_and_out(TIMER_OVF_REG, TIMER_OVF_FLG, PWM_CHANNEL_A, PWM_CHANNEL_B)

    ; Update generators
    ldi     AL, FDIV                ; 1
    code_update_tone(a_period, a_counter, chA) ; max:24 min:12
    code_update_tone(b_period, b_counter, chB) ; max:24 min:12
    code_update_tone(c_period, c_counter, chC) ; max:24 min:12
    code_reset_envelope()           ; max:12  min:3
    code_update_noise_envelope()    ; max:324 min:100
    code_apply_mixer()              ; max:6   min:6

    ; [.32|.26] Compute samples and output
    ldi     ZL, low(P(amp_5bit))    ; 1   Get envelope amplitude from table
    ldp     AL, e_stp               ; 3   using envelope step as index
    ldi     BL, low(P(amp_4bit))    ; 1
    code_compute_sample(a_volume, chA, XL) ; max:9 min:7
    code_compute_sample(b_volume, chB, BH) ; max:9 min:7
    code_compute_sample(c_volume, chC, XH) ; max:9 min:7
    code_compute_output(STEREO_ABC) ; max:3 min:3
    rjmp    loop                    ; 2   Go to main loop next iteration

; ------------------------------------------------------------------------------
; SRAM
; ------------------------------------------------------------------------------

    .dseg
    .org    SRAM_START

    sram_psg_regs_and_state()
