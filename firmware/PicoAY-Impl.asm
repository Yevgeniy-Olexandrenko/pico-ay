; ------------------------------------------------------------------------------
; DEFINES
; ------------------------------------------------------------------------------

    .equ    BAUD_RATE   = 57600
    .equ    SAMPLE_RATE = 31250
    .equ    FDIV        = (F_PSG / 8 / SAMPLE_RATE)
    .equ    MAX_AMP     = 170

    .def    ZERO    = r16           ;
    .def    raddr   = r17           ;
    .def    flags   = r18           ; 
    .def    n_cnt   = r19           ;
    .def    e_stp   = r20           ;
    .def    e_gen   = r21           ;
    .def    AL      = r22           ;
    .def    AH      = r23           ;
    .def    BL      = r24           ;
    .def    BH      = r25           ;
;   .def    XL      = r26           ;
;   .def    XH      = r27           ;
;   .def    YL      = r28           ;
;   .def    YH      = r29           ;
;   .def    ZL      = r30           ;
;   .def    ZH      = r31           ;

    .equ    chA     = bit0
    .equ    chB     = bit1
    .equ    chC     = bit2

    .equ    NS_B16  = bit7          ; Noise shifter bit16
    .equ    EG_RES  = bit6          ; Envelope generator reset
    .equ    WF_REG  = bit4          ; Waiting for register address

    ; config 0 bits:
    ; b0 - disable channel A
    ; b1 - disable channel B
    ; b2 - disable channel C
    ; b3 - disable auto mute
    ; b4 - 0: use default 1.75 MHz PSG clock, 1: use custom PSG clock
    ; b5 - 0: custom 1.00 MHz PSG clock, 1: custom 2.00 MHz clock
    ; b6 - envelope resolution: 0: 5-bit, 1: 4-bit
    ; b7 - stereo mode: 0: ABC, 1: ACB

    .include "../PicoAY-Blocks.asm"

; ------------------------------------------------------------------------------
; FLASH
; ------------------------------------------------------------------------------
    .cseg

    .org    0x0000
    rjmp    main                    ; RESET handler

    .org    UART_INT_ADDR
    rjmp    uart_isr                ; UART handler

; DATA TABLES  -----------------------------------------------------------------
    data_reg_mask()
    data_amp_4bit(MAX_AMP)
    data_amp_5bit(MAX_AMP)
    data_envelopes(32)

; ENTRY POINT ------------------------------------------------------------------
main:
    code_setup_sram()
    code_setup_data_access()

    ; Setup MCU hardware components --------------------------------------------
    setup_cpu_clock                 ;
    setup_uart_rx_interrupt         ;
    setup_8bit_pwm_timer            ;
    setup_unused_hardware           ;

    ; Setup everything else ----------------------------------------------------
    code_setup_and_start_emulator()

; UART PROTOCOL ----------------------------------------------------------------
    .equ    BIT_DURATION = (F_CPU / BAUD_RATE)
    .equ    BIT_DELAY_10 = int((1.0 * BIT_DURATION - 5 + 1.5) / 3)
    .equ    BIT_DELAY_15 = int((1.5 * BIT_DURATION - 9 + 1.5) / 3)

    ; It's good to have the delay between received bytes about 200 microseconds.
    ; In this case the receiver will be able to handle new byte properly.

uart_isr:
    ; Enter interrupt service routine ------------------------------------------
    push    YL                      ; 2   Delay loop counter
    push    YH                      ; 2   Data bits shift register
    in      YH, SREG                ; 1   This ISR needs for 4+2 bytes of SRAM
    push    YH                      ; 2   to save registers and return address

    ; Read data bits from LSB to MSB and wait for stop bit ---------------------
    ldi     YL, BIT_DELAY_15        ; 1   Delay for 1.5 bit (0.5*START+1.0*DATA) 
    ldi     YH, 0x80                ; 1   Bit shift counter
data_bit_loop:
    subi    YL, 1                   ; 1   Decrement and clear carry
    brne    data_bit_loop           ; 1|2 Go to next iteration
    ldi     YL, BIT_DELAY_10        ; 1   Load delay for next bit
    sbic    PINB, PORTB2            ; 1|2 Check UART RX PIN
    sec                             ; 1   Set carry if RX is HIGH
    ror     YH                      ; 1   Shift register loading carry to bit7
    brcc    data_bit_loop           ; 1|2 Loop through shift register
stop_bit_loop:
    dec     YL                      ; 1   Stop bit delay loop
    brne    stop_bit_loop           ; 1|2 Go to next iteration

    ; Handle received byte according to the protocol ---------------------------
    sbrs    raddr, WF_REG           ;     Skip next instruction if waiting for 
    rjmp    reg_data_received       ;     incoming register address
    cpi     YH, 0x10                ;     Check if received byte is a valid
    brsh    reg_addr_exit           ;     register addres, otherwise try to sync
    mov     raddr, YH               ;     Received data is a register address,
    rjmp    reg_addr_exit           ;     so save it and exit
reg_data_received:
    push    ZL                      ;     We need one more register for indexing
    ldi     ZL, low(P(reg_mask))    ;     Read register mask from FLASH for
    ldp     ZL, raddr               ;     current register address
    and     ZL, YH                  ;     Apply mask for received register data
    ldi     YL, low (psg_regs)      ;     Store register data to the SRAM
    ldi     YH, high(psg_regs)
    add     YL, raddr
    st      Y, ZL
    pop     ZL                      ;     Restore register from stack
    cpi     raddr, 0x0D             ;     Check if register address is an
    brne    reg_data_exit           ;     envelope shape register address
    sbr     flags, M(EG_RES)        ;     Set envelope generator reset flag
reg_data_exit:
    sbr     raddr, M(WF_REG)        ;     Wait for next byte as register address
reg_addr_exit:

    ; Exit interrupt service routine -------------------------------------------
    pop     YH                      ;     Restore used registers from stack
    out     SREG, YH                ;
    pop     YH                      ;
    pop     YL                      ;
    reti                            ;     Return from ISR

; MAIN LOOP --------------------------------------------------------------------
.macro tone_generator
    ; AL is FDIV constant
    ; @0 is period register
    ; @1 is counter
    ; @2 is channel bit
    ; MAX CYCLES: 24
    ; MIN CYCLES: 12
    lds     XL, L(@0)               ; 1   Load tone period from SRAM
    lds     XH, H(@0)               ; 1   Tone period high byte
    lds     YL, L(@1)               ; 1   Load tone counter from SRAM
    lds     YH, H(@1)               ; 1   Tone counter high byte
    cp      YL, XL                  ; 1   Compare counter against period
    cpc     YH, XH                  ; 1
    brlo    exit_tone               ; 1|2 Skip following if counter < period
    sub     YL, XL                  ; 1   counter = counter - period
    sbc     YH, XH                  ; 1
    cp      YL, AL                  ; 1   Compare counter against fdiv
    cpc     YH, ZERO                ; 1
    brlo    toggle_flip_flop        ; 1|2 Skip following if counter < fdiv
    clr     YL                      ; 1   Reset counter
    clr     YH                      ; 1
toggle_flip_flop:
    ldi     AH, M(@2)               ; 1   Load tone mask
    eor     flags, AH               ; 1   Toggle tone flip-flop
    cp      XL, AL                  ; 1   Compare period against fdiv
    cpc     XH, ZERO                ; 1
    brsh    exit_tone               ; 1|2 Skip following if period >= fdiv
    or      flags, AH               ; 1   Lock flip-flop in high state
exit_tone:
    add     YL, AL                  ; 1   counter = counter + fdiv
    adc     YH, ZERO                ; 1
    sts     L(@1), YL               ; 1   Save tone counter into SRAM
    sts     H(@1), YH               ; 1   Tone counter high byte
.endm

.macro reset_envelope_generator
    ; MAX CYCLES: 12
    ; MIN CYCLES: 3
    sbrs    flags, EG_RES           ; 1|2 Skip next instruction if no need to
    rjmp    exit_envelope_reset     ; 2   reset envelope generator
    cbr     flags, M(EG_RES)        ; 1   Clear request for reset envelope
    sts     L(e_counter), ZERO      ; 1   Reset envelope counter
    sts     H(e_counter), ZERO      ; 1
    lds     e_gen, e_shape          ; 1   Init envelope generator with a new
    lsl     e_gen                   ; 1   shape, index in table is shape * 4
    lsl     e_gen                   ; 1
    ldi     ZL, low(P(envelopes)+1) ; 1   Init envelope step with value from
    ldp     e_stp, e_gen            ; 3   envelope generator table
exit_envelope_reset:
.endm

.macro noise_envelope_generator
    ; MAX CYCLES (fdiv = 0x08): 9 + fdiv * (16 + 20 + 3) + 3 = 324
    ; MIN CYCLES (fdiv = 0x08): 9 + fdiv * (4 + 4 + 3) + 3 = 100
    ; Read state from SRAM and init loop ------------------------------[..9|..9]
    lds     XL, L(e_period)         ; 1   Load envelope peiod from SRAM
    lds     XH, H(e_period)         ; 1   Envelope period high byte
    lds     YL, L(e_counter)        ; 1   Load envelope counter from SRAM
    lds     YH, H(e_counter)        ; 1   Envelope counter high byte
    lds     BL, L(n_shifter)        ; 1   Load noise shifter from SRAM
    lds     BH, H(n_shifter)        ; 1   Noise shifter high byte
    lds     AL, n_period            ; 1   Load noise period from SRAM and double
    lsl     AL                      ; 1   it to simulate noise prescaler
    ldi     AH, FDIV                ; 1   Init update iterations
iteration_loop:

    ; Update noise generator ------------------------------------------[.16|..4]
    cp      n_cnt, AL               ; 1   Compare counter against period
    brlo    exit_noise              ; 1|2 Skip following if counter < period
    clr     n_cnt                   ; 1   Reset counter
    bst     BL, bit3                ; 1   Compute the feedback based on
    bld     ZL, bit0                ; 1   bit3 xor bit0 of the shifter
    eor     ZL, BL                  ; 1
    ror     BH                      ; 1   Shift 17-bit shifter, bit16 is located
    ror     BL                      ; 1   in the bit7 of the flags regster
    bst     flags, NS_B16           ; 1
    bld     BH, bit7                ; 1
    bst     ZL, bit0                ; 1   Store feedback as bit16 of the shifter
    bld     flags, NS_B16           ; 1   in bit7 of the flags register
    cbr     flags, 0b00111000       ; 1   Set noise output flags according to
    sbrc    BL, bit0                ; 1|2 bit0 of the current shifter state
    sbr     flags, 0b00111000       ; 1
exit_noise:
    inc     n_cnt                   ; 1   counter = counter + 1

    ; Update envelope generator ---------------------------------------[.20|..6]
    cp      YL, XL                  ; 1   Compare counter against period
    cpc     YH, XH                  ; 1
    brlo    exit_envelope           ; 1|2 Skip following if counter < period
    clr     YL                      ; 1   Reset counter
    clr     YH                      ; 1
    ldi     ZL, low(P(envelopes))   ; 1   Inc/dec envelope step counter depend-
    ldp     ZL, e_gen               ; 3   ing on envelope generation config
    add     e_stp, ZL               ; 1
    cpi     e_stp, 0x20             ; 1   When envelope step reaches 0x20 after
    brlo    exit_envelope           ; 1|2 increment or get 0xFF after decrement
    ldi     ZL, 0b00000010          ; 1   then generation config switches to the
    eor     e_gen, ZL               ; 1   alteravive phase and envelope step 
    ldi     ZL, low(P(envelopes)+1) ; 1   reloads with a new value from config
    ldp     e_stp, e_gen            ; 3
exit_envelope:
    ld      ZL, Y+                  ; 2   counter = counter + 1

    ; Go to the next iteration or exit --------------------------------[..6|..3]
    dec     AH                      ; 1   Decrement iteration counter and
    brne    iteration_loop          ; 1|2 go to next interation
    sts     L(n_shifter), BL        ; 1   Save noise shifter into SRAM
    sts     H(n_shifter), BH        ; 1   Noise shifter high byte
    sts     L(e_counter), YL        ; 1   Save envelope counter into SRAM
    sts     H(e_counter), YH        ; 1   Envelope counter high byte
.endm

.macro sample_generator
    ; AL is envelope sample
    ; AH is mixer output
    ; BL is amplitude table offset
    ; @0 is volume register
    ; @1 is channel bit
    ; @2 is output sample
    ; MAX CYCLES: 9
    ; MIN CYCLES: 7
    mov     @2, AL                  ; 1   Use envelope amplitude by default
    lds     ZL, @0                  ; 1   volume+envelope flag from PSG register
    bst     ZL, bit4                ; 1   Check if envelope enabled in this
    brts    use_envelope            ; 1|2 channel and skip amplitude computation
    ldp     @2, BL                  ; 3   Get amplitude from table using volume
use_envelope:
    sbrs    AH, @1                  ; 1|2 If channel disabled in mixer (N and T)
    clr     @2                      ; 1   then set amplitude to zero value
.endm

loop:
    ; MAX CYCLES: 6 + 73 + 336 + 6 + 32 + 5 = 458
    ; MIN CYCLES: 6 + 37 + 103 + 6 + 26 + 5 = 183
    ; AVG CYCLES: (457 + 182) / 2 = 320

    ; Waiting for timer overflow and outputting samples ---------------[..6|..4]
    in      AL, TIMER_OVF_REG       ; 1   Check timer0 overflow flag TOV0
    sbrs    AL, TIMER_OVF_FLG       ; 1|2 Skip next instruction if TOV0 is set
    rjmp    loop                    ; 2   otherwise jump to the loop beginning
    out     TIMER_OVF_REG, AL       ; 1   Clear timer overflow flag
    out     PWM_CHANNEL_A, XL       ; 1   Output L/R channel 8-bit amplitude or
    out     PWM_CHANNEL_B, XH       ; 1   single mono channel 16-bit amplitude

    ; Update tone generators ------------------------------------------[.73|.37]
    ldi     AL, FDIV                ; 1
    tone_generator a_period, a_counter, chA ; max:24 min:12
    tone_generator b_period, b_counter, chB ; max:24 min:12
    tone_generator c_period, c_counter, chC ; max:24 min:12

    ; Update noise and envelope generators ----------------------------[336|103]
    reset_envelope_generator        ; max:12  min:3
    noise_envelope_generator        ; max:324 min:100

    ; Apply enable flags from mixer -----------------------------------[..6|..6]
    lds     AH, mixer               ; 1   Mixer: xxCB.Acba (CBA:Noise, cba:Tone)
    or      AH, flags               ; 1   Apply disables:  xxCB.Acba | xxNN.Ncba
    mov     AL, AH                  ; 1
    lsl     AL                      ; 1   Shift left:      xxCB.Acba > xCBA.cbax
    swap    AL                      ; 1   Swap nibbles:    xCBA.cbax > cbax.xCBA
    and     AH, AL                  ; 1   Output:          xxxx.xcba & xxxx.xCBA

    ; Compute sample for each channel ---------------------------------[.32|.26]
    ldi     ZL, low(P(amp_5bit))    ; 1   Get envelope amplitude from table
    ldp     AL, e_stp               ; 3   using envelope step as index
    ldi     BL, low(P(amp_4bit))    ; 1
    sample_generator a_volume, chA, XL ; max:9 min:7
    sample_generator b_volume, chB, BH ; max:9 min:7
    sample_generator c_volume, chC, XH ; max:9 min:7

    ; Complute left/right channels samples  ---------------------------[..5|..5]
    lsr     BH                      ; 1   Divide B channel by 2
    add     XL, BH                  ; 1   Left  = Add B channel to A channel
    add     XH, BH                  ; 1   Right = Add B channel to C channel
    rjmp    loop                    ; 2   Go to main loop next iteration

; ------------------------------------------------------------------------------
; SRAM
; ------------------------------------------------------------------------------

    .dseg
    .org    SRAM_START

psg_regs:
a_period:   .byte 2
b_period:   .byte 2
c_period:   .byte 2
n_period:   .byte 1
mixer:      .byte 1
a_volume:   .byte 1
b_volume:   .byte 1
c_volume:   .byte 1
e_period:   .byte 2
e_shape:    .byte 1
port_a:     .byte 1
port_b:     .byte 1
a_counter:  .byte 2
b_counter:  .byte 2
c_counter:  .byte 2
n_shifter:  .byte 2
e_counter:  .byte 2
psg_end:
