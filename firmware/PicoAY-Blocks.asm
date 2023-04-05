; ==============================================================================
; GLOBAL DEFINES
; ==============================================================================

    .equ    BAUD_RATE   = 57600
    .equ    SAMPLE_RATE = (F_CPU / (TIMER_TOP + 1))
    .equ    U_STEP      = (F_PSG / 8 / SAMPLE_RATE)

.if U_STEP < 0x04 || U_STEP > 0x08
    .error "Update step is out of range"
.endif

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

    .equ    bit0    = 0
    .equ    bit1    = 1
    .equ    bit2    = 2
    .equ    bit3    = 3
    .equ    bit4    = 4
    .equ    bit5    = 5
    .equ    bit6    = 6
    .equ    bit7    = 7

    .equ    chA     = bit0
    .equ    chB     = bit1
    .equ    chC     = bit2

    .equ    NS_B16  = bit7          ; Noise shifter bit16
    .equ    EG_RES  = bit6          ; Envelope generator reset
    .equ    WF_REG  = bit4          ; Waiting for register address

    #define M(bit)  (1 << bit)
    #define L(addr) (addr + 0)
    #define H(addr) (addr + 1)
    #define P(addr) (2 * addr)

; ==============================================================================
; DATA BLOCKS
; ==============================================================================

.listmac

; VALUE MASK FOR EACH PSG REGISTER ---------------------------------------------
.macro __reg_mask
    ; 16 bytes
    .db     0xFF, 0x0F, 0xFF, 0x0F, 0xFF, 0x0F, 0x1F, 0x3F
    .db     0x1F, 0x1F, 0x1F, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF
.endmacro
#define data_reg_mask() \
reg_mask: __reg_mask

; 4-BIT VOLUME TO AMPLITUDE ----------------------------------------------------
.macro __amp_4bit
    ; @0 maximum amplitude
    ; 16 bytes
.if @0 >= 0x00 && @0 <= 0xFF
    .db     int(0.0 + @0 * 0.0056), int(0.5 + @0 * 0.0079)
    .db     int(0.5 + @0 * 0.0112), int(0.5 + @0 * 0.0158)
    .db     int(0.5 + @0 * 0.0224), int(0.5 + @0 * 0.0316)
    .db     int(0.5 + @0 * 0.0447), int(0.5 + @0 * 0.0631)
    .db     int(0.5 + @0 * 0.0891), int(0.5 + @0 * 0.1259)
    .db     int(0.5 + @0 * 0.1778), int(0.5 + @0 * 0.2512)
    .db     int(0.5 + @0 * 0.3548), int(0.5 + @0 * 0.5012)
    .db     int(0.5 + @0 * 0.7079), int(0.5 + @0 * 1.0000)
.else
    .error "Maximum amplitude is out of range"
.endif
.endmacro
#define data_amp_4bit(MAX_AMP) \
amp_4bit: __amp_4bit MAX_AMP

; 5-BIT VOLUME TO AMPLITUDE ----------------------------------------------------
.macro __amp_5bit
    ; @0 maximum amplitude
    ; 32 bytes
.if @0 >= 0x00 && @0 <= 0xFF
    .db     int(0.0 + @0 * 0.0047), int(0.5 + @0 * 0.0056)
    .db     int(0.5 + @0 * 0.0067), int(0.5 + @0 * 0.0079)
    .db     int(0.5 + @0 * 0.0094), int(0.5 + @0 * 0.0112)
    .db     int(0.5 + @0 * 0.0133), int(0.5 + @0 * 0.0158)
    .db     int(0.5 + @0 * 0.0188), int(0.5 + @0 * 0.0224)
    .db     int(0.5 + @0 * 0.0266), int(0.5 + @0 * 0.0316)
    .db     int(0.5 + @0 * 0.0376), int(0.5 + @0 * 0.0447)
    .db     int(0.5 + @0 * 0.0531), int(0.5 + @0 * 0.0631)
    .db     int(0.5 + @0 * 0.0750), int(0.5 + @0 * 0.0891)
    .db     int(0.5 + @0 * 0.1059), int(0.5 + @0 * 0.1259)
    .db     int(0.5 + @0 * 0.1496), int(0.5 + @0 * 0.1778)
    .db     int(0.5 + @0 * 0.2113), int(0.5 + @0 * 0.2512)
    .db     int(0.5 + @0 * 0.2985), int(0.5 + @0 * 0.3548)
    .db     int(0.5 + @0 * 0.4217), int(0.5 + @0 * 0.5012)
    .db     int(0.5 + @0 * 0.5957), int(0.5 + @0 * 0.7079)
    .db     int(0.5 + @0 * 0.8414), int(0.5 + @0 * 1.0000)
.else
    .error "Maximum amplitude is out of range"
.endif
.endmacro
#define data_amp_5bit(MAX_AMP) \
amp_5bit: __amp_5bit MAX_AMP

; ENVELOPE GENERATION INSTRUCTIONS ---------------------------------------------
.macro __envelopes
    ; @0 number of steps in waveform
    ; 64 bytes
.if @0 == 16 || @0 == 32
    .equ    _inc =  1
    .equ    _dec = -1
    .equ    _hld =  0
    .equ    _top = @0-1
    .equ    _bot = 0x00
    .db     _dec, _top, _hld, _bot  ; 0
    .db     _dec, _top, _hld, _bot  ; 1
    .db     _dec, _top, _hld, _bot  ; 2
    .db     _dec, _top, _hld, _bot  ; 3
    .db     _inc, _bot, _hld, _bot  ; 4
    .db     _inc, _bot, _hld, _bot  ; 5
    .db     _inc, _bot, _hld, _bot  ; 6
    .db     _inc, _bot, _hld, _bot  ; 7
    .db     _dec, _top, _dec, _top  ; 8
    .db     _dec, _top, _hld, _bot  ; 9
    .db     _dec, _top, _inc, _bot  ; A
    .db     _dec, _top, _hld, _top  ; B
    .db     _inc, _bot, _inc, _bot  ; C
    .db     _inc, _bot, _hld, _top  ; D
    .db     _inc, _bot, _dec, _top  ; E
    .db     _inc, _bot, _hld, _bot  ; F
.else
    .error "Unknown number of steps for envelope"
.endif
.endmacro
#define data_envelopes(STEPS) \
envelopes: __envelopes STEPS

; ==============================================================================
; CODE BLOCKS
; ==============================================================================

; CLEAR SRAM VARIABLES AND SET STACK POINTER -----------------------------------
.macro __setup_sram
    clr     ZERO                    ; Always zero value used across the code

    ; Clear SRAM variables
    ldi     ZL, low(psg_regs)       ; Setup start address in SRAM
    ldi     ZH, high(psg_regs)      ;
    ldi     AL, psg_end-psg_regs    ; SRAM size to be cleared
sram_clear_loop:                    ;
    st      Z+, ZERO                ;
    dec     AL                      ;
    brne    sram_clear_loop         ;

    ; Set stack pointer
    ldi     AL, low(RAMEND)         ;
    out     SPL, AL                 ;
.ifdef SPH
    ldi     AL, high(RAMEND)        ;
    out     SPH, AL                 ;
.endif
.endmacro
#define code_setup_sram() \
__setup_sram

; SET Z POINTER TO FIRST 256 BYTES OF FLASH TO ASSESS DATA ---------------------
.macro __setup_data_access
.ifdef MAPPED_FLASH_START
    ldi     ZH, high(MAPPED_FLASH_START)
.else
    ldi     ZH, 0x00
.endif
.endmacro
#define code_setup_data_access() \
__setup_data_access

; SETUP EVERYTHING ELSE AND START EMULATOR -------------------------------------
.macro __setup_and_start_emulator
    ldi     flags, M(NS_B16) | M(EG_RES)
    ldi     raddr, M(WF_REG)        ; Wait the register address to write
    mov     XL, ZERO                ; Load zero to left channel sample register
    mov     XH, ZERO                ; Load zero to right channel sample register
    sei                             ; Enable interrupts
    rjmp    loop                    ; Go to main loop
.endmacro
#define code_setup_and_start_emulator() \
__setup_and_start_emulator

; LOAD DATA FROM FLASH USING TABLE BASE AND DISPLACEMENT -----------------------
.macro ldp
    ; ZL table base or displasement
    ; @0 destination register
    ; @1 displacement or table base
    ; AVR8L:3/3 V2:4/4
#ifdef __CORE_AVR8L_0__
    add     ZL, @1                  ; 1   Add table base with displacement
    ld      @0, Z                   ; 2   Load indirrect from Z
#elif __CORE_V2__
    add     ZL, @1                  ; 1   Add table base with displacement
    lpm     @0, Z                   ; 3   Load indirrect from Z
#else
    .error "Unknown AVR core version"
#endif
.endmacro

; HANDLE UART RECEIVED DATA ACCORDING TO PROTOCOL ------------------------------
.macro __handle_uart_data
    ; YH received data byte
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
.endmacro
#define code_handle_uart_data() \
__handle_uart_data

; SOFTWARE UART DATA RECEIVE ISR -----------------------------------------------
.macro __sw_uart_rx_isr
    ; @0 mcu port for UART RX input
    ; @1 mcu pin number in port for UART RX

    .equ    BIT_DURATION = (F_CPU / BAUD_RATE)
    .equ    BIT_DELAY_10 = int((1.0 * BIT_DURATION - 5 + 1.5) / 3)
    .equ    BIT_DELAY_15 = int((1.5 * BIT_DURATION - 9 + 1.5) / 3)
    ; It's good to have the delay between received bytes about 200 microseconds.
    ; In this case the receiver will be able to handle new byte properly.

    ; Enter interrupt service routine
    push    YL                      ; 2   Delay loop counter
    push    YH                      ; 2   Data bits shift register
    in      YH, SREG                ; 1   This ISR needs for 4+2 bytes of SRAM
    push    YH                      ; 2   to save registers and return address

    ; Read data bits from LSB to MSB and wait for stop bit
    ldi     YL, BIT_DELAY_15        ; 1   Delay for 1.5 bit (0.5*START+1.0*DATA) 
    ldi     YH, 0x80                ; 1   Bit shift counter
data_bit_loop:
    subi    YL, 1                   ; 1   Decrement and clear carry
    brne    data_bit_loop           ; 1|2 Go to next iteration
    ldi     YL, BIT_DELAY_10        ; 1   Load delay for next bit
    sbic    @0, @1                  ; 1|2 Check UART RX PIN
    sec                             ; 1   Set carry if RX is HIGH
    ror     YH                      ; 1   Shift register loading carry to bit7
    brcc    data_bit_loop           ; 1|2 Loop through shift register
stop_bit_loop:
    dec     YL                      ; 1   Stop bit delay loop
    brne    stop_bit_loop           ; 1|2 Go to next iteration

    ; Handle received byte in register YH
    code_handle_uart_data()

    ; Exit interrupt service routine
    pop     YH                      ;     Restore used registers from stack
    out     SREG, YH                ;
    pop     YH                      ;
    pop     YL                      ;
    reti                            ;     Return from ISR
.endmacro
#define code_sw_uart_rx_isr(PORT, PIN) \
sw_uart_rx_isr: __sw_uart_rx_isr PORT, PIN

; HARDWARE UART DATA RECEIVE ISR -----------------------------------------------
.macro __hw_uart_rx_isr
    ; TODO
.endmacro
#define code_hw_uart_rx_isr() \
hw_uart_isr: __hw_uart_rx_isr

; SYNCHRONIZE IN TIME AND OUTPUT PREVIOUSLY COMPUTED RESULT --------------------
.macro __sync_and_out
    ; @0 mcu register containing timer overflow flag
    ; @1 mcu timer overflow flag bit in register
    ; @2 PWM channel A
    ; @3 PWM channel B
    ; AVR8L:6-4 V2:6-4
    in      AL, @0                  ; 1   Check timer overflow flag
    sbrs    AL, @1                  ; 1|2 Skip next instruction if flag is set
    rjmp    loop                    ; 2   otherwise jump to the loop beginning
    out     @0, AL                  ; 1   Clear timer overflow flag
    out     @2, XL                  ; 1   Output L/R channel 8-bit samples or
    out     @3, XH                  ; 1   mono channel 16-bit sample
.endmacro
#define code_sync_and_out(TOVF_R, TOVF_F, PWM_A, PWM_B) \
__sync_and_out TOVF_R, TOVF_F, PWM_A, PWM_B

; UPDATE TONE GENERATOR --------------------------------------------------------
.macro __update_tone
    ; AL U_STEP constant
    ; @0 channel tone period
    ; @1 channel tone counter
    ; @2 channel bit
    ; AVR8L:24-12 V2:30-18
    lds     XL, L(@0)               ; 1~2 Load tone period from SRAM
    lds     XH, H(@0)               ; 1~2 Tone period high byte
    lds     YL, L(@1)               ; 1~2 Load tone counter from SRAM
    lds     YH, H(@1)               ; 1~2 Tone counter high byte
    cp      YL, XL                  ; 1   Compare counter against period
    cpc     YH, XH                  ; 1
    brlo    exit_tone               ; 1|2 Skip following if counter < period
    sub     YL, XL                  ; 1   counter = counter - period
    sbc     YH, XH                  ; 1
    cp      YL, AL                  ; 1   Compare counter against U_STEP
    cpc     YH, ZERO                ; 1
    brlo    toggle_flip_flop        ; 1|2 Skip following if counter < U_STEP
    clr     YL                      ; 1   Reset counter
    clr     YH                      ; 1
toggle_flip_flop:
    ldi     AH, M(@2)               ; 1   Load tone mask
    eor     flags, AH               ; 1   Toggle tone flip-flop
    cp      XL, AL                  ; 1   Compare period against U_STEP
    cpc     XH, ZERO                ; 1
    brsh    exit_tone               ; 1|2 Skip following if period >= U_STEP
    or      flags, AH               ; 1   Lock flip-flop in high state
exit_tone:
    add     YL, AL                  ; 1   counter = counter + U_STEP
    adc     YH, ZERO                ; 1
    sts     L(@1), YL               ; 1~2 Save tone counter into SRAM
    sts     H(@1), YH               ; 1~2 Tone counter high byte
.endmacro
#define code_update_tone(PERIOD, COUNTER, CHANNEL) \
__update_tone PERIOD, COUNTER, CHANNEL

; RESET ENVELOPE GENERATOR -----------------------------------------------------
.macro __reset_envelope
    ; AVR8L:12-3 V2:16-3
    sbrs    flags, EG_RES           ; 1|2 Skip next instruction if no need to
    rjmp    exit_envelope_reset     ; 2   reset envelope generator
    cbr     flags, M(EG_RES)        ; 1   Clear request for reset envelope
    sts     L(e_counter), ZERO      ; 1~2 Reset envelope counter
    sts     H(e_counter), ZERO      ; 1~2
    lds     e_gen, e_shape          ; 1~2 Init envelope generator with a new
    lsl     e_gen                   ; 1   shape, index in table is shape * 4
    lsl     e_gen                   ; 1
    ldi     ZL, low(P(envelopes)+1) ; 1   Init envelope step with value from
    ldp     e_stp, e_gen            ; 3~4 envelope generator table
exit_envelope_reset:
.endmacro
#define code_reset_envelope() \
__reset_envelope

; UPDATE NOISE AND ENVELOPE GENERATORS -----------------------------------------
.macro __update_noise_envelope
    ; AVR8L (U_STEP = 0x08): 324-116
    ; 9 + U_STEP * (16 + 20 + 3) + 3 
    ; 9 + U_STEP * (4 + 6 + 3) + 3 
    ; V2 (U_STEP = 0x08): 351-127
    ; 16 + U_STEP * (16 + 22 + 3) + 7 
    ; 16 + U_STEP * (4 + 6 + 3) + 7 

    ; Read state from SRAM and init loop
    ; AVR8L:9-9 V2:16-16
    lds     XL, L(e_period)         ; 1~2 Load envelope peiod from SRAM
    lds     XH, H(e_period)         ; 1~2 Envelope period high byte
    lds     YL, L(e_counter)        ; 1~2 Load envelope counter from SRAM
    lds     YH, H(e_counter)        ; 1~2 Envelope counter high byte
    lds     BL, L(n_shifter)        ; 1~2 Load noise shifter from SRAM
    lds     BH, H(n_shifter)        ; 1~2 Noise shifter high byte
    lds     AL, n_period            ; 1~2 Load noise period from SRAM and double
    lsl     AL                      ; 1   it to simulate noise prescaler
    ldi     AH, U_STEP              ; 1   Init update iterations
iteration_loop:

    ; Update noise generator
    ; AVR8L:16-4 V2:16-4
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

    ; Update envelope generator
    ; AVR8L:20-6 V2:22-6
    cp      YL, XL                  ; 1   Compare counter against period
    cpc     YH, XH                  ; 1
    brlo    exit_envelope           ; 1|2 Skip following if counter < period
    clr     YL                      ; 1   Reset counter
    clr     YH                      ; 1
    ldi     ZL, low(P(envelopes))   ; 1   Inc/dec envelope step counter depend-
    ldp     ZL, e_gen               ; 3~4 ing on envelope generation config
    add     e_stp, ZL               ; 1
    cpi     e_stp, 0x20             ; 1   When envelope step reaches 0x20 after
    brlo    exit_envelope           ; 1|2 increment or get 0xFF after decrement
    ldi     ZL, 0b00000010          ; 1   then generation config switches to the
    eor     e_gen, ZL               ; 1   alteravive phase and envelope step 
    ldi     ZL, low(P(envelopes)+1) ; 1   reloads with a new value from config
    ldp     e_stp, e_gen            ; 3~4
exit_envelope:
    ld      ZL, Y+                  ; 2   counter = counter + 1

    ; Go to the next iteration or exit
    ; AVR8L:6-3 V2:10-3
    dec     AH                      ; 1   Decrement iteration counter and
    brne    iteration_loop          ; 1|2 go to next interation
    sts     L(n_shifter), BL        ; 1~2 Save noise shifter into SRAM
    sts     H(n_shifter), BH        ; 1~2 Noise shifter high byte
    sts     L(e_counter), YL        ; 1~2 Save envelope counter into SRAM
    sts     H(e_counter), YH        ; 1~2 Envelope counter high byte
.endmacro
#define code_update_noise_envelope() \
__update_noise_envelope

; APPLY MIXER FLAGS AND COMPUTE TONE/NOISE LEVELS ------------------------------
.macro __apply_mixer
    ; AH tone/noise level bits output
    ; AVR8L:6-7 V2:6-7
    lds     AH, mixer               ; 1~2 Mixer: xxCB.Acba (CBA:Noise, cba:Tone)
    or      AH, flags               ; 1   Apply disables:  xxCB.Acba | xxNN.Ncba
    mov     AL, AH                  ; 1
    lsl     AL                      ; 1   Shift left:      xxCB.Acba > xCBA.cbax
    swap    AL                      ; 1   Swap nibbles:    xCBA.cbax > cbax.xCBA
    and     AH, AL                  ; 1   Output:          xxxx.xcba & xxxx.xCBA
.endmacro
#define code_apply_mixer() \
__apply_mixer

; COMPUTE CHANNEL SAMPLE -------------------------------------------------------
.macro __compute_sample
    ; AL envelope sample
    ; AH mixer output
    ; BL amplitude table offset
    ; @0 channel volume
    ; @1 channel bit
    ; @2 output sample
    ; AVR8L:9-7 V2:11-8
    mov     @2, AL                  ; 1   Use envelope amplitude by default
    lds     ZL, @0                  ; 1~2 volume+envelope flag from PSG register
    bst     ZL, bit4                ; 1   Check if envelope enabled in this
    brts    use_envelope            ; 1|2 channel and skip amplitude computation
    ldp     @2, BL                  ; 3~4 Get amplitude from table using volume
use_envelope:
    sbrs    AH, @1                  ; 1|2 If channel disabled in mixer (N and T)
    clr     @2                      ; 1   then set amplitude to zero value
.endmacro
#define code_compute_sample(VOLUME, CHANNEL, SAMPLE) \
__compute_sample VOLUME, CHANNEL, SAMPLE

; COMPUTE STEREO 8-BIT OR MONO 16-BIT OUTPUT -----------------------------------
    .equ MONO       = 0             ;
    .equ STEREO_ABC = 1             ;
    .equ STEREO_ACB = 2             ;

.macro __compute_output
    ; XL channel A sample -> stereo L sample / mono sample LSB
    ; BH channel B sample
    ; XH channel C sample -> stereo R sample / mono sample MSB
    ; @0 output type
.if @0 == MONO
    ; TODO
.elif @0 == STEREO_ABC
    ; AVR8L:3-3 V2:3-3
    lsr     BH                      ; 1   Divide B channel sample by 2
    add     XL, BH                  ; 1   Left  = Add B channel to A channel
    add     XH, BH                  ; 1   Right = Add B channel to C channel
.elif @0 == STEREO_ACB
    ; AVR8L:3-3 V2:3-3
    lsr     XH                      ; 1   Divide C channel sample by 2
    add     XL, XH                  ; 1   Left  = Add C channel to A channel
    add     XH, BH                  ; 1   Right = Add C channel to B channel
.else
    .error "Unknown output mode"
.endif
.endmacro
#define code_compute_output(TYPE) \
__compute_output TYPE

; ==============================================================================
; SRAM BLOCKS
; ==============================================================================

#define sram_psg_regs_and_state()   \
psg_regs:                           \
a_period:   .byte 2                 \
b_period:   .byte 2                 \
c_period:   .byte 2                 \
n_period:   .byte 1                 \
mixer:      .byte 1                 \
a_volume:   .byte 1                 \
b_volume:   .byte 1                 \
c_volume:   .byte 1                 \
e_period:   .byte 2                 \
e_shape:    .byte 1                 \
port_a:     .byte 1                 \
port_b:     .byte 1                 \
a_counter:  .byte 2                 \
b_counter:  .byte 2                 \
c_counter:  .byte 2                 \
n_shifter:  .byte 2                 \
e_counter:  .byte 2                 \
psg_end: