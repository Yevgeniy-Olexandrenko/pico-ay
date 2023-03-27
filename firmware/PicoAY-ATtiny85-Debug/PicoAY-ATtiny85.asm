;                                                                              ;
;                                   ATtiny85                                   ;
;                                  +--------+                                  ;
;                   (/RESET) PB5 --|*1    8 |-- VCC                            ;
;                            PB3 --| 2    7 |-- PB2 (UART_RX)                  ;
;                            PB4 --| 3    6 |-- PB1 (PWM_OUT_R)                ;
;                            GND --| 4    5 |-- PB0 (PWM_OUT_L)                ;
;                                  +--------+                                  ;
;                                                                              ;

#if 0
; ------------------------------------------------------------------------------
; DEFINES
; ------------------------------------------------------------------------------

    .equ    F_CPU = (30420 * 256)   ;8000000
    .equ    BAUD_RATE = 57600
    .equ    BIT_DURATION = (F_CPU / BAUD_RATE)
    .equ    MAX_AMP = 170

    .def    FDIV    = r16           ;
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

    .equ    bit0 = 0
    .equ    bit1 = 1
    .equ    bit2 = 2
    .equ    bit3 = 3
    .equ    bit4 = 4
    .equ    bit5 = 5
    .equ    bit6 = 6
    .equ    bit7 = 7

    .equ    NS_B16 = bit7
    .equ    EG_RES = bit6

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

    .org    INT0addr
    rjmp    int0_isr                ; INT0  handler

; DATA TABLES  -----------------------------------------------------------------
reg_mask:   ; 16 bytes
    .db     0xFF, 0x0F, 0xFF, 0x0F, 0xFF, 0x0F, 0x1F, 0x3F
    .db     0x1F, 0x1F, 0x1F, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF

amp_4bit:   ; 16 bytes
    .db     int(0.0 + MAX_AMP * 0.0056), int(0.5 + MAX_AMP * 0.0079)
    .db     int(0.5 + MAX_AMP * 0.0112), int(0.5 + MAX_AMP * 0.0158)
    .db     int(0.5 + MAX_AMP * 0.0224), int(0.5 + MAX_AMP * 0.0316)
    .db     int(0.5 + MAX_AMP * 0.0447), int(0.5 + MAX_AMP * 0.0631)
    .db     int(0.5 + MAX_AMP * 0.0891), int(0.5 + MAX_AMP * 0.1259)
    .db     int(0.5 + MAX_AMP * 0.1778), int(0.5 + MAX_AMP * 0.2512)
    .db     int(0.5 + MAX_AMP * 0.3548), int(0.5 + MAX_AMP * 0.5012)
    .db     int(0.5 + MAX_AMP * 0.7079), int(0.5 + MAX_AMP * 1.0000)

amp_5bit:   ; 32 bytes
    .db     int(0.0 + MAX_AMP * 0.0047), int(0.5 + MAX_AMP * 0.0056)
    .db     int(0.5 + MAX_AMP * 0.0067), int(0.5 + MAX_AMP * 0.0079)
    .db     int(0.5 + MAX_AMP * 0.0094), int(0.5 + MAX_AMP * 0.0112)
    .db     int(0.5 + MAX_AMP * 0.0133), int(0.5 + MAX_AMP * 0.0158)
    .db     int(0.5 + MAX_AMP * 0.0188), int(0.5 + MAX_AMP * 0.0224)
    .db     int(0.5 + MAX_AMP * 0.0266), int(0.5 + MAX_AMP * 0.0316)
    .db     int(0.5 + MAX_AMP * 0.0376), int(0.5 + MAX_AMP * 0.0447)
    .db     int(0.5 + MAX_AMP * 0.0531), int(0.5 + MAX_AMP * 0.0631)
    .db     int(0.5 + MAX_AMP * 0.0750), int(0.5 + MAX_AMP * 0.0891)
    .db     int(0.5 + MAX_AMP * 0.1059), int(0.5 + MAX_AMP * 0.1259)
    .db     int(0.5 + MAX_AMP * 0.1496), int(0.5 + MAX_AMP * 0.1778)
    .db     int(0.5 + MAX_AMP * 0.2113), int(0.5 + MAX_AMP * 0.2512)
    .db     int(0.5 + MAX_AMP * 0.2985), int(0.5 + MAX_AMP * 0.3548)
    .db     int(0.5 + MAX_AMP * 0.4217), int(0.5 + MAX_AMP * 0.5012)
    .db     int(0.5 + MAX_AMP * 0.5957), int(0.5 + MAX_AMP * 0.7079)
    .db     int(0.5 + MAX_AMP * 0.8414), int(0.5 + MAX_AMP * 1.0000)

envelopes:  ; 64 bytes
    .equ    _inc =  1
    .equ    _dec = -1
    .equ    _hld =  0
    .equ    _top = 0x1F
    .equ    _bot = 0x00
    .db     _dec, _top, _hld, _bot
    .db     _dec, _top, _hld, _bot
    .db     _dec, _top, _hld, _bot
    .db     _dec, _top, _hld, _bot
    .db     _inc, _bot, _hld, _bot
    .db     _inc, _bot, _hld, _bot
    .db     _inc, _bot, _hld, _bot
    .db     _inc, _bot, _hld, _bot
    .db     _dec, _top, _dec, _top
    .db     _dec, _top, _hld, _bot
    .db     _dec, _top, _inc, _bot
    .db     _dec, _top, _hld, _top
    .db     _inc, _bot, _inc, _bot
    .db     _inc, _bot, _hld, _top
    .db     _inc, _bot, _dec, _top
    .db     _inc, _bot, _hld, _bot

; ENTRY POINT ------------------------------------------------------------------
main:
    ; Clear SRAM ---------------------------------------------------------------
    ldi     AL, SRAM_SIZE           ;
    ldi     AH, 0x00                ; Zero value used in initialization
    ldi     ZL, SRAM_START          ;
    mov     ZH, AH                  ;
clear_loop:                         ;
    st      Z+, AH                  ;
    dec     AL                      ;
    brne    clear_loop              ;

    ; Setup stack and access to FLASH ------------------------------------------
    ldi     AL, RAMEND
    out     SPL, AL
    out     SPH, AH
    ldi     ZH, high(MAPPED_FLASH_START)

    ; Setup main clock to 8 MHz ------------------------------------------------
    ldi     AL, 0xD8                ; Write correct signature to Configuration
    out     CCP, AL                 ; Change Protection register and set clock
    out     CLKPSR, AH              ; division factor to 1 for 8 MHz

    ; Setup external interrupt INT0 --------------------------------------------
#if 1
    cbi     DDRB,  PORTB2           ; Set PORTB2 as input
    sbi     PUEB,  PUEB2            ; Enable pull-up resistor on PORTB2
    cbi     EICRA, ISC00            ; Falling edge of INT0 generates an
    sbi     EICRA, ISC01            ; interrupt request
    sbi     EIMSK, INT0             ; Allow INT0 ISR execution
#endif

    ; Setup Timer0 for Fast PWM 8-bit with 0xFF top ----------------------------
    sbi     DDRB, PORTB0            ; Set PORTB0 and PORTB1 as output
    sbi     DDRB, PORTB1            ; for Fast PWM (OC0A and OC0B)
    ldi     AL, 0b10100001          ; Clear OC0A/OC0B on compare match
    out     TCCR0A, AL              ; COM0A1+COM0B1+WGM00 bits set
    ldi     AL, 0b00001001          ; Fast PWM with no prescaling
    out     TCCR0B, AL              ; WGM02+CS00 bits set

    ; Setup everything else ----------------------------------------------------
    ldi     FDIV,  0x07             ;
    ldi     flags, 0b11000111       ;
    sei                             ;
#if 0
    ldi     AL, 0b00111101
    sts     mixer, AL

    ldi     AL, 0x0F
    sts     a_volume, AL
    sts     c_volume, AL

    ldi     AL, 0x10
    sts     b_volume, AL

    ldi     AL, 0x01
    sts     a_period + 1, AL

    ldi     AL, 0x02
    sts     c_period + 1, AL

    ldi     AL, 0x10
    sts     b_period + 1, AL

    ldi     AL, 0x10
    sts     e_period + 0, AL

    ldi     AL, 0x0A
    sts     e_shape, AL
#endif
    rjmp    loop                    ;

; UART PROTOCOL ----------------------------------------------------------------
    .equ    STR_BIT_DELAY = (((BIT_DURATION / 2) - 9 + 3) / 3)
    .equ    DAT_BIT_DELAY = (( BIT_DURATION - 6) / 3)
    .equ    STP_BIT_DELAY = (( BIT_DURATION - 6) / 3)

    ; It's good to have the delay between received bytes about 200 microseconds.
    ; In this case the receiver will be able to handle new byte properly.

.macro uart_delay
    ldi     YL, @0                  ; 1   One iteration delays for 3 cpu cycles
delay_loop:
    dec     YL                      ; 1   Last iteration delays for 2 cpu cycles
    brne    delay_loop              ; 1|2 but loop init is taken into account
.endmacro

int0_isr:
    push    YL                      ; 2   Delay loop counter
    push    YH                      ; 2   Data bits shift register
    push    ZL                      ; 2   dData bits loop counter
    in      ZL, SREG                ; 1   This ISR needs for 4+2 bytes of SRAM
    push    ZL                      ; 2   to save registers and return address

    ; Delay for the middle of the start bit ------------------------------------
    uart_delay STR_BIT_DELAY

    ; Read data bits from LSB to MSB -------------------------------------------
    nop                             ; 1   For better delaying
    clr     YH                      ; 1   Clear data bits shift register
    ldi     ZL, 8                   ; 1   Init data bits loop counter
bit_read_loop:
    uart_delay DAT_BIT_DELAY
    lsr     YH                      ; 1   Shift bit register to the right
    sbic    PINB, PORTB2            ; 1|2 Skip next instruction if RX is clear
    ori     YH, 0x80                ; 1   Set data bit 7 if RX is set
    dec     ZL                      ; 1   Go to the next bit
    brne    bit_read_loop           ; 1|2
    nop                             ; 1   For better delaying

    ; Check if the stop bit is correct -----------------------------------------
    uart_delay STP_BIT_DELAY
    sbis    PINB, PORTB2            ; Skip next instruction if RX is set, it
    rjmp    exit_isr                ; means the stop bit is correct

    ; Handle received byte according to the protocol ---------------------------
    sbrs    raddr, bit4             ; Skip next instruction if waiting for 
    rjmp    reg_data_received       ; incoming register address
    cpi     YH, 0xF0                ; Check if received byte is a valid
    brlo    reg_addr_received       ; register addres, otherwise try to sync
    sbr     raddr, (1 << bit4)      ; Set reg address beyond allowed value to
    rjmp    exit_isr                ; indicate the waiting for allowed value
reg_addr_received:
    mov     raddr, YH               ; Received data is a register address,
    rjmp    exit_isr                ; so save it and exit
reg_data_received:
    ldi     ZL, low(2*reg_mask)     ; Read register mask from FLASH for
    add     ZL, raddr               ; current register address
    ld      ZL, Z
    and     ZL, YH                  ; Apply mask for received register data
    ldi     YL, low (psg_regs)      ; store register data to the SRAM
    ldi     YH, high(psg_regs)
    add     YL, raddr
    st      Y, ZL
    cpi     raddr, 0x0D             ; Check if register address is an
    brne    exit_isr                ; envelope shape register address
    sbr     flags, (1 << EG_RES)    ; Set envelope generator reset flag

    ; Exit interrupt service routine -------------------------------------------
exit_isr:
    pop     ZL
    out     SREG, ZL
    pop     ZL
    pop     YH
    pop     YL
    reti

; MAIN LOOP --------------------------------------------------------------------
.macro tone_generator
    ; AL must be 0x00
    ; @0 is period register
    ; @1 is counter
    ; @2 is channel mask
    lds     XL, @0 + 0              ; 1   Load tone period from SRAM
    lds     XH, @0 + 1              ; 1   Tone period high byte
    lds     YL, @1 + 0              ; 1   Load tone counter from SRAM
    lds     YH, @1 + 1              ; 1   Tone counter high byte
    cp      YL, XL                  ; 1   Compare counter against period
    cpc     YH, XH                  ; 1
    brlo    exit_tone               ; 1|2 Skip following if counter < period
    sub     YL, XL                  ; 1   counter = counter - period
    sbc     YH, XH                  ; 1
    cp      YL, FDIV                ; 1   Compare counter against FDIV
    cpc     YH, AL                  ; 1
    brlo    toggle_flip_flop        ; 1|2 Skip following if counter < FDIV
    clr     YL                      ; 1   Reset counter
    clr     YH                      ; 1
toggle_flip_flop:
    ldi     AH, @2                  ; 1   Load tone mask
    eor     flags, AH               ; 1   Toggle tone flip-flop
    cp      XL, FDIV                ; 1   Compare period against FDIV
    cpc     XH, AL                  ; 1
    brsh    exit_tone               ; 1|2 Skip following if period >= FDIV
    or      flags, AH               ; 1   Lock flip-flop in high state
exit_tone:
    add     YL, FDIV                ; 1   counter = counter + FDIV
    adc     YH, AL                  ; 1
    sts     @1 + 0, YL              ; 1   Save tone counter into SRAM
    sts     @1 + 1, YH              ; 1   Tone counter high byte
.endmacro                           ; MAX CYCLES: 24

.macro noise_envelope_generator
    ; Read state from SRAM and init loop ------------------------------[  9]----
    lds     XL, e_period  + 0       ; 1   Load envelope peiod from SRAM
    lds     XH, e_period  + 1       ; 1   Envelope period high byte
    lds     YL, e_counter + 0       ; 1   Load envelope counter from SRAM
    lds     YH, e_counter + 1       ; 1   Envelope counter high byte
    lds     BL, n_shifter + 0       ; 1   Load noise shifter from SRAM
    lds     BH, n_shifter + 1       ; 1   Noise shifter high byte
    lds     AL, n_period            ; 1   Load noise period from SRAM and double
    lsl     AL                      ; 1   it to simulate noise prescaler
    mov     AH, FDIV                ; 1   Init update iterations
iteration_loop:

    ; Update noise generator ------------------------------------------[ 16]----
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

    ; Update envelope generator ---------------------------------------[ 20]----
    cp      YL, XL                  ; 1   Compare counter against period
    cpc     YH, XH                  ; 1
    brlo    exit_envelope           ; 1|2 Skip following if counter < period
    clr     YL                      ; 1   Reset counter
    clr     YH                      ; 1
    ldi     ZL, low(2*envelopes)    ; 1   Increment or decrement envelope step
    add     ZL, e_gen               ; 1   counter depending on envelope genera-
    ld      ZL, Z                   ; 2   tion config
    add     e_stp, ZL               ; 1
    cpi     e_stp, 0x20             ; 1   When envelope step reaches 0x20 after
    brlo    exit_envelope           ; 1|2 increment or get 0xFF after decrement
    ldi     ZL, 0b00000010          ; 1   then generation config switches to the
    eor     e_gen, ZL               ; 1   alteravive phase and envelope step 
    ldi     ZL, low(2*envelopes+1)  ; 1   reloads with a new value from config
    add     ZL, e_gen               ; 1
    ld      e_stp, Z                ; 2
exit_envelope:
    ld      ZL, Y+                  ; 2   counter = counter + 1

    ; Go to the next iteration or exit --------------------------------[  6]----
    dec     AH                      ; 1   Decrement iteration counter and
    brne    iteration_loop          ; 1|2 go to next interation
    sts     n_shifter + 0, BL       ; 1   Save noise shifter into SRAM
    sts     n_shifter + 1, BH       ; 1   Noise shifter high byte
    sts     e_counter + 0, YL       ; 1   Save envelope counter into SRAM
    sts     e_counter + 1, YH       ; 1   Envelope counter high byte
.endmacro                           ; MAX CYCLES: 9 + FDIV * (16 + 20) + 6 = 303

.macro sample_generator
    ; AL is envelope sample
    ; AH is mixer output
    ; BL is amplitude table offset
    ; @0 is volume register
    ; @1 is channel bit
    ; @2 is output sample
    mov     @2, AL                  ; 1   Use envelope amplitude by default
    lds     ZL, @0                  ; 1   volume+envelope flag from PSG register
    bst     ZL, bit4                ; 1   Check if envelope enabled in this
    brts    use_envelope            ; 1|2 channel and skip amplitude computation
    add     ZL, BL                  ; 1   Get amplitude value from table using
    ld      @2, Z                   ; 2   volume as index (0x00-0x0F)
use_envelope:
    sbrs    AH, @1                  ; 1|2 If channel disabled in mixer (N and T)
    clr     @2                      ; 1   then set amplitude to zero value
.endmacro                           ; MAX CYCLES: 9

loop:
    ; Check for timer overflow --------------------------------------[   5 ]----
    in      AL, TIFR0               ; 1   Check timer0 overflow flag TOV0
    sbrs    AL, TOV0                ; 1|2 Skip next instruction if TOV0 is set
    rjmp    loop                    ; 2   otherwise jump to the loop beginning
    out     TIFR0, AL               ; 1   Clear timer overflow flag
    clr     AL                      ; 1   Clear temp register to use as zero

    ; Reset envelope generator after shape change -------------------[  12 ]----
    sbrs    flags, EG_RES           ; 1|2 Skip next instruction if no need to
    rjmp    no_envelope_reset       ; 2   reset envelope generator
    cbr     flags, (1 << EG_RES)    ; 1   Clear request for reset envelope
    sts     e_counter + 0, AL       ; 1   Reset envelope counter
    sts     e_counter + 1, AL       ; 1
    lds     e_gen, e_shape          ; 1   Init envelope generator with a new
    lsl     e_gen                   ; 1   shape, index in table is shape * 4
    lsl     e_gen                   ; 1
    ldi     ZL, low(2*envelopes+1)  ; 1   Init envelope step with value from
    add     ZL, e_gen               ; 1   envelope generator table
    ld      e_stp, Z                ; 2
no_envelope_reset:

    ; Update tone generators ----------------------------------------[  72 ]----
    tone_generator a_period, a_counter, 0b00000001 ; max: 24
    tone_generator b_period, b_counter, 0b00000010 ; max: 24
    tone_generator c_period, c_counter, 0b00000100 ; max: 24

    ; Update noise and envelope generators --------------------------[ 303 ]----
    noise_envelope_generator        ; max: 303

    ; Apply enable flags from mixer ---------------------------------[   6 ]----
    lds     AH, mixer               ; 1   Mixer: xxCB.Acba (CBA:Noise, cba:Tone)
    or      AH, flags               ; 1   Apply disables:  xxCB.Acba | xxNN.Ncba
    mov     AL, AH                  ; 1
    lsl     AL                      ; 1   Shift left:      xxCB.Acba > xCBA.cbax
    swap    AL                      ; 1   Swap nibbles:    xCBA.cbax > cbax.xCBA
    and     AH, AL                  ; 1   Output:          xxxx.xcba & xxxx.xCBA

    ; Compute sample for each channel -------------------------------[  32 ]----
    ldi     ZL, low(2*amp_5bit)     ; 1
    add     ZL, e_stp               ; 1
    ld      AL, Z                   ; 2
    ldi     BL, low(2*amp_4bit)     ; 1
    sample_generator a_volume, bit0, XL ; max: 9
    sample_generator b_volume, bit1, BH ; max: 9
    sample_generator c_volume, bit2, XH ; max: 9

    ; Outup samples to compare match registers ----------------------[   7 ]----
    lsr     BH                      ; 1
    add     XL, BH                  ; 1
    add     XH, BH                  ; 1
    out     OCR0AL, XL              ; 1
    out     OCR0BL, XH              ; 1
    rjmp    loop                    ; 2

    ; MAX CYCLES: 5 + 12 + 72 + 303 + 6 + 32 + 7 = 437
    ; AVG CYCLES: 437 / 2 = 218
#endif
; ------------------------------------------------------------------------------
; SRAM
; ------------------------------------------------------------------------------

    .dseg
    .org    SRAM_START ; 0x0040 for ATtiny10

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
config:     .byte 1
unused:     .byte 1

a_counter:  .byte 2
b_counter:  .byte 2
c_counter:  .byte 2
n_shifter:  .byte 2
e_counter:  .byte 2