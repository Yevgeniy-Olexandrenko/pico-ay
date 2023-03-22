.include "tn10def.inc"

; ------------------------------------------------------------------------------
; DEFINES
; ------------------------------------------------------------------------------

    .equ    F_CPU = 8000000
    .equ    BAUD_RATE = 57600
    .equ    BIT_DURATION = (F_CPU / BAUD_RATE)

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


    ; .def    AL      = r16           ; general purpose register for emulation
    ; .def    AH      = r17           ; general purpose register for emulation
    ; .def    BL      = r18           ; general purpose register for emulation
    ; .def    BH      = r19           ; general purpose register for emulation
    ; .def    flags   = r20           ; register holding flags for emulation
    ; .def    e_amp   = r21           ; register holding envelope out amplitude
    ; .def    e_gen   = r22           ; register holding envelope generator flags
    ; .def    e_stp   = r23           ; register holding envelope step counter
    ; .def    n_xor   = r24           ;
    ; .def    ZERO    = r25           ; register holding zero constant
    ; .def    TAMP    = r26           ; register holding amplitude table offset
    ; .def    TENV    = r27           ; register holding envelope table offset


    ; .equ    DECR    = 0x07          ; TODO
    ; .equ    ERESET  = bit7          ;
    ; .equ    EATTACK = bit0          ;
    ; .equ    EINVERT = bit1          ;
    ; .equ    EHOLD   = bit2          ;
    ; .equ    ESTOP   = bit7          ;
    ; .equ    EENABLE = bit4          ;
    ; .equ    NMASK   = 0b00111000    ;
    ; .equ    ABIT    = bit0          ;
    ; .equ    BBIT    = bit1          ;
    ; .equ    CBIT    = bit2          ;

; ------------------------------------------------------------------------------
; FLASH
; ------------------------------------------------------------------------------
    .cseg

    .org    0x0000
    rjmp    main                    ; RESET handler

    .org    INT0addr
    rjmp    int0_isr                ; INT0  handler

; Data arrays ------------------------------------------------------------------
reg_mask:
    .db     0xFF, 0x0F, 0xFF, 0x0F, 0xFF, 0x0F, 0x1F, 0xFF
    .db     0x1F, 0x1F, 0x1F, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF

amplitude:
    .db     0x00, 0x03, 0x04, 0x05, 0x08, 0x0C, 0x10, 0x1B
    .db     0x20, 0x34, 0x4B, 0x5F, 0x7E, 0xA2, 0xCD, 0xFF

envelopes:
    ; bit0 - attack
    ; bit1 - invert on next cycle
    ; bit2 - stop generator on next cycle
    .db     0x07, 0x07, 0x07, 0x07, 0x04, 0x04, 0x04, 0x04
    .db     0x01, 0x07, 0x03, 0x05, 0x00, 0x06, 0x02, 0x04

; ENTRY POINT ------------------------------------------------------------------
main:
    ; Setup stack and access to FLASH ------------------------------------------
    ldi     AL, RAMEND
    out     SPL, AL
    ldi     ZH, high(MAPPED_FLASH_START)

    ; Setup main clock to 8 MHz ------------------------------------------------
    ldi     AL, 0x00
    ldi     AH, 0xD8                ; Write correct signature to Configuration
    out     CCP, AH                 ; Change Protection register and set clock
    out     CLKPSR, AL              ; division factor to 1 for 8 MHz

    ; Setup external interrupt INT0 --------------------------------------------
    cbi     DDRB,  PORTB2           ; Set PORTB2 as input
    sbi     PUEB,  PUEB2            ; Enable pull-up resistor on PORTB2
    cbi     EICRA, ISC00            ; Falling edge of INT0 generates an
    sbi     EICRA, ISC01            ; interrupt request
    sbi     EIMSK, INT0             ; Allow INT0 ISR execution

    ; Setup Timer0 for Fast PWM 8-bit with 0xFF top ----------------------------
    sbi     DDRB, PORTB0            ; Set PORTB0 and PORTB1 as output
    sbi     DDRB, PORTB1            ; for Fast PWM (OC0A and OC0B)
    ldi     AL, 0b10100001          ; Clear OC0A/OC0B on compare match
    out     TCCR0A, AL              ; COM0A1+COM0B1+WGM00 bits set
    ldi     AL, 0b00001001          ; Fast PWM with no prescaling
    out     TCCR0B, AL              ; WGM02+CS00 bits set

    ; Setup everything else ----------------------------------------------------
    ldi     FDIV, 0x07              ;
    sei                             ;
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
    sbr     raddr, bit4             ; Set reg address beyond allowed value to
    rjmp    exit_isr                ; indicate the waiting for allowed value
reg_addr_received:
    mov     raddr, YH               ; Received data is a register address,
    rjmp    exit_isr                ; so save it and exit
reg_data_received:
    ldi     ZL, low(reg_mask)       ; Read register mask from FLASH for
    add     ZL, raddr               ; current register address
    ld      ZL, Z
    and     ZL, YH                  ; Apply mask for received register data
    ldi     YL, low (psg_regs)      ; store register data to the SRAM
    ldi     YH, high(psg_regs)
    add     YL, raddr
    st      Y, ZL
    cpi     raddr, 0x0D             ; Check if register address is an
    brne    exit_isr                ; envelope shape register address
    sbr     flags, EG_RES           ; Set envelope generator reset flag

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
.endmacro                           ; 24  CPU cycles max

.macro noise_envelope_generator
    lds     XL, e_period  + 0       ; 1   Load envelope peiod from SRAM
    lds     XH, e_period  + 1       ; 1   Envelope period high byte
    lds     YL, e_counter + 0       ; 1   Load envelope counter from SRAM
    lds     YH, e_counter + 1       ; 1   Envelope counter high byte
    lds     BL, n_shifter + 0       ; 1   Load noise shifter from SRAM
    lds     BH, n_shifter + 1       ; 1   Noise shifter high byte
    lds     AL, n_period            ; 1   Load noise period from SRAM and double
    lsl     AL                      ; 1   it to simulate noise prescaler
    mov     AH, FDIV                ; 1   Iterate
iteration_loop:
    ; Update noise generator ------------------------------------------[ 17]----
    ; TODO: shifting of the flags is bad idea!
    cp      n_cnt, AL               ; 1   Compare counter against period
    brlo    exit_noise              ; 1|2 Skip following if counter < period
    clr     n_cnt                   ; 1   Reset counter
    mov     ZL, BL                  ; 1   Compute the feedback based on
    lsr     ZL                      ; 1   bit3 xor bit0 of the shifter
    lsr     ZL                      ; 1
    lsr     ZL                      ; 1
    eor     ZL, BL                  ; 1
    rol     flags                   ; 1   Shift 17-bit shifter, bit16 is located
    ror     BH                      ; 1   in the bit7 of the flags regster
    ror     BL                      ; 1
    ror     ZL                      ; 1   Store feedback as bit16 of the shifter
    ror     flags                   ; 1   in bit7 of the flags register
    cbr     flags, 0b00111000       ; 1   Set noise output flags according to
    sbrc    BL, bit0                ; 1|2 bit0 of the current shifter state
    sbr     flags, 0b00111000       ; 1
exit_noise:
    inc     n_cnt                   ; 1   counter = counter + 1

    ; Update envelope generator ------------------------------------------------
    ; TODO

    dec     AH                      ; 1   Decrement iteration counter and
    brne    iteration_loop          ; 1|2 go to next interation
    sts     n_shifter + 0, BL       ; 1   Save noise shifter into SRAM
    sts     n_shifter + 1, BH       ; 1   Noise shifter high byte
    sts     e_counter + 0, YL       ; 1   Save envelope counter into SRAM
    sts     e_counter + 1, YH       ; 1   Envelope counter high byte
.endmacro

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
.endmacro                           ; 9   CPU cycles max

loop:
    ; Check for timer overflow --------------------------------------[   3 ]----
    in      AL, TIFR0               ; 1   Check timer0 overflow flag TOV0
    sbrs    AL, TOV0                ; 1|2 Skip next instruction if TOV0 is set
    rjmp    loop                    ; 2   otherwise jump to the loop beginning
    out     TIFR0, AL               ; 1   Clear timer overflow flag

    ; Update tone generators ----------------------------------------[  73 ]----
    clr     AL                      ; 1
    tone_generator a_period, a_counter, 0b00000001 ; 24
    tone_generator b_period, b_counter, 0b00000010 ; 24
    tone_generator c_period, c_counter, 0b00000100 ; 24

    ; Update noise and envelope generators --------------------------[   0 ]----
    noise_envelope_generator

    ; Apply enable flags from mixer ---------------------------------[   6 ]----
    lds     AH, mixer               ; 1   Mixer: xxCB.Acba (CBA:Noise, cba:Tone)
    or      AH, flags               ; 1   Apply disables:  xxCB.Acba | xxNN.Ncba
    mov     AL, AH                  ; 1
    lsl     AL                      ; 1   Shift left:      xxCB.Acba > xCBA.cbax
    swap    AL                      ; 1   Swap nibbles:    xCBA.cbax > cbax.xCBA
    and     AH, AL                  ; 1   Output:          xxxx.xcba & xxxx.xCBA

    ; Compute envelope sample ---------------------------------------[   4 ]----
    ldi     ZL, low(amplitude)      ; 1
    add     ZL, e_stp               ; 1
    ld      AL, Z                   ; 2

    ; Compute sample for each channel -------------------------------[  28 ]----
    ldi     BL, low(amplitude)      ; 1
    sample_generator a_volume, bit0, XL ; 9
    sample_generator b_volume, bit1, BH ; 9
    sample_generator c_volume, bit2, XH ; 9

    ; Outup samples to compare match registers ----------------------[   7 ]----
    lsr     BH                      ; 1
    add     XL, BH                  ; 1
    add     XH, BH                  ; 1
    out     OCR0AL, XL              ; 1
    out     OCR0BL, XH              ; 1
    rjmp    loop                    ; 2

;     ; update envelope generator and compute level ------------------------------
;     lds     AL, e_counter + 0       ; load counter from SRAM
;     lds     AH, e_counter + 1       ;
;     sbrs    flags, ERESET           ; if envelope shape is unchanged then skip
;     rjmp    e_not_changed           ; envelope generator initialization
;     cbr     flags, (1 << ERESET)    ; clear envelope shape changed flag
;     lds     ZL, e_shape             ; initialize envelope genertor with shape
;     add     ZL, TENV                ; get envelope generation code from table
;     ld      e_gen, Z                ; using envelope shape as index (0x00-0x0F)
;     ldi     e_stp, 0x1F             ; reload step counter with top value
;     rjmp    e_reload                ; continue with period counter reload
; e_not_changed:
;     sbrc    e_gen, ESTOP            ; check if envelope geneator is stopped
;     rjmp    e_no_reload             ;
;     subi    AL, DECR                ; decrement by sampling constant
;     sbc     AH, ZERO                ;
;     brcs    e_next_step             ; if counter < 0 then go to next step
;     brne    e_no_reload             ; if counter > 0 then skip reload section
; e_next_step:
;     dec     e_stp                   ; decrement step counter
;     brpl    e_reload                ; if counter >= 0 then skip step reloading
;     ldi     e_stp, 0x1F             ; reload step counter with top value
;     ldi     BL, (1 << EATTACK)      ;
;     sbrc    e_gen, EINVERT          ; if 'invert on next cycle' bit is active
;     eor     e_gen, BL               ; then invert envelope 'attack' bit
;     sbrc    e_gen, EHOLD            ; if 'stop on next cycle' bit is active
;     sbr     e_gen, (1 << ESTOP)     ; then stop envelope generator
; e_reload:
;     lds     BL, e_period + 0        ; as counter <= 0 it reloads with
;     lds     BH, e_period + 1        ; new period from PSG registers
;     add     AL, BL                  ; add period to the counter to pre-save the
;     adc     AH, BH                  ; sampling fraction left after substraction
;     mov     ZL, e_stp               ;
;     ldi     BL, 0x1F                ;
;     sbrs    e_gen, EATTACK          ; invert envelope volume if 'attack' bit 
;     eor     ZL, BL                  ; is not set in envelope generator
;     lsr     ZL                      ; convert 5-bit volume to 4-bit one
;     add     ZL, TAMP                ; get amplitude value from table using
;     ld      e_amp, Z                ; envelope volume as index (0x00-0x0F)
; e_no_reload:
;     sts     e_counter + 0, AL       ; save counter back to SRAM
;     sts     e_counter + 1, AH       ;

;     ; update noise generator and toggle level ----------------------------------
;     lds     AL, n_counter           ; load counter from SRAM
;     subi    AL, DECR                ;
;     breq    n_reload                ; if counter = 0 then go to reload section
;     brpl    n_no_reload             ; if counter > 0 then skip reload section
; n_reload:
;     lds     BL, n_period            ; as counter <= 0 it reloads with new
;     add     AL, BL                  ; period from PSG register
;     lds     BL, n_shifter + 0       ; load shifter from SRAM
;     lds     BH, n_shifter + 1       ;
;     lsr     n_xor                   ;
;     mov     n_xor, BL               ;
;     ror     BH                      ; rotate 16-bit shifter
;     ror     BL                      ;
;     sbr     flags, NMASK            ; set noise level high
;     sbrs    BL, bit0                ; check bit 0 of the shifter
;     cbr     flags, NMASK            ; set noise level low
;     lsl     n_xor                   ;
;     eor     n_xor, BL               ;
;     lsr     n_xor                   ;
;     sts     n_shifter + 0, BL       ; save shifter back to SRAM
;     sts     n_shifter + 1, BH       ;
; n_no_reload:
;     sts     n_counter, AL           ; save counter back to SRAM

;     ; update tone A counter and toggle level -----------------------------------
;     lds     AL, a_counter + 0       ; load counter from SRAM
;     lds     AH, a_counter + 1       ; as 16-bit signed value
;     subi    AL, DECR                ; decrement by sampling constant
;     sbc     AH, ZERO                ;
;     breq    a_reload                ; if counter = 0 then go to reload section
;     brpl    a_no_reload             ; if counter > 0 then skip reload section
; a_reload:
;     lds     BL, a_period + 0        ; as counter <= 0 it reloads with
;     lds     BH, a_period + 1        ; new period from PSG registers
;     add     AL, BL                  ; add period to the counter to pre-save the
;     adc     AH, BH                  ; sampling fraction left after substraction
;     ldi     BL, (1 << ABIT)         ;
;     eor     flags, BL               ; toggle tone level
; a_no_reload:
;     sts     a_counter + 0, AL       ; save counter back to SRAM
;     sts     a_counter + 1, AH       ;

;     ; update tone B counter and toggle level -----------------------------------
;     lds     AL, b_counter + 0       ; load counter from SRAM
;     lds     AH, b_counter + 1       ; as 16-bit signed value
;     subi    AL, DECR                ; decrement by sampling constant
;     sbc     AH, ZERO                ;
;     breq    b_reload                ; if counter = 0 then go to reload section
;     brpl    b_no_reload             ; if counter > 0 then skip reload section
; b_reload:
;     lds     BL, b_period + 0        ; as counter <= 0 it reloads with
;     lds     BH, b_period + 1        ; new period from PSG registers
;     add     AL, BL                  ; add period to the counter to pre-save the
;     adc     AH, BH                  ; sampling fraction left after substraction
;     ldi     BL, (1 << BBIT)         ;
;     eor     flags, BL               ; toggle tone level
; b_no_reload:
;     sts     b_counter + 0, AL       ; save counter back to SRAM
;     sts     b_counter + 1, AH       ;

;     ; update tone C counter and toggle level -----------------------------------
;     lds     AL, c_counter + 0       ; load counter from SRAM
;     lds     AH, c_counter + 1       ; as 16-bit signed value
;     subi    AL, DECR                ; decrement by sampling constant
;     sbc     AH, ZERO                ;
;     breq    c_reload                ; if counter = 0 then go to reload section
;     brpl    c_no_reload             ; if counter > 0 then skip reload section
; c_reload:
;     lds     BL, c_period + 0        ; as counter <= 0 it reloads with
;     lds     BH, c_period + 1        ; new period from PSG registers
;     add     AL, BL                  ; add period to the counter to pre-save the
;     adc     AH, BH                  ; sampling fraction left after substraction
;     ldi     BL, (1 << CBIT)         ;
;     eor     flags, BL               ; toggle tone level
; c_no_reload:
;     sts     c_counter + 0, AL       ; save counter back to SRAM
;     sts     c_counter + 1, AH       ;

;     ; apply enable flags from mixer --------------------------------------------
;     ; output = (MixerN | LevelN) & (MixerT | LevelT)
;     lds     AH, mixer               ; mixer: xxCBAcba where CBA:Noise, cba:Tone
;     or      AH, flags               ; apply disable mask: xxCBAcba | xxNNNcba
;     mov     AL, AH                  ;
;     lsl     AL                      ; shift left:   xxCB|Acba -> xCBA|cbax
;     swap    AL                      ; swap nibbles: xCBA|cbax -> cbax|xCBA
;     and     AH, AL                  ; output: xxxx|xcba & xxxx|

;     ; compute channel B amplitude ----------------------------------------------
;     lds     ZL, b_volume            ; volume and envelope flag from PSG register
;     mov     BL, e_amp               ; use envelope amplitude by default
;     bst     ZL, EENABLE             ; check if envelope enabled in this
;     brts    b_with_envelope         ; channel and skip amplitude computation
;     add     ZL, TAMP                ; get amplitude value from table using
;     ld      BL, Z                   ; volume as index (0x00-0x0F)
; b_with_envelope:
;     sbrs    AH, BBIT                ; if channel disabled in mixer (N and T)
;     clr     BL                      ; then set amplitude to zero value
;     lsr     BL                      ; channel B is at half amplitude

;     ; compute channel A amplitude ----------------------------------------------
;     lds     ZL, a_volume            ; volume and envelope flag from PSG register
;     mov     BH, e_amp               ; use envelope amplitude by default
;     bst     ZL, EENABLE             ; check if envelope enabled in this
;     brts    a_with_envelope         ; channel and skip amplitude computation
;     add     ZL, TAMP                ; get amplitude value from table using
;     ld      BH, Z                   ; volume as index (0x00-0x0F)
; a_with_envelope:
;     sbrs    AH, ABIT                ; if channel disabled in mixer (N and T)
;     clr     BH                      ; then set amplitude to zero value
;     add     BH, BL                  ; left = amplitudeA + 0.5 * amplitudeB
;     out     OCR0AL, BH              ; send result to PWM compare match

;     ; compute channel C amplitude ----------------------------------------------
;     lds     ZL, c_volume            ; volume and envelope flag from PSG register
;     mov     BH, e_amp               ; use envelope amplitude by default
;     bst     ZL, EENABLE             ; check if envelope enabled in this
;     brts    c_with_envelope         ; channel and skip amplitude computation
;     add     ZL, TAMP                ; get amplitude value from table using
;     ld      BH, Z                   ; volume as index (0x00-0x0F)
; c_with_envelope:
;     sbrs    AH, CBIT                ; if channel disabled in mixer (N and T)
;     clr     BH                      ; then set amplitude to zero value
;     add     BH, BL                  ; left = amplitudeA + 0.5 * amplitudeB
;     out     OCR0BL, BH              ; send result to PWM compare match

; ------------------------------------------------------------------------------
; SRAM
; ------------------------------------------------------------------------------

    .dseg
    .org SRAM_START ; 0x0040 for ATtiny10

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
