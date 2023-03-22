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
    .equ    ERESET = bit6

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

amplitudes:
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
    ; setup stack and assess to flash ------------------------------------------
    ldi     AL, RAMEND              ; load RAMEND into r16
    out     SPL, AL                 ; store r16 in stack pointer
    ldi     ZH, high(MAPPED_FLASH_START)

    ; setup main clock to 8 MHz ------------------------------------------------
    ldi     AL, 0x00
    ldi     AH, 0xD8                ; write correct signature
    out     CCP, AH                 ; to Configuration Change Protection register
    out     CLKPSR, AL              ; and set clock division factor to 1 for 8 MHz

    ; setup external interrupt INT0 --------------------------------------------
    cbi     DDRB,  PORTB2           ; set PORTB2 as input
    sbi     PUEB,  PUEB2            ; enable pull-up resistor on PORTB2
    cbi     EICRA, ISC00            ; falling edge of INT0 generates an
    sbi     EICRA, ISC01            ; interrupt request
    sbi     EIMSK, INT0             ; allow INT0 ISR execution

    ; setup Timer0 for Fast PWM 8-bit with 0xFF top ----------------------------
    sbi     DDRB, PORTB0            ; set PORTB0 and PORTB1 as output
    sbi     DDRB, PORTB1            ; for Fast PWM (OC0A and OC0B)
    ldi     AL, 0b10100001          ; Clear OC0A/OC0B on compare match
    out     TCCR0A, AL              ; COM0A1+COM0B1+WGM00 bits set
    ldi     AL, 0b00001001          ; Fast PWM with no prescaling
    out     TCCR0B, AL              ; WGM02+CS00 bits set

    ; setup everything else ----------------------------------------------------
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
    ldi     YL, @0                  ; 1 one iteration delays for 3 cpu cycles
delay_loop:
    dec     YL                      ; 1 last iteration delays for 2 cpu cycles
    brne    delay_loop              ; 1|2 but we take into account loop init
.endmacro

int0_isr:
    push    YL                      ; 2 delay loop counter
    push    YH                      ; 2 data bits shift register
    push    ZL                      ; 2 data bits loop counter
    in      ZL, SREG                ; 1 this ISR needs for 4 + 2 bytes of SRAM
    push    ZL                      ; 2 to save registers and return address

    ; waiting for the middle of the start bit ----------------------------------
    uart_delay STR_BIT_DELAY

    ; read data bits
    ldi     ZL, 8                   ; 1 init data bits loop counter
    clr     YH                      ; 1 clear data bits shift register
    nop                             ; 1 for better delaying
bit_read_loop:
    uart_delay DAT_BIT_DELAY
    lsr     YH                      ; 1 shift bit register to right
    sbic    PINB, PORTB2            ; 1|2 skip next instruction if RX is clear
    ori     YH, 0x80                ; 1 set data bit 7 if RX is set
    dec     ZL                      ; 1 go to the next bit
    brne    bit_read_loop           ; 1|2
    nop                             ; 1 for better delaying

    ; check if the received stop bit is correct --------------------------------
    uart_delay STP_BIT_DELAY
    sbis    PINB, PORTB2            ; skip next instruction if RX is set, it
    rjmp    exit_isr                ; means the stop bit is correct

    ; interpret the received byte according to the protocol --------------------
    sbrs    raddr, bit4             ; skip next instruction if waiting for 
    rjmp    reg_data_received       ; incoming register address
    cpi     YH, 0xF0                ; check if received byte is a valid
    brlo    reg_addr_received       ; register addres, otherwise try to sync
    ldi     raddr, (1 << bit4)      ; set reg address beyond allowed value to
    rjmp    exit_isr                ; indicate the waiting for allowed value
reg_addr_received:
    mov     raddr, YH               ; received data is a register address,
    rjmp    exit_isr                ; so save it in variable and exit
reg_data_received:
    ldi     ZL, low(reg_mask)       ; read register mask from FLASH for  
    add     ZL, raddr               ; current register address
    ld      ZL, Z
    and     ZL, YH                  ; apply mask for received register data
    ldi     YL, low(psg_regs)       ; store register data to the SRAM
    ldi     YH, high(psg_regs)
    add     YL, raddr
    st      Y, ZL
    cpi     raddr, 0x0D             ; check if register address is an
    brne    exit_isr                ; envelope shape register address
    sbr     flags, ERESET           ; set envelope generator reload flag

    ; exit interrupt service routine -------------------------------------------
exit_isr:
    pop     ZL
    out     SREG, ZL
    pop     ZL
    pop     YH
    pop     YL
    reti

; MAIN LOOP --------------------------------------------------------------------
loop:
    in      AL, TIFR0               ; check timer0 overflow flag TOV0
    sbrs    AL, TOV0                ; skip next instruction if TOV0 is set
    rjmp    loop                    ; otherwise jump to the loop beginning
    out     TIFR0, AL               ; clear timer overflow flag

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

    ; all work is done, so go to next loop -------------------------------------
    rjmp loop

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
cfg_0:      .byte 1
cfg_1:      .byte 1

a_counter:  .byte 2
b_counter:  .byte 2
c_counter:  .byte 2
n_shifter:  .byte 2
e_counter:  .byte 2
