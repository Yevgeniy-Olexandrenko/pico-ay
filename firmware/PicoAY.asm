; Features to implement:
; - Internal clock calibration using UART signal as source of sync
; - Auto mute chip using WDT when there is no UART communication
; - Disable all unused hardware to reduce power consumption
; - PSG chip selection using dedicated pin (high - Chip #0, low - Chip #1)
; + PSG stereo mode selection using dedicated pin (high - ABC, low - ACB)

; ==============================================================================
; GLOBAL DEFINES
; ==============================================================================

   .equ     BAUD_RATE = 57600
   .equ     SAMP_RATE = int(0.5 + (F_CPU / 1.0 / S_CYCLES))
   .equ     U_STEP    = int(0.5 + (F_PSG / 8.0 / SAMP_RATE))
   .if      (U_STEP < 1 || U_STEP > 10)
   .error   "Update step is out of range"
   .endif

   .def     ZERO    = r16           ; Constant holding zero value
   .def     raddr   = r17           ; PSG register address and addr/data latch
   .def     flags   = r18           ; Tone/Nose flip-flops and additional flags
   .def     n_cnt   = r19           ; Noise generator period counter
   .def     e_stp   = r20           ; Envelope generator step counter
   .def     e_gen   = r21           ; Envelope generation config pointer
   .def     AL      = r22           ; General purpose usage
   .def     AH      = r23           ; General purpose usage
   .def     BL      = r24           ; 16-bit value #0 low byte or general usage
   .def     BH      = r25           ; 16-bit value #0 high byte or general usage
;  .def     XL      = r26           ; 16-bit value #1 low byte or general usage
;  .def     XH      = r27           ; 16-bit value #1 high byte or general usage
;  .def     YL      = r28           ; 16-bit value #2 low byte or general usage
;  .def     YH      = r29           ; 16-bit value #2 high byte or general usage
;  .def     ZL      = r30           ; Flash memory offset or general usage
;  .def     ZH      = r31           ; Flash memory first 256 bytes pointer

   .equ     bit0    = 0
   .equ     bit1    = 1
   .equ     bit2    = 2
   .equ     bit3    = 3
   .equ     bit4    = 4
   .equ     bit5    = 5
   .equ     bit6    = 6
   .equ     bit7    = 7

   .equ     AFFBIT  = bit0          ; Tone A flip-flop bit
   .equ     BFFBIT  = bit1          ; Tone A flip-flop bit
   .equ     CFFBIT  = bit2          ; Tone A flip-flop bit
   .equ     NFFMSK  = 0b00_111_000  ; Noise flip-flop mask
   .equ     NS_B16  = bit7          ; Noise shifter bit16
   .equ     EG_RES  = bit6          ; Envelope generator reset
   .equ     WF_REG  = bit4          ; Waiting for register address

    #define M(bit)  (1 << bit)      ; Get mask from bit number
    #define L(addr) (addr + 0)      ; Gel low byte address
    #define H(addr) (addr + 1)      ; Get high byte address
    #define P(addr) (2 * addr)      ; Get address in flash

   .listmac

; ==============================================================================
; DEBUG PROBES
; ==============================================================================

#define DEF_PROBE(name, port, bit)              \
   .equ     probe_##name##_pi = PIN##port       \
   .equ     probe_##name##_po = PORT##port      \
   .equ     probe_##name##_pb = PORT##port##bit \
    sbi     PORT##port, PORT##port##bit         \
    sbi     DDR##port,  PORT##port##bit

.macro __set_probe
   .if      (defined(@0) && defined(@1))
    sbi     @0, @1
   .endif
.endmacro

.macro __clr_probe
   .if      (defined(@0) && defined(@1))
    cbi     @0, @1
   .endif
.endmacro

#define SET_PROBE(name) \
__set_probe probe_##name##_po, probe_##name##_pb

#define CLR_PROBE(name) \
__clr_probe probe_##name##_po, probe_##name##_pb

#define TGL_PROBE(name) \
__set_probe probe_##name##_pi, probe_##name##_pb

; ==============================================================================
; DATA BLOCKS
; ==============================================================================

; VALUE MASK FOR EACH PSG REGISTER ---------------------------------------------
.macro __reg_mask
    ; 16 bytes
   .db      0xFF, 0x0F, 0xFF, 0x0F, 0xFF, 0x0F, 0x1F, 0x3F
   .db      0x1F, 0x1F, 0x1F, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF
.endmacro
#define data_reg_mask() \
reg_mask: __reg_mask

; 4-BIT VOLUME TO AMPLITUDE ----------------------------------------------------
.macro __amp_4bit
    ; @0 maximum amplitude
    ; 16 bytes
   .if      (@0 < 0x00 || @0 > 0xFF)
   .error   "Maximum amplitude is out of range"
   .endif
   .db      int(0.5 + @0 * 0.00000000000000), int(0.5 + @0 * 0.00772106507973)
   .db      int(0.5 + @0 * 0.01396200503550), int(0.5 + @0 * 0.02001983672850)
   .db      int(0.5 + @0 * 0.02969405661100), int(0.5 + @0 * 0.04039063096060)
   .db      int(0.5 + @0 * 0.05833524071110), int(0.5 + @0 * 0.07777523460750)
   .db      int(0.5 + @0 * 0.11108567940800), int(0.5 + @0 * 0.14848554207700)
   .db      int(0.5 + @0 * 0.21155107957600), int(0.5 + @0 * 0.28110170138100)
   .db      int(0.5 + @0 * 0.40042725261300), int(0.5 + @0 * 0.53443198291000)
   .db      int(0.5 + @0 * 0.75800717174000), int(0.5 + @0 * 1.00000000000000)
.endmacro
#define data_amp_4bit(MAX_AMP) \
amp_4bit: __amp_4bit MAX_AMP

; 5-BIT VOLUME TO AMPLITUDE ----------------------------------------------------
.macro __amp_5bit
    ; @0 maximum amplitude
    ; 32 bytes
   .if      (@0 < 0x00 || @0 > 0xFF)
   .error   "Maximum amplitude is out of range"
   .endif
   .db      int(0.5 + @0 * 0.00000000000000), int(0.5 + @0 * 0.00000000000000)
   .db      int(0.5 + @0 * 0.00465400167849), int(0.5 + @0 * 0.00772106507973)
   .db      int(0.5 + @0 * 0.01095597772180), int(0.5 + @0 * 0.01396200503550)
   .db      int(0.5 + @0 * 0.01699855039290), int(0.5 + @0 * 0.02001983672850)
   .db      int(0.5 + @0 * 0.02436865796900), int(0.5 + @0 * 0.02969405661100)
   .db      int(0.5 + @0 * 0.03506523231860), int(0.5 + @0 * 0.04039063096060)
   .db      int(0.5 + @0 * 0.04853894865340), int(0.5 + @0 * 0.05833524071110)
   .db      int(0.5 + @0 * 0.06805523765930), int(0.5 + @0 * 0.07777523460750)
   .db      int(0.5 + @0 * 0.09251544975970), int(0.5 + @0 * 0.11108567940800)
   .db      int(0.5 + @0 * 0.12974746318800), int(0.5 + @0 * 0.14848554207700)
   .db      int(0.5 + @0 * 0.17666895552000), int(0.5 + @0 * 0.21155107957600)
   .db      int(0.5 + @0 * 0.24638742656600), int(0.5 + @0 * 0.28110170138100)
   .db      int(0.5 + @0 * 0.33373006790300), int(0.5 + @0 * 0.40042725261300)
   .db      int(0.5 + @0 * 0.46738384069600), int(0.5 + @0 * 0.53443198291000)
   .db      int(0.5 + @0 * 0.63517204547200), int(0.5 + @0 * 0.75800717174000)
   .db      int(0.5 + @0 * 0.87992675669500), int(0.5 + @0 * 1.00000000000000)
.endmacro
#define data_amp_5bit(MAX_AMP) \
amp_5bit: __amp_5bit MAX_AMP

; ENVELOPE GENERATION INSTRUCTIONS ---------------------------------------------
.macro __envelopes
    ; @0 number of steps in waveform
    ; 64 bytes
   .if      (@0 != 16 && @0 != 32)
   .error   "Unknown number of steps for envelope"
   .endif
   .equ     _inc =  1
   .equ     _dec = -1
   .equ     _hld =  0
   .equ     _top = @0-1
   .equ     _bot = 0x00
   .db      _dec, _top, _hld, _bot  ; 0
   .db      _dec, _top, _hld, _bot  ; 1
   .db      _dec, _top, _hld, _bot  ; 2
   .db      _dec, _top, _hld, _bot  ; 3
   .db      _inc, _bot, _hld, _bot  ; 4
   .db      _inc, _bot, _hld, _bot  ; 5
   .db      _inc, _bot, _hld, _bot  ; 6
   .db      _inc, _bot, _hld, _bot  ; 7
   .db      _dec, _top, _dec, _top  ; 8
   .db      _dec, _top, _hld, _bot  ; 9
   .db      _dec, _top, _inc, _bot  ; A
   .db      _dec, _top, _hld, _top  ; B
   .db      _inc, _bot, _inc, _bot  ; C
   .db      _inc, _bot, _hld, _top  ; D
   .db      _inc, _bot, _dec, _top  ; E
   .db      _inc, _bot, _hld, _bot  ; F
.endmacro
#define data_envelopes(STEPS) \
envelopes: __envelopes STEPS

; ==============================================================================
; CUSTOM INSTRUCTIONS
; ==============================================================================

; LOAD DATA FROM IO REGISTER ---------------------------------------------------
.macro ldio
    ; @0 destination CPU register
    ; @1 source I/O register
    ; AVR8L_0: 1
    ; V2/V2E:  2-1
   .if      (@1 < 0x40)
    in      @0, @1                  ; 1   I/O register
   .elif    (@1 >= 0x60 && @1 < SRAM_START)
    lds     @0, @1                  ; 1~2 Memory mapped register
   .else
   .error   "Invalid I/O register address"
   .endif
.endmacro

; STORE DATA TO IO REGISTER ----------------------------------------------------
.macro stio
    ; @0 destination I/O register
    ; @1 source CPU register
    ; AVR8L_0: 1
    ; V2/V2E:  2-1
   .if      (@0 < 0x40)
    out     @0, @1                  ; 1   I/O register
   .elif    (@0 >= 0x60 && @0 < SRAM_START)
    sts     @0, @1                  ; 1~2 Memory mapped register
   .else
   .error   "Invalid I/O register address"
   .endif
.endmacro

; LOAD DATA FROM FLASH USING TABLE BASE AND DISPLACEMENT -----------------------
.macro ldp
    ; ZL table base or displasement
    ; @0 destination register
    ; @1 displacement or table base
    ; AVR8L_0: 3
    ; V2/V2E:  4
#if __CORE_AVR8L_0__
    add     ZL, @1                  ; 1   Add table base with displacement
    ld      @0, Z                   ; 2   Load indirrect from Z
#elif __CORE_V2__ || __CORE_V2E__
    add     ZL, @1                  ; 1   Add table base with displacement
    lpm     @0, Z                   ; 3   Load indirrect from Z
#else
   .error   "Unknown AVR core version"
#endif
.endmacro

; ==============================================================================
; CODE BLOCKS
; ==============================================================================

; CLEAR SRAM VARIABLES AND SET STACK POINTER -----------------------------------
.macro __setup_sram
    clr     ZERO                    ;     Always zero value used across the code
    ldi     ZL, low(psg_regs)       ;     Setup start address in SRAM
    ldi     ZH, high(psg_regs)      ;
    ldi     AL, psg_end-psg_regs    ;     SRAM size to be cleared
sram_clear_loop:                    ;
    st      Z+, ZERO                ;
    dec     AL                      ;
    brne    sram_clear_loop         ;
    ldi     AL, low(RAMEND)         ;
    stio    SPL, AL                 ;
   .if      defined(SPH)
    ldi     AL, high(RAMEND)        ;
    stio    SPH, AL                 ;
   .endif
.endmacro
#define code_setup_sram() \
__setup_sram

; SET Z POINTER TO FIRST 256 BYTES OF FLASH TO ASSESS DATA ---------------------
.macro __setup_data_access
   .if      defined(MAPPED_FLASH_START)
    ldi     ZH, high(MAPPED_FLASH_START)
   .else
    ldi     ZH, 0x00
   .endif
.endmacro
#define code_setup_data_access() \
__setup_data_access

; SETUP PORT PIN AS INPUT WITH PULL-UP -----------------------------------------
.macro __setup_input_pullup
    ; @0 DDRx
    ; @1 PORTx
    ; @2 PORTxn
    ; @3 PUEx
    ; @4 PUExn
    cbi     @0, @2                  ;     Set port as input
   .if      defined(@3)
    sbi     @3, @4                  ;     Enable pull-up resistor using PUEx
   .else
    sbi     @1, @2                  ;     Enable pull-up resistor using PORTx
   .endif
.endmacro
#define code_setup_input_pullup(P, B) \
__setup_input_pullup DDR##P, PORT##P, PORT##P##B, PUE##P, PUE##P##B

; SETUP EVERYTHING ELSE AND START GENERATION -----------------------------------
.macro __setup_and_start_generation
    ldi     flags, M(NS_B16) | M(EG_RES)
    ldi     raddr, M(WF_REG)        ;     Wait the register address to write
    mov     XL, ZERO                ;     Load zero to left output sample
    mov     XH, ZERO                ;     Load zero to right output sample
    sei                             ;     Enable interrupts
    rjmp    loop                    ;     Go to main loop
.endmacro
#define code_setup_and_start_generation() \
__setup_and_start_generation

; INTERNAL OSCILLATOR CALIBRATION USING UART SIGNAL ----------------------------
.macro __osccal_t16_pcint_isr
    ; @0 port name
    ; @1 port bit number
    ; @2 timer number
    ; @3 CSn0 where n is a timer number
    ; AL changed, general usage
    ; AH measurement counter
    ; XL bit duration in cpu cycles (low byte)
    ; XH bit duration in cpu cycles (high byte)
    sbic    PIN@0, PORT@0@1         ; Check pin level (low/high)
    rjmp    high_level_triggered    ; Skip if clear (low level)
    stio    TCNT@2H, ZERO           ; Reset timer counter
    stio    TCNT@2L, ZERO           ;
    ldi     AL, M(@3)               ; Start timer counting
    stio    TCCR@2B, AL             ;
    rjmp    isr_exit                ; Exit ISR
high_level_triggered:
    stio    TCCR@2B, ZERO           ; Stop timer counting
    tst     AH                      ; Checks if measurement has been
    breq    isr_exit                ; completed
    push    BL                      ; Save BL/BH
    push    BH                      ;
    ldio    BL, TCNT@2L             ; Get timer counter value 
    ldio    BH, TCNT@2H             ;
    cp      XL, BL                  ; Compare with lowest measured value
    cpc     XH, BH                  ;
    brlo    high_level_exit         ; XL/XH < BL/BH
    mov     XL, BL                  ; Update with new lowest measured value
    mov     XH, BH                  ;
high_level_exit:
    TGL_PROBE(CAL_MEASUREMENT)
    dec     AH                      ; Decrement measurement counter
    pop     BH                      ; Restore BL/BH
    pop     BL                      ;
isr_exit:
    reti                            ; Exit ISR
.endmacro
#define proc_osccal_t16_pcint_isr(__P, __B, __T) \
osccal_t16_pcint_isr: __osccal_t16_pcint_isr __P, __B, __T, CS##__T##0

.macro __osccal_uart_bit_duration
    ; @0 port name
    ; @1 port bit number
    ; AH measurement counter
    ; XL bit duration in cpu cycles (low byte)
    ; XH bit duration in cpu cycles (high byte)
    ldi     XL, 0xFF                ; Set lowest value as top of
    ldi     XH, 0xFF                ; 16-bit value
    ldi     AH, 16                  ; Number of bit duration measurements
wait_for_high_level:
    sbis    PIN@0, PORT@0@1         ; Wait for pin level goes high then
    rjmp    wait_for_high_level     ; start new measurement
    sei                             ; Enable interrupts
wait_for_measurement:
    tst     AH                      ; Check if measurement has been
    brne    wait_for_measurement    ; completed
    cli                             ; Disable interrupts
    ret                             ; Return
.endmacro
#define proc_osccal_uart_bit_duration(__P, __B) \
osccal_uart_bit_duration: __osccal_uart_bit_duration __P, __B

.macro __calibrate_internal_oscillator
    ; @0 pin change mask register
    ; @1 pin change enable bit number for mask 
    ; @2 pin change interrupt enable register
    ; @3 pin change interrupt enable flag 
    CLR_PROBE(CAL_START_STOP)
    ldi     AL, M(PCINT@1)          ;
    stio    @0, AL                  ;
    ldi     AL, M(@3)               ;
    stio    @2, AL                  ;

    ; Binary search for coarse calibration
    ldi     YL, 128                 ;
    ldi     YH, 0                   ;
binary_search_loop:
    mov     AL, YH                  ;
    add     AL, YL                  ;
    stio    OSCCAL, AL              ;
    TGL_PROBE(CAL_WRITE_OSCCAL)
    rcall   osccal_uart_bit_duration
    subi    XL, low (BIT_DURATION)  ;
    sbci    XH, high(BIT_DURATION)  ;
    brsh    osc_to_high             ;
    add     YH, YL                  ;
osc_to_high:
    lsr     YL                      ;
    brne    binary_search_loop      ;

    ; Linear search for neighborhood values
    ldi     BL, 0xFF                ;
    ldi     BH, 0xFF                ;
    dec     YH                      ; From: calibration-1
    ldi     YL, 3                   ; To:   calibration+1
linaer_search_loop:
    stio    OSCCAL, YH              ;
    rcall   osccal_uart_bit_duration
    subi    XL, low (BIT_DURATION)  ;
    sbci    XH, high(BIT_DURATION)  ;
    brsh    delta_is_positive       ;
    clr     AL                      ;
    clr     AH                      ;
    sub     AL, XL                  ;
    sbc     AH, XH                  ;
    mov     XL, AL                  ;
    mov     XH, AH                  ;
delta_is_positive:
    cp      XL, BL                  ;
    cpc     XH, BH                  ;
    brsh    delta_is_larger         ;
    mov     BL, XL                  ;
    mov     BH, XH                  ;
    mov     ZL, YH                  ;
delta_is_larger:
    inc     YH                      ; 
    dec     YL                      ;
    brne    linaer_search_loop      ;
    stio    OSCCAL, ZL              ;
    stio    @2, ZERO                ;
    stio    @0, ZERO                ;
    SET_PROBE(CAL_START_STOP)
.endmacro
#define code_calibrate_internal_oscillator(__MR, __MB, __IR, __IF) \
__calibrate_internal_oscillator __MR, __MB, __IR, __IF

; HANDLE UART RECEIVED DATA ACCORDING TO PROTOCOL ------------------------------
.macro __handle_uart_data
    ; YL must be saved outside this code
    ; YH received data byte
    sbrs    raddr, WF_REG           ;     Skip next instruction if waiting for 
    rjmp    reg_data_received       ;     incoming register address
    cpi     YH, 0x10-2              ;     Check if received byte is a valid
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

; SOFTWARE UART DATA RECEIVE ---------------------------------------------------
   .equ     BIT_DURATION = (F_CPU / BAUD_RATE)
   .equ     ADC_DURATION = (F_CPU * 13 / 1000000)
   .equ     BIT_EX_DELAY = ((BIT_DURATION - ADC_DURATION - 36) / 3)

    ; TODO: write techical aspects of the implementation

.macro __setup_sw_uart_int0
    ; @0 External interrupt INT0 mode register
    ; @1 External interrupt INT0 enable register
    ldi     AL, M(ISC01)            ;     Falling edge of INT0 generates an
    stio    @0, AL                  ;     interrupt request
    ldi     AL, M(INT0)             ;     Allow INT0 ISR execution
    stio    @1, AL                  ;
    ldi     AL, M(ADEN) | M(ADSC)   ;     Enable ADC, set min prescaler & start
    stio    ADCSRA, AL              ;     conversion to achieve stable 13 cycles
.endmacro
#define code_setup_sw_uart_int0(EIMR, EIER) \
__setup_sw_uart_int0 EIMR, EIER

.macro __sw_uart_int0_sbit_isr
   .if      (F_CPU != 16000000 && F_CPU != 8000000)
   .error   "CPU clock must be 16 or 8 Mhz"
   .endif
    CLR_PROBE(UART_START)
   .if      (F_CPU == 16000000)     ;     Delay: 32*13=416=1.5*277.3 cycles
    ldi     YH, M(ADEN) | M(ADSC) | M(ADIE) | M(ADPS2) | M(ADPS0)
   .elif    (F_CPU == 8000000)      ;     Delay: 16*13=208=1.5*138.7 cycles
    ldi     YH, M(ADEN) | M(ADSC) | M(ADIE) | M(ADPS2)
   .endif
    stio    ADCSRA, YH              ;     Set prescale and start conversion
    stio    @0, ZERO                ;     Disable INT0 ISR execution
    ldi     YH, 0x80                ;     Init UART data shift register
    sts     uart_data, YH           ;
    CLR_PROBE(UART_DELAY)
    reti                            ;
.endmacro
#define proc_sw_uart_int0_sbit_isr(EIER) \
sw_uart_int0_sbit_isr: __sw_uart_int0_sbit_isr EIER

.macro __sw_uart_int0_dbit_isr
    ; @0 mcu port for UART RX input
    ; @1 mcu port bit number of UART RX pin
   .if      (F_CPU != 16000000 && F_CPU != 8000000)
   .error   "CPU clock must be 16 or 8 Mhz"
   .endif
    push    YH                      ; 2
    ldio    YH, SREG                ; 1~2
    push    YH                      ; 2
    lds     YH, uart_data           ; 1~2
    clc                             ; 1
    sbic    @0, @1                  ; 1|2 Check UART RX pin
    sec                             ; 1
    ror     YH                      ; 1
    TGL_PROBE(UART_DELAY)
    brcs    handle_uart_data        ; 1|2
    sts     uart_data, YH           ; 1~2
   .if      (BIT_EX_DELAY > 0)
    ldi     YH, BIT_EX_DELAY        ; 1
extra_delay_loop:
    dec     YH                      ; 1
    brne    extra_delay_loop        ; 1
   .endif
   .if      (F_CPU == 16000000)     ;     Delay: 16*13=208, extra: 277-208=69
    ldi     YH, M(ADEN) | M(ADSC) | M(ADIE) | M(ADPS2)
   .elif    (F_CPU == 8000000)      ;     Delay: 8*13=104,  extra: 138-104=34
    ldi     YH, M(ADEN) | M(ADSC) | M(ADIE) | M(ADPS1) | M(ADPS0)
   .endif
    stio    ADCSRA, YH              ;     Set prescale and start conversion
    rjmp    exit_isr
handle_uart_data:
    CLR_PROBE(UART_STORE)
    push    YL                      ;
    code_handle_uart_data()         ;
    pop     YL                      ;
    SET_PROBE(UART_STORE)
    ldi     YH, M(INTF0)            ;     Clear any pending INT0 ISR
    stio    @2, YH                  ;
    ldi     YH, M(INT0)             ;     Allow INT0 ISR execution
    stio    @3, YH                  ;
    SET_PROBE(UART_DELAY)
    SET_PROBE(UART_START)
exit_isr:
    pop     YH                      ;
    stio    SREG, YH                ;
    pop     YH                      ;
    reti                            ;
.endmacro
#define proc_sw_uart_int0_dbit_isr(P, B, EIFR, EIER) \
sw_uart_int0_dbit_isr: __sw_uart_int0_dbit_isr PIN##P, PORT##P##B, EIFR, EIER

; HARDWARE UART DATA RECEIVE ---------------------------------------------------
.macro __hw_uart_data_isr
    ; @0 mcu register for UART RX input
    push    YL                      ;
    push    YH                      ;
    ldio    YH, SREG                ;
    push    YH                      ;
    ldio    YH, @0                  ;
    code_handle_uart_data()         ;
    pop     YH                      ;
    stio    SREG, YH                ;
    pop     YH                      ;
    pop     YL                      ;
    reti                            ;
.endmacro
#define proc_hw_uart_data_isr(UART) \
hw_uart_data_isr: __hw_uart_data_isr UART

; SYNCHRONIZE IN TIME AND OUTPUT PREVIOUSLY COMPUTED RESULT --------------------
.macro __sync_and_out
    ; @0 mcu register containing timer overflow flag
    ; @1 mcu timer overflow flag bit in register
    ; @2 PWM channel A
    ; @3 PWM channel B
    ; AVR8L_0: 6-4
    ; V2/V2E:  6-4
    ldio    AL, @0                  ; 1   Check timer overflow flag
    sbrs    AL, @1                  ; 1|2 Skip next instruction if flag is set
    rjmp    loop                    ; 2   otherwise jump to the loop beginning
    stio    @0, AL                  ; 1   Clear timer overflow flag
    stio    @2, XL                  ; 1   Output L/R channel 8-bit samples or
    stio    @3, XH                  ; 1   mono channel 16-bit sample
.endmacro
#define code_sync_and_out(TOVF_R, TOVF_F, PWM_A, PWM_B) \
__sync_and_out TOVF_R, TOVF_F, PWM_A, PWM_B

; UPDATE TONE GENERATOR --------------------------------------------------------
.macro __update_tone
    ; AL U_STEP constant
    ; @0 channel tone period
    ; @1 channel tone counter
    ; @2 channel bit
    ; AVR8L_0: 24-12
    ; V2/V2E:  30-18
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

.macro __update_tones
    ; AVR8L_0: 73-37
    ; V2/V2E:  91-55
    ldi     AL, U_STEP              ; 1
    code_update_tone(a_period, a_counter, AFFBIT)
    code_update_tone(b_period, b_counter, BFFBIT)
    code_update_tone(c_period, c_counter, CFFBIT)
.endmacro
#define code_update_tones() \
__update_tones

; REINIT ENVELOPE GENERATOR ----------------------------------------------------
.macro __reinit_envelope
    ; AVR8L_0: 12-3
    ; V2/V2E:  16-3
    sbrs    flags, EG_RES           ; 1|2 Skip next instruction if no need to
    rjmp    reinit_exit             ; 2   reset envelope generator
    cbr     flags, M(EG_RES)        ; 1   Clear request for reset envelope
    sts     L(e_counter), ZERO      ; 1~2 Reset envelope counter
    sts     H(e_counter), ZERO      ; 1~2
    lds     e_gen, e_shape          ; 1~2 Init envelope generator with a new
    lsl     e_gen                   ; 1   shape, index in table is shape * 4
    lsl     e_gen                   ; 1
    ldi     ZL, low(P(envelopes)+1) ; 1   Init envelope step with value from
    ldp     e_stp, e_gen            ; 3~4 envelope generator table
reinit_exit:
.endmacro
#define code_reinit_envelope() \
__reinit_envelope

; UPDATE NOISE AND ENVELOPE GENERATORS -----------------------------------------
.macro __update_noise_envelope
    ; +---+---------------+---------------+
    ; | U |32 steps       |16 steps       |
    ; | S |-------+-------|-------+-------|
    ; | T |AVR8L_0|V2/V2E |AVR8L_0|V2/V2E |
    ; | E |---+---|---+---|---+---|---+---|
    ; | P |max|min|max|min|max|min|max|min|
    ; |---|---|---|---|---|---|---|---|---|
    ; | A |402|142|433|153|206|76 |227|87 |
    ; |---|---|---|---|---|---|---|---|---|
    ; | 9 |363|129|392|140| - | - | - | - |
    ; |---|---|---|---|---|---|---|---|---|
    ; | 8 |324|116|351|127|167|63 |186|74 |
    ; |---|---|---|---|---|---|---|---|---|
    ; | 7 |285|103|310|114| - | - | - | - |
    ; |---|---|---|---|---|---|---|---|---|
    ; | 6 |246|90 |269|101|128|50 |145|61 |
    ; |---|---|---|---|---|---|---|---|---|
    ; | 5 |207|77 |228|88 | - | - | - | - |
    ; |---|---|---|---|---|---|---|---|---|
    ; | 4 |168|64 |187|75 |89 |37 |104|48 |
    ; |---|---|---|---|---|---|---|---|---|
    ; | 3 |129|51 |146|62 | - | - | - | - |
    ; |---|---|---|---|---|---|---|---|---|
    ; | 2 |90 |38 |105|49 |50 |24 |63 |35 |
    ; |---|---|---|---|---|---|---|---|---|
    ; | 1 |51 |25 |64 |36 | - | - | - | - |
    ; +---+---+---+---+---+---+---+---+---+

    ; 32 step envelope:
    ;   AVR8L_0:
    ;       max: 9  + U_STEP * (16 + 20 + 3) + 3 = U_STEP * 39 + 12
    ;       min: 9  + U_STEP * (4  + 6  + 3) + 3 = U_STEP * 13 + 12
    ;   V2/V2E:
    ;       max: 16 + U_STEP * (16 + 22 + 3) + 7 = U_STEP * 41 + 23
    ;       min: 16 + U_STEP * (4  + 6  + 3) + 7 = U_STEP * 13 + 23
    ; 16 step envelope:
    ;   AVR8L_0:
    ;       max: 8  + (U_STEP / 2) * (16 + 20 + 3) + 3 = (U_STEP / 2) * 39 + 11
    ;       min: 8  + (U_STEP / 2) * (4  + 6  + 3) + 3 = (U_STEP / 2) * 13 + 11
    ;   V2/V2E:
    ;       max: 15 + (U_STEP / 2) * (16 + 22 + 3) + 7 = (U_STEP / 2) * 41 + 22
    ;       min: 15 + (U_STEP / 2) * (4  + 6  + 3) + 7 = (U_STEP / 2) * 13 + 22

   .if      (@0 != 16 && @0 != 32)
   .error   "Unknown number of steps for envelope"
   .endif
    ; Read state from SRAM and init loop
    ; AVR8L_0: 9-9
    ; V2/V2E:  16-16
    lds     XL, L(e_period)         ; 1~2 Load envelope peiod from SRAM
    lds     XH, H(e_period)         ; 1~2 Envelope period high byte
    lds     YL, L(e_counter)        ; 1~2 Load envelope counter from SRAM
    lds     YH, H(e_counter)        ; 1~2 Envelope counter high byte
    lds     BL, L(n_shifter)        ; 1~2 Load noise shifter from SRAM
    lds     BH, H(n_shifter)        ; 1~2 Noise shifter high byte
    lds     AL, n_period            ; 1~2 Load noise period from SRAM and double
   .if      (@0 == 16)
   .if      (U_STEP/2 != U_STEP-U_STEP/2)
   .error   "Update step must be even number"
   .endif
    ldi     AH, U_STEP/2            ; 1   Init update iterations
   .else
    lsl     AL                      ; 1   it to simulate noise prescaler
    ldi     AH, U_STEP              ; 1   Init update iterations
   .endif
update_loop:

    ; Update noise generator
    ; AVR8L_0: 16-4
    ; V2/V2E:  16-4
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
    cbr     flags, NFFMSK           ; 1   Set noise output flags according to
    sbrc    BL, bit0                ; 1|2 bit0 of the current shifter state
    sbr     flags, NFFMSK           ; 1
exit_noise:
    inc     n_cnt                   ; 1   counter = counter + 1

    ; Update envelope generator
    ; AVR8L_0: 20-6 
    ; V2/V2E:  22-6
    cp      YL, XL                  ; 1   Compare counter against period
    cpc     YH, XH                  ; 1
    brlo    exit_envelope           ; 1|2 Skip following if counter < period
    clr     YL                      ; 1   Reset counter
    clr     YH                      ; 1
    ldi     ZL, low(P(envelopes))   ; 1   Inc/dec envelope step counter depend-
    ldp     ZL, e_gen               ; 3~4 ing on envelope generation config
    add     e_stp, ZL               ; 1
    cpi     e_stp, @0               ; 1   When envelope step reaches 16/32 after
    brlo    exit_envelope           ; 1|2 increment or get 255 after decrement
    ldi     ZL, 0b00000010          ; 1   then generation config switches to the
    eor     e_gen, ZL               ; 1   alternative phase and envelope step
    ldi     ZL, low(P(envelopes)+1) ; 1   reloads with a new value from config
    ldp     e_stp, e_gen            ; 3~4
exit_envelope:
    ld      ZL, Y+                  ; 2   counter = counter + 1

    ; Go to the next iteration or exit
    ; AVR8L_0: 6-3 
    ; V2/V2E:  10-3
    dec     AH                      ; 1   Decrement iteration counter and
    brne    update_loop             ; 1|2 go to next interation
    sts     L(n_shifter), BL        ; 1~2 Save noise shifter into SRAM
    sts     H(n_shifter), BH        ; 1~2 Noise shifter high byte
    sts     L(e_counter), YL        ; 1~2 Save envelope counter into SRAM
    sts     H(e_counter), YH        ; 1~2 Envelope counter high byte
.endmacro
#define code_update_noise_envelope(STEPS) \
__update_noise_envelope STEPS

; APPLY MIXER FLAGS AND COMPUTE TONE/NOISE LEVELS ------------------------------
.macro __apply_mixer
    ; AH tone/noise level bits output
    ; AVR8L_0: 6
    ; V2/V2E:  7
    lds     AH, mixer               ; 1~2 Mixer: xxCB.Acba (CBA:Noise, cba:Tone)
    or      AH, flags               ; 1   Apply disables:  xxCB.Acba | xxNN.Ncba
    mov     AL, AH                  ; 1
    lsl     AL                      ; 1   Shift left:      xxCB.Acba > xCBA.cbax
    swap    AL                      ; 1   Swap nibbles:    xCBA.cbax > cbax.xCBA
    and     AH, AL                  ; 1   Output:          xxxx.xcba & xxxx.xCBA
.endmacro
#define code_apply_mixer() \
__apply_mixer

; COMPUTE ENVELOPE AMPLITUDE ---------------------------------------------------
.macro __compute_envelope_amp
    ; AL envelope amplitude
    ; AVR8L_0: 4
    ; V2/V2E:  5
   .if      (@0 != 16 && @0 != 32)
   .error   "Unknown number of steps for envelope"
   .endif
   .if      (@0 == 16)
    ldi     ZL, low(P(amp_4bit))    ; 1
   .else
    ldi     ZL, low(P(amp_5bit))    ; 1
   .endif
    ldp     AL, e_stp               ; 3~4
.endmacro
#define code_compute_envelope_amp(STEPS) \
__compute_envelope_amp STEPS

; COMPUTE CHANNEL AMPLITUDE ----------------------------------------------------
.macro __compute_channel_amp
    ; AL envelope amplitude
    ; AH mixer output
    ; BL amplitude table offset
    ; @0 channel volume
    ; @1 channel bit
    ; @2 output amplitude
    ; AVR8L_0: 9-7
    ; V2/V2E:  11-8
    mov     @2, AL                  ; 1   Use envelope amplitude by default
    lds     ZL, @0                  ; 1~2 volume+envelope flag from PSG register
    bst     ZL, bit4                ; 1   Check if envelope enabled in this
    brts    use_envelope            ; 1|2 channel and skip amplitude computation
    ldp     @2, BL                  ; 3~4 Get amplitude from table using volume
use_envelope:
    sbrs    AH, @1                  ; 1|2 If channel disabled in mixer (N and T)
    clr     @2                      ; 1   then set amplitude to zero value
.endmacro
#define code_compute_channel_amp(VOLUME, CHANNEL, AMP) \
__compute_channel_amp VOLUME, CHANNEL, AMP

.macro __compute_channels_amp
    ; AL envelope amplitude
    ; AH mixer output
    ; XL channel A amplitude
    ; BH channel B amplitude
    ; XH channel C amplitude
    ; AVR8L_0: 28-22
    ; V2/V2E:  34-25
    ldi     BL, low(P(amp_4bit))    ; 1
    code_compute_channel_amp(a_volume, AFFBIT, XL)
    code_compute_channel_amp(b_volume, BFFBIT, BH)
    code_compute_channel_amp(c_volume, CFFBIT, XH)
.endmacro
#define code_compute_channels_amp() \
__compute_channels_amp

; COMPUTE STEREO 8-BIT PER CHANNEL OUTPUT --------------------------------------
.macro __compute_output_abc
    ; XL channel A amplitude -> stereo L sample
    ; BH channel B amplitude
    ; XH channel C amplitude -> stereo R sample
    ; AVR8L_0: 5
    ; V2/V2E:  5
    lsr     BH                      ; 1   Divide B channel amplitude by 2
    add     XL, BH                  ; 1   Left  = Add B channel to A channel
    add     XH, BH                  ; 1   Right = Add B channel to C channel
    rjmp    loop                    ; 2
.endmacro
#define code_compute_output_abc \
__compute_output_abc

.macro __compute_output_acb
    ; XL channel A amplitude -> stereo L sample
    ; BH channel B amplitude
    ; XH channel C amplitude -> stereo R sample
    ; AVR8L_0: 5
    ; V2/V2E:  5
    lsr     XH                      ; 1   Divide C channel amplitude by 2
    add     XL, XH                  ; 1   Left  = Add C channel to A channel
    add     XH, BH                  ; 1   Right = Add C channel to B channel
    rjmp    loop                    ; 2
.endmacro
#define code_compute_output_acb \
__compute_output_acb

.macro __compute_output
    ; XL channel A amplitude -> stereo L sample
    ; BH channel B amplitude
    ; XH channel C amplitude -> stereo R sample
    ; AVR8L_0: 8-7
    ; V2/V2E:  8-7
    sbis    @0, @1                  ; 1|2 1 - default output mode (ABC),
    rjmp    output_alternative      ; 2   0 - alternative output mode (ACB)
    code_compute_output_abc()       ; 5   Default output implementation
output_alternative:
    code_compute_output_acb()       ; 5   Alternative output implementation
.endmacro
#define code_compute_output(P, B) \
__compute_output PIN##P, PORT##P##B

; ==============================================================================
; SRAM BLOCKS
; ==============================================================================

#define sram_psg_regs_and_state()   \
psg_regs:                           \
a_period:  .byte 2                  \
b_period:  .byte 2                  \
c_period:  .byte 2                  \
n_period:  .byte 1                  \
mixer:     .byte 1                  \
a_volume:  .byte 1                  \
b_volume:  .byte 1                  \
c_volume:  .byte 1                  \
e_period:  .byte 2                  \
e_shape:   .byte 1                  \
unused_0:  .byte 1                  \
uart_data: .byte 1                  \
a_counter: .byte 2                  \
b_counter: .byte 2                  \
c_counter: .byte 2                  \
n_shifter: .byte 2                  \
e_counter: .byte 2                  \
psg_end:
