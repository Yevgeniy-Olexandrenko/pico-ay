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
#define data_reg_mask() reg_mask: __reg_mask

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
#define data_amp_4bit(MAX_AMP) amp_4bit: __amp_4bit MAX_AMP

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
#define data_amp_5bit(MAX_AMP) amp_5bit: __amp_5bit MAX_AMP

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
#define data_envelopes(STEPS) envelopes: __envelopes STEPS

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
#define code_setup_sram() __setup_sram

; SET Z POINTER TO FIRST 256 BYTES OF FLASH TO ASSESS DATA ---------------------
.macro __setup_data_access
.ifdef MAPPED_FLASH_START
    ldi     ZH, high(MAPPED_FLASH_START)
.else
    ldi     ZH, 0x00
.endif
.endmacro
#define code_setup_data_access() __setup_data_access

; SETUP EVERYTHING ELSE AND START EMULATOR -------------------------------------
.macro __setup_and_start_emulator
    ldi     flags, M(NS_B16) | M(EG_RES)
    ldi     raddr, M(WF_REG)        ; Wait the register address to write
    mov     XL, AH                  ; Load zero to left channel sample register
    mov     XH, AH                  ; Load zero to right channel sample register
    sei                             ; Enable interrupts
    rjmp    loop                    ; Go to main loop
.endmacro
#define code_setup_and_start_emulator() __setup_and_start_emulator

; LOAD DATA FROM FLASH USING TABLE BASE AND DISPLACEMENT -----------------------
.macro ldp
    ; ZL table base or displasement
    ; @0 destination register
    ; @1 displacement or table base
#ifdef __CORE_AVR8L_0__
    add     ZL, @1                  ; Add table base with displacement
    ld      @0, Z                   ; Load indirrect from Z
#elif __CORE_V2__
    add     ZL, @1                  ; Add table base with displacement
    lpm     @0, Z                   ; Load indirrect from Z
#else
    .error "Unknown AVR core version"
#endif
.endmacro

; HANDLE UART RECEIVED DATA BYTE ACCORDING TO PROTOCOL -------------------------
.macro __handle_uart_data
    ; YH received data byte
    ; modified: raddr, flags, YL, YH, Stack
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
#define code_handle_uart_data() __handle_uart_data

; SOFTWARE UART DATA RECEIVE ISR -----------------------------------------------
.macro __sw_uart_rx_isr
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
    sbic    PINB, PORTB2            ; 1|2 Check UART RX PIN
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
#define code_sw_uart_rx_isr() sw_uart_rx_isr: __sw_uart_rx_isr

; HARDWARE UART DATA RECEIVE ISR -----------------------------------------------
.macro __hw_uart_rx_isr
    ; TODO
.endmacro
#define code_hw_uart_rx_isr() hw_uart_isr: __hw_uart_rx_isr
