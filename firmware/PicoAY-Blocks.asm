; ------------------------------------------------------------------------------

.macro __reg_mask
    ; 16 bytes
    .db     0xFF, 0x0F, 0xFF, 0x0F, 0xFF, 0x0F, 0x1F, 0x3F
    .db     0x1F, 0x1F, 0x1F, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF
.endmacro
#define cseg_reg_mask() reg_mask: __reg_mask

.macro __amp_4bit
    ; @0 - Maximum amplitude
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
#define cseg_amp_4bit(MAX_AMP) amp_4bit: __amp_4bit MAX_AMP

.macro __amp_5bit
    ; @0 - Maximum amplitude
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
#define cseg_amp_5bit(MAX_AMP) amp_5bit: __amp_5bit MAX_AMP

.macro __envelopes
    ; @0 - Number of steps in waveform
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
#define cseg_envelopes(STEPS) envelopes: __envelopes STEPS
