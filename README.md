# PicoAY project

### Microcontroller requirements
- From 6 to 8 pin package design
- Internal oscillator capable to run at 8, 12, 16 or 20 MHz
- Flash memory at least 1024 bytes, SRAM is 32 bytes or more
- AVR CPU core at least AVR8L (16 registers, 54 instructions)
- One or more pins capable of sensing the input signal change
- Analog to digital converter (ADC) or hardware USART module
- At least one timer with two independent compare units for PWM
- Watchdog timer (WDT) supporting interrupt service routine (ISR)

### Firmware options
||Units|FW-T10|FW-T102|FW-T25|
|-|:-:|:-:|:-:|:-:|
|MCU model|-|ATtiny10|ATtiny102|ATtiny25
|MCU system clock|Mhz|12|11.059|16|
|MCU cycles per PSG sample|cycle|328|302|432|
|PSG virtual clock|Mhz|1.756|1.758|1.778|
|PSG envelope steps|-|16|16|32|
|PSG sample rate|Hz|36585|36620|37037|
|PSG sample timer|-|Timer0 (16)|Timer0 (16)|Timer0 (8)|
|PWM sample rate|kHz|36.6|36.6|250|
|PWM sample timer|-|Timer0 (16)|Timer0 (16)|Timer1 (8)|
|PWM resolution|levels|164|151|256|
|UART speed|baud|57600|57600|57600|
|UART implementation|-|Software|Hardware|Software|
|UART receive|pin|4 (PB2)|7 (PB3)|7 (PB2)|
|PWM audio left channel|pin|1 (PB0)|5 (PB1)|6 (PB1)|
|PWM audio right channel|pin|3 (PB1)|3 (PA1)|3 (PB4)|
|PSG chip #0/#1 select|pin|-|6 (PB2)|5 (PB0)|
|PSG stereo ABC/ACB select|pin|-|2 (PA0)|2 (PB3)|
|MCU reset|pin|6 (PB3)|4 (PA2)|1 (PB5)|
|MCU programming interface|-|TPI|TPI|ICSP|

### Hardware options
|Model|MCU|Firmware|Channels|Vcc|Stereo|RC (R)|RC (C)|Vp-p|
|-|-|-|-|-|-|-|-|-|
|HW-T10-X1-3.3|1 x ATtiny10|FW-T10|3|3.3V|ABC|?|?|?|
|HW-T10-X1-5.0|1 x ATtiny10|FW-T10|3|5.0V|ABC|?|?|?|
|HW-T10-X2-3.3|2 x ATtiny10|FW-T10|6|3.3V|ABC|?|?|?|
|HW-T10-X2-5.0|2 x ATtiny10|FW-T10|6|5.0V|ABC|?|?|?|
|HW-T102-X1-3.3|1 x ATtiny102|FW-T102|3|3.3V|ABC/ACB|?|?|?|
|HW-T102-X1-5.0|1 x ATtiny102|FW-T102|3|5.0V|ABC/ACB|?|?|?|
|HW-T102-X2-3.3|2 x ATtiny102|FW-T102|6|3.3V|ABC/ACB|?|?|?|
|HW-T102-X2-5.0|2 x ATtiny102|FW-T102|6|5.0V|ABC/ACB|?|?|?|
|HW-T25-X1-3.3|1 x ATtiny25|FW-T25|3|3.3V|ABC/ACB|?|?|?|
|HW-T25-X1-5.0|1 x ATtiny25|FW-T25|3|5.0V|ABC/ACB|?|?|?|
|HW-T25-X2-3.3|2 x ATtiny25|FW-T25|6|3.3V|ABC/ACB|?|?|?|
|HW-T25-X2-5.0|2 x ATtiny25|FW-T25|6|5.0V|ABC/ACB|?|?|?|