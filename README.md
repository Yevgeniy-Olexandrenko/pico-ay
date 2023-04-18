PicoAY project

||Units|ATtiny10|ATtiny102|ATtiny25|
|-|:-:|:-:|:-:|:-:|
|Firmware features|-|Standard|Standard|Standard|
|MCU system clock|Mhz|8|8|16|
|MCU cycles per PSG sample|cycle|292|292|432|
|PSG virtual clock|Mhz|1.753|1.753|1.778|
|PSG envelope steps|-|16|16|32|
|PSG sample rate|kHz|27.4|27.4|37|
|PSG sample timer|-|Timer0 (16)|Timer0 (16)|Timer0 (8)|
|PWM sample rate|kHz|27.4|27.4|250|
|PWM sample timer|-|Timer0 (16)|Timer0 (16)|Timer1 (8)|
|UART speed|baud|57600|57600|57600|
|UART implementation|-|Software|Hardware|Software|
|UART receive|pin|4 (PB2)|7 (PB3)|7 (PB2)|
|PWM audio left channel|pin|1 (PB0)|5 (PB1)|6 (PB1)|
|PWM audio right channel|pin|3 (PB1)|3 (PA1)|3 (PB4)|
|PSG chip #0/#1 select|pin|-|6 (PB2)|5 (PB0)|
|PSG stereo ABC/ACB mode|pin|-|2 (PA0)|2 (PB3)|
|MCU reset|pin|6 (PB3)|4 (PA2)|1 (PB5)|
|MCU programming interface|-|TPI|TPI|ICSP|
