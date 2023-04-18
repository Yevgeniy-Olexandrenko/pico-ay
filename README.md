# PicoAY project

<details>
<summary>ATtiny10 firmware</summary>

||Units|ATtiny10|ATtiny10|
|-|:-:|:-:|:-:|
|Firmware features|-|Standard|Overclocked|
|Sound quality|-|Acceptable|Good|
|MCU system clock|Mhz|8|12|
|MCU cycles per PSG sample|cycle|292|???|
|PSG virtual clock|Mhz|1.753|???|
|PSG envelope steps|-|16|???|
|PSG sample rate|kHz|27.4|???|
|PSG sample timer|-|Timer0 (16)|Timer0 (16)|
|PWM sample rate|kHz|27.4|???|
|PWM sample timer|-|Timer0 (16)|Timer0 (16)|
|UART speed|baud|57600|57600|
|UART implementation|-|Software|Software|

</details>

<details>
<summary>ATtiny102 firmware</summary>

||Units|ATtiny102|ATtiny102|
|-|:-:|:-:|:-:|
|Firmware features|-|Standard|Overclocked|
|Sound quality|-|Acceptable|Good|
|MCU system clock|Mhz|8|12|
|MCU cycles per PSG sample|cycle|292|???|
|PSG virtual clock|Mhz|1.753|???|
|PSG envelope steps|-|16|???|
|PSG sample rate|kHz|27.4|???|
|PSG sample timer|-|Timer0 (16)|Timer0 (16)|
|PWM sample rate|kHz|27.4|???|
|PWM sample timer|-|Timer0 (16)|Timer0 (16)|
|UART speed|baud|57600|57600|
|UART implementation|-|Hardware|Hardware|

</details>

<details>
<summary>ATtiny25 firmware</summary>

||Units|ATtiny25|
|-|:-:|:-:|
|Firmware features|-|Standard|
|Sound quality|-|Awesome|
|MCU system clock|Mhz|16|
|MCU cycles per PSG sample|cycle|432|
|PSG virtual clock|Mhz|1.778|
|PSG envelope steps|-|32|
|PSG sample rate|kHz|37|
|PSG sample timer|-|Timer0 (8)|
|PWM sample rate|kHz|250|
|PWM sample timer|-|Timer1 (8)|
|UART speed|baud|57600|
|UART implementation|-|Software|

</details>

<details>
<summary>Pinouts</summary>

||Units|ATtiny10|ATtiny102|ATtiny25|
|-|:-:|:-:|:-:|:-:|
|UART receive|pin|4 (PB2)|7 (PB3)|7 (PB2)|
|PWM audio left channel|pin|1 (PB0)|5 (PB1)|6 (PB1)|
|PWM audio right channel|pin|3 (PB1)|3 (PA1)|3 (PB4)|
|PSG chip #0/#1 select|pin|-|6 (PB2)|5 (PB0)|
|PSG stereo ABC/ACB mode|pin|-|2 (PA0)|2 (PB3)|
|MCU reset|pin|6 (PB3)|4 (PA2)|1 (PB5)|
|MCU programming interface|-|TPI|TPI|ICSP|

</details>
