#if 1

#include <stdint.h>
#include <Windows.h>
#include <MMSystem.h>

#undef min
#undef max 

#include <fstream>
#include <algorithm>

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

const uint32_t F_CPU        = 8000000;// 8000000;
const uint32_t F_PSG        = 1774300;
const uint8_t  PWM_BITS     = 8;
const uint8_t  PWM_CHANNELS = 1;

const uint8_t  FRAME_RATE   = 50;
const uint32_t SAMPLE_RATE  = (F_CPU / (1 << PWM_BITS));//(F_CPU / 289);
const uint8_t  SAMPLE_SIZE  = (PWM_BITS / 8 * PWM_CHANNELS);
const uint16_t PSG_C_PER_S  = (F_PSG / 8 / SAMPLE_RATE);
const uint16_t BUFFER_SIZE  = (SAMPLE_RATE / FRAME_RATE) * SAMPLE_SIZE;

// PSG registers
#pragma pack(push, 1)
union {
	struct {
		uint16_t a_period;
		uint16_t b_period;
		uint16_t c_period;
		uint8_t  n_period;
		uint8_t  mixer;
		uint8_t  a_volume;
		uint8_t  b_volume;
		uint8_t  c_volume;
		uint16_t e_period;
		uint8_t  e_shape;
	};
	uint8_t reg[14];
} psg;

struct {
	uint16_t a_counter; // tone A generator period counter
	uint16_t b_counter; // tone B generator period counter
	uint16_t c_counter; // tone C generator period counter
	uint8_t  n_counter; // noise generator period counter
	uint16_t n_shift;   // noise generator shift register
	uint16_t e_counter; // envelope generator period counter
	int8_t   e_volume;  // envelope generator shape counter 
	uint8_t  output;    // S.P.a.b.c.C.B.A
	                    // | | | | | | | |
	                    // | | *-*-* | | *-> tone A generator waveform
	                    // | |   |   | *---> tone B generator waveform
	                    // | |   |   *-----> tone C generator waveform
	                    // | |   *---------> noise generator waveform
						// | *-------------> noise generator prescale
	                    // *---------------> envelope generator segment
	uint8_t  flags;
} state;

#pragma pack(pop)

const uint8_t amplitude[] =
{
	0x00 >> 0, 0x00 >> 0, 0x01 >> 0, 0x02 >> 0, 0x03 >> 0, 0x04 >> 0, 0x04 >> 0, 0x05 >> 0,
	0x06 >> 0, 0x08 >> 0, 0x09 >> 0, 0x0A >> 0, 0x0C >> 0, 0x0F >> 0, 0x11 >> 0, 0x14 >> 0,
	0x18 >> 0, 0x1C >> 0, 0x21 >> 0, 0x26 >> 0, 0x2D >> 0, 0x36 >> 0, 0x3F >> 0, 0x48 >> 0,
	0x55 >> 0, 0x66 >> 0, 0x77 >> 0, 0x88 >> 0, 0xA2 >> 0, 0xC1 >> 0, 0xE0 >> 0, 0xFF >> 0
};

#define AMPLITUDE_4b(v)  (amplitude[(v << 1) | 1])
#define AMPLITUDE_5b(v)  (amplitude[v])
#define THREEQUARTERS(x) ((x >> 2) + (x >> 1))

enum { SU = +1, SD = -1, HT = 0, HB = 0, RT = 0x1F, RB = 0x00 };

const int envelopes[16][2][2]
{
	{ { SD, RT }, { HB, RB }, },
	{ { SD, RT }, { HB, RB }, },
	{ { SD, RT }, { HB, RB }, },
	{ { SD, RT }, { HB, RB }, },
	{ { SU, RB }, { HB, RB }, },
	{ { SU, RB }, { HB, RB }, },
	{ { SU, RB }, { HB, RB }, },
	{ { SU, RB }, { HB, RB }, },
	{ { SD, RT }, { SD, RT }, },
	{ { SD, RT }, { HB, RB }, },
	{ { SD, RT }, { SU, RB }, },
	{ { SD, RT }, { HT, RT }, },
	{ { SU, RB }, { SU, RB }, },
	{ { SU, RB }, { HT, RT }, },
	{ { SU, RB }, { SD, RT }, },
	{ { SU, RB }, { HB, RB }, },
};

void avr_init()
{
	memset(&psg, 0, sizeof(psg));
	memset(&state, 0, sizeof(state));
}

void avr_update(uint8_t& out_l, uint8_t& out_r, uint8_t& out_c)
{
	if (state.flags & 0b00000001)
	{
		state.e_counter = 0;
		state.flags &= 0b01111110;
		state.e_volume = envelopes[psg.e_shape][state.flags >> 7 & 1][1];
	}

	int16_t counter;
	uint16_t period;
	uint8_t  output = state.output;
	uint8_t  volume, e_sample = 0;
	uint8_t  sample_a, sample_b, sample_c;

	const uint16_t _PSG_C_PER_S = PSG_C_PER_S;
	//for (int i = 0; i < PSG_C_PER_S; ++i)
	{
		// update tone A
#if 1
		counter = state.a_counter;
		counter -= _PSG_C_PER_S;
		if (counter <= 0)
		{
			counter += psg.a_period;
			output ^= 0b00000001;
		}
		state.a_counter = counter;
#else
		period = psg.a_period;
		counter = state.a_counter;
		counter += _PSG_C_PER_S;
		if (counter >= period) 
		{ 
			counter -= period;
			if (counter > _PSG_C_PER_S)
			{
				//counter %= _PSG_C_PER_S;
				counter &= 0x0007;
				//::printf("A: %d\n", counter);
			}
			output ^= 0b00000001; 
		}
		state.a_counter = counter;
#endif

		// update tone B
#if 1
		counter = state.b_counter;
		counter -= _PSG_C_PER_S;
		if (counter <= 0)
		{
			counter += psg.b_period;
			output ^= 0b00000010;
		}
		state.b_counter = counter;
#else
		period = psg.b_period;
		counter = state.b_counter;
		counter += _PSG_C_PER_S;
		if (counter >= period) 
		{ 
			counter -= period;
			if (counter > _PSG_C_PER_S)
			{
				//counter %= _PSG_C_PER_S;
				counter &= 0x0007;
				//::printf("B: %d\n", counter);
			}
			output ^= 0b00000010;
		}
		state.b_counter = counter;
#endif

		// update tone C
#if 1
		counter = state.c_counter;
		counter -= _PSG_C_PER_S;
		if (counter <= 0)
		{
			counter += psg.c_period;
			output ^= 0b00000100;
		}
		state.c_counter = counter;
#else
		period = psg.c_period;
		counter = state.c_counter;
		counter += _PSG_C_PER_S;
		if (counter >= period) 
		{
			counter -= period;
			if (counter > _PSG_C_PER_S)
			{
				//counter %= _PSG_C_PER_S;
				counter &= 0x0007;
				//::printf("C: %d\n", counter);
			}
			output ^= 0b00000100; 
		}
		state.c_counter = counter;
#endif

		// update envelope
#if 1
		int volume = state.e_volume;
		counter = state.e_counter;
		counter -= _PSG_C_PER_S;
		if (counter <= 0)
		{
			counter += psg.e_period;

			volume += envelopes[psg.e_shape][state.flags >> 7 & 1][0];
			if (volume < RB || volume > RT)
			{
				state.flags ^= 0b10000000;
				volume = envelopes[psg.e_shape][state.flags >> 7 & 1][1];
			}
		}
		state.e_counter = uint16_t(counter);
		state.e_volume = int8_t(volume);
		e_sample = AMPLITUDE_5b(volume);
#else
		{
			period = psg.e_period;
			uint32_t counter = state.e_counter;
			counter += _PSG_C_PER_S;
			int volume = state.e_volume;

			if (counter >= period)
			{
				counter -= period;
				if (counter > _PSG_C_PER_S)
				{
					//counter %= _PSG_C_PER_S;
					counter &= 0x0007;
					//::printf("E: %d\n", counter);
				}

				volume += envelopes[psg.e_shape][state.flags >> 7 & 1][0];
				if (volume < RB || volume > RT)
				{
					state.flags ^= 0b10000000;
					volume = envelopes[psg.e_shape][state.flags >> 7 & 1][1];
				}
			}
			state.e_counter = uint16_t(counter);
			state.e_volume = int8_t(volume);
			e_sample = AMPLITUDE_5b(volume);
		}
#endif
	}

	// update output by mixer
	state.output = output;
	output |= psg.mixer | 0b00111000;
	output = (output & 0b0000111) & ((output >> 3) & 0b00000111);

	// generate sample B
	sample_b = e_sample;
	volume = psg.b_volume;
	if ((volume & 0b00010000) == 0) { sample_b = AMPLITUDE_4b(volume); }
	if ((output & 0b00000010) == 0) { sample_b = 0; }
	//sample_b = THREEQUARTERS(sample_b);

	// generate sample A
	sample_a = e_sample;
	volume = psg.a_volume;
	if ((volume & 0b00010000) == 0)	{ sample_a = AMPLITUDE_4b(volume); }
	if ((output & 0b00000001) == 0)	{ sample_a = 0; }
	//sample_a += sample_b;
	
	// generate sample C
	sample_c = e_sample;
	volume = psg.c_volume;
	if ((volume & 0b00010000) == 0) { sample_c = AMPLITUDE_4b(volume); }
	if ((output & 0b00000100) == 0)	{ sample_c = 0; }
	//sample_c += sample_b;

	out_l = sample_a;
	out_r = sample_c;
	out_c = sample_b;
}

void avr_write(uint8_t reg, uint8_t data)
{
	if (reg < 16)
	{
		psg.reg[reg] = data;
		switch (reg)
		{
		case 0x00: case 0x01: if (psg.a_period < PSG_C_PER_S) psg.a_period = PSG_C_PER_S; break;
		case 0x02: case 0x03: if (psg.b_period < PSG_C_PER_S) psg.b_period = PSG_C_PER_S; break;
		case 0x04: case 0x05: if (psg.c_period < PSG_C_PER_S) psg.c_period = PSG_C_PER_S; break;
		case 0x06:            if (psg.n_period < PSG_C_PER_S) psg.n_period = PSG_C_PER_S; break;
		case 0x0B: case 0x0C: if (psg.e_period < PSG_C_PER_S) psg.e_period = PSG_C_PER_S; break;
		case 0x0D:
			psg.e_shape = (data & 0x0F);
			state.flags |= 0b00000001;
			break;
		}
	}
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


WAVEFORMATEX fmt;
std::ifstream stream;
int skip;

void init()
{
	stream.open("2-3.psg", std::fstream::binary);
	if (stream) stream.ignore(16); else exit(1);
	skip = 0;

	avr_init();
	::printf("ram: %d", (sizeof(psg) + sizeof(state)));
}

bool play()
{
	if (skip) skip--;
	else
	{
		// skip frame begin marker 0xFF or
		// pending argument of 0xFE command
		if (!stream.ignore(1)) return false;

		// loop through register values
		// until next frame marker
		for (uint8_t v1, v2;;)
		{
			if (stream.peek() == EOF ) break;
			if (stream.peek() == 0xFF) break;
			if (stream.peek() == 0xFE)
			{
				// skip 0xFE command byte
				stream.ignore(1);

				// peek next byte if available
				if (stream.peek() == EOF) return false;
				v2 = uint8_t(stream.peek());

				// setup number of frames to skip
				skip = (v2 * 4 - 1);
				break;
			}

			// read register and value
			stream.get((char&)v1);
			if (!stream.get((char&)v2)) return false;

			// update frame state
			avr_write(v1, v2);
		}
	}
	return true;
}

void fill(uint8_t* data)
{
	if (!play()) exit(1);

	uint8_t out_l, out_r, out_c;
	for (int i = 0; i < BUFFER_SIZE; i += SAMPLE_SIZE)
	{
		avr_update(out_l, out_r, out_c);

		data[i] = int8_t((out_l + out_r + out_c) / 3);
		
	}
}

int main(int argc, char* argv[])
{
	fmt.wFormatTag = WAVE_FORMAT_PCM;
	fmt.nSamplesPerSec = SAMPLE_RATE;
	fmt.nAvgBytesPerSec = (SAMPLE_RATE * SAMPLE_SIZE);
	fmt.wBitsPerSample = (SAMPLE_SIZE * 8);
	fmt.nChannels = PWM_CHANNELS;
	fmt.nBlockAlign = SAMPLE_SIZE;
	fmt.cbSize = 0;

	HWAVEOUT out;
	HRESULT rc = waveOutOpen(&out, WAVE_MAPPER, &fmt, NULL, NULL, CALLBACK_NULL);
	if (rc != MMSYSERR_NOERROR) 
		exit(1);

	WAVEHDR bufs[8] = 
	{
		{ (char*)malloc(BUFFER_SIZE), BUFFER_SIZE },
		{ (char*)malloc(BUFFER_SIZE), BUFFER_SIZE },
		{ (char*)malloc(BUFFER_SIZE), BUFFER_SIZE },
		{ (char*)malloc(BUFFER_SIZE), BUFFER_SIZE },
		{ (char*)malloc(BUFFER_SIZE), BUFFER_SIZE },
		{ (char*)malloc(BUFFER_SIZE), BUFFER_SIZE },
		{ (char*)malloc(BUFFER_SIZE), BUFFER_SIZE },
		{ (char*)malloc(BUFFER_SIZE), BUFFER_SIZE },
	};
	int i = 0;

	init();

	fill((uint8_t*)bufs[i].lpData);
	bufs[i].dwFlags = WHDR_PREPARED;
	waveOutPrepareHeader(out, bufs + i, sizeof(WAVEHDR));
	waveOutWrite(out, bufs + i, sizeof(WAVEHDR));
	i = (i + 1) % 8;

	while (!(GetAsyncKeyState(VK_ESCAPE) & 1))
	{
		fill((uint8_t*)bufs[i].lpData);
		bufs[i].dwFlags = WHDR_PREPARED;
		waveOutPrepareHeader(out, bufs + i, sizeof(WAVEHDR));
		waveOutWrite(out, bufs + i, sizeof(WAVEHDR));
		i = (i + 1) % 8;
		while (waveOutUnprepareHeader(out, bufs + i, sizeof(WAVEHDR)) == WAVERR_STILLPLAYING);
	}
}

#endif
