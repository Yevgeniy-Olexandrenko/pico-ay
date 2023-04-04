#include <stdint.h>
#include <Windows.h>
#include <MMSystem.h>

#undef min
#undef max 

#include <fstream>
#include <algorithm>

#define SIMPLIFIED_COUNTERS	0
#define SIMPLIFIED_NOISE	1
#define SIMPLIFIED_ENVELOPE	1

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

const uint32_t F_CPU        = 8000000;
const uint32_t F_PSG        = 1750000;
const uint8_t  PWM_BITS     = 8;
const uint8_t  PWM_CHANNELS = 1;

const uint8_t  FRAME_RATE   = 50;
const uint32_t SAMPLE_RATE  = (F_CPU / (1 << PWM_BITS));
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
	uint8_t  e_config;
	uint8_t  e_step;    // envelope generator shape counter 
	uint8_t  flags;     // S.P.a.b.c.C.B.A
	                    // | | | | | | | |
	                    // | | *-*-* | | *-> tone A generator waveform
	                    // | |   |   | *---> tone B generator waveform
	                    // | |   |   *-----> tone C generator waveform
	                    // | |   *---------> noise generator waveform
						// | *-------------> noise generator prescale
	                    // *---------------> envelope generator segment
} state;

#pragma pack(pop)

const float max_amp = 170;

const uint8_t amplitude4b[] =
{
	uint8_t(0.0f + max_amp * 0.0056),
	uint8_t(0.5f + max_amp * 0.0079),
	uint8_t(0.5f + max_amp * 0.0112),
	uint8_t(0.5f + max_amp * 0.0158),
	uint8_t(0.5f + max_amp * 0.0224),
	uint8_t(0.5f + max_amp * 0.0316),
	uint8_t(0.5f + max_amp * 0.0447),
	uint8_t(0.5f + max_amp * 0.0631),
	uint8_t(0.5f + max_amp * 0.0891),
	uint8_t(0.5f + max_amp * 0.1259),
	uint8_t(0.5f + max_amp * 0.1778),
	uint8_t(0.5f + max_amp * 0.2512),
	uint8_t(0.5f + max_amp * 0.3548),
	uint8_t(0.5f + max_amp * 0.5012),
	uint8_t(0.5f + max_amp * 0.7079),
	uint8_t(0.5f + max_amp * 1.0000)
};

const uint8_t amplitude5b[] =
{
	uint8_t(0.0f + max_amp * 0.0047),
	uint8_t(0.5f + max_amp * 0.0056),
	uint8_t(0.5f + max_amp * 0.0067),
	uint8_t(0.5f + max_amp * 0.0079),
	uint8_t(0.5f + max_amp * 0.0094),
	uint8_t(0.5f + max_amp * 0.0112),
	uint8_t(0.5f + max_amp * 0.0133),
	uint8_t(0.5f + max_amp * 0.0158),
	uint8_t(0.5f + max_amp * 0.0188),
	uint8_t(0.5f + max_amp * 0.0224),
	uint8_t(0.5f + max_amp * 0.0266),
	uint8_t(0.5f + max_amp * 0.0316),
	uint8_t(0.5f + max_amp * 0.0376),
	uint8_t(0.5f + max_amp * 0.0447),
	uint8_t(0.5f + max_amp * 0.0531),
	uint8_t(0.5f + max_amp * 0.0631),
	uint8_t(0.5f + max_amp * 0.0750),
	uint8_t(0.5f + max_amp * 0.0891),
	uint8_t(0.5f + max_amp * 0.1059),
	uint8_t(0.5f + max_amp * 0.1259),
	uint8_t(0.5f + max_amp * 0.1496),
	uint8_t(0.5f + max_amp * 0.1778),
	uint8_t(0.5f + max_amp * 0.2113),
	uint8_t(0.5f + max_amp * 0.2512),
	uint8_t(0.5f + max_amp * 0.2985),
	uint8_t(0.5f + max_amp * 0.3548),
	uint8_t(0.5f + max_amp * 0.4217),
	uint8_t(0.5f + max_amp * 0.5012),
	uint8_t(0.5f + max_amp * 0.5957),
	uint8_t(0.5f + max_amp * 0.7079),
	uint8_t(0.5f + max_amp * 0.8414),
	uint8_t(0.5f + max_amp * 1.0000)
};

enum { INC = +1, DEC = -1, HLD = 0, TOP = 0x1F, BOT = 0x00 };
const uint8_t envelopes[]
{
	uint8_t(DEC), uint8_t(TOP), uint8_t(HLD), uint8_t(BOT),
	uint8_t(DEC), uint8_t(TOP), uint8_t(HLD), uint8_t(BOT),
	uint8_t(DEC), uint8_t(TOP), uint8_t(HLD), uint8_t(BOT),
	uint8_t(DEC), uint8_t(TOP), uint8_t(HLD), uint8_t(BOT),
	uint8_t(INC), uint8_t(BOT), uint8_t(HLD), uint8_t(BOT),
	uint8_t(INC), uint8_t(BOT), uint8_t(HLD), uint8_t(BOT),
	uint8_t(INC), uint8_t(BOT), uint8_t(HLD), uint8_t(BOT),
	uint8_t(INC), uint8_t(BOT), uint8_t(HLD), uint8_t(BOT),
	uint8_t(DEC), uint8_t(TOP), uint8_t(DEC), uint8_t(TOP),
	uint8_t(DEC), uint8_t(TOP), uint8_t(HLD), uint8_t(BOT),
	uint8_t(DEC), uint8_t(TOP), uint8_t(INC), uint8_t(BOT),
	uint8_t(DEC), uint8_t(TOP), uint8_t(HLD), uint8_t(TOP),
	uint8_t(INC), uint8_t(BOT), uint8_t(INC), uint8_t(BOT),
	uint8_t(INC), uint8_t(BOT), uint8_t(HLD), uint8_t(TOP),
	uint8_t(INC), uint8_t(BOT), uint8_t(DEC), uint8_t(TOP),
	uint8_t(INC), uint8_t(BOT), uint8_t(HLD), uint8_t(BOT)
};

void avr_init()
{
	memset(&psg, 0, sizeof(psg));
	memset(&state, 0, sizeof(state));
	state.flags = 0b11000111;
}

void avr_tone_generator(const uint8_t& step, const uint16_t& period, uint16_t& counter, const uint8_t& mask, uint8_t& flags)
{
#if SIMPLIFIED_COUNTERS
	if ((int16_t)counter <= 0)
	{
		if (period < step)
		{
			// special case, no generation
			counter = 0;
			flags |= mask;
		}
		else
		{
			// reload counter, toggle flip-flop
			counter += period;
			flags ^= mask;
		}
	}
	counter -= step;
#else
	if (counter >= period)
	{
		// reload counter
		counter -= period;
		if (counter >= step)
		{
			counter = 0;
		}

		// toggle flip-flop
		if (period >= step)
			flags ^= mask;
		else
			flags |= mask;
	}
	counter += step;
#endif
}

void avr_envelope_generator(const uint8_t& step, const uint16_t& period, uint16_t& counter, uint8_t& config, uint8_t& e_stp)
{
#if SIMPLIFIED_ENVELOPE
	static bool flag = false;
	uint8_t _step = (flag ^= true) ? (step >> 1) : step - (step >> 1);

	for (int i = 0; i < _step; ++i)
	{
		if (counter >= period)
		{
			// reload counter
			counter = 0;

			// update cycle step
			e_stp += envelopes[config];
			if (e_stp > 0x0F)
			{
				config ^= 0b00000010;
				e_stp = (envelopes[config | 1] >> 1);
			}
		}
		counter++;
	}
#else
	for (int i = 0; i < step; ++i)
	{
		if (counter >= period)
		{
			// reload counter
			counter = 0;

			// update cycle step
			e_stp += envelopes[config];
			if (e_stp > 0x1F)
			{
				config ^= 0b00000010;
				e_stp = envelopes[config | 1];
			}
		}
		counter++;
	}
#endif
}

void avr_noise_generator(const uint8_t& step, const uint8_t& period, uint8_t& counter, uint16_t& shift, uint8_t& flags)
{
#if SIMPLIFIED_NOISE
	static bool flag = false;
	uint8_t _step = (flag ^= true) ? (step >> 1) : step - (step >> 1);

	for (int i = 0; i < _step; ++i)
	{
		if (counter >= period)
		{
			// reload counter
			counter = 0;

			// update shifter
			auto b16 = uint8_t((shift ^ (shift >> 3)) << 7);
			shift >>= 1;
			shift |= ((flags & 0b10000000) << 8);
			flags &= 0b01111111;
			flags |= b16;

			// toggle flip-flop
			flags &= 0b11000111;
			if (shift & 0x0001) flags |= 0b00111000;
		}
		counter++;
	}
#else
	for (int i = 0; i < step; ++i)
	{
		uint8_t period_x2 = (period << 1);
		if (counter >= period_x2)
		{
			// reload counter
			counter = 0;

			// update shifter
			auto b16 = uint8_t((shift ^ (shift >> 3)) << 7);
			shift >>= 1;
			shift |= ((flags & 0b10000000) << 8);
			flags &= 0b01111111;
			flags |= b16;

			// toggle flip-flop
			flags &= 0b11000111;
			if (shift & 0x0001) flags |= 0b00111000;
		}
		counter++;
	}
#endif
}

void avr_sample_generator(const uint8_t& e_sample, const uint8_t& volume, const uint8_t& flags, const uint8_t& mask, uint8_t& sample)
{
	sample = e_sample;
	if ((volume & 0x10) == 0) { sample = amplitude4b[volume]; }
	if ((flags  & mask) == 0) { sample = 0; }
}

void avr_update(uint8_t& out_l, uint8_t& out_r)
{
	if (state.flags & 0b01000000)
	{
		state.flags &= 0b10111111;
		state.e_config = psg.e_shape << 2;
		state.e_step = envelopes[state.e_config | 1];
		state.e_counter = 0;
	}

	avr_tone_generator(PSG_C_PER_S, psg.a_period, state.a_counter, 0b00000001, state.flags);
	avr_tone_generator(PSG_C_PER_S, psg.b_period, state.b_counter, 0b00000010, state.flags);
	avr_tone_generator(PSG_C_PER_S, psg.c_period, state.c_counter, 0b00000100, state.flags);

	avr_noise_generator(PSG_C_PER_S, psg.n_period, state.n_counter, state.n_shift, state.flags);
	avr_envelope_generator(PSG_C_PER_S, psg.e_period, state.e_counter, state.e_config, state.e_step);

	uint8_t output = (state.flags | psg.mixer);
	output &= output >> 3;

	uint8_t a_sample, b_sample, c_sample;
#if SIMPLIFIED_ENVELOPE
	uint8_t e_sample = amplitude4b[state.e_step];
#else
	uint8_t e_sample = amplitude5b[state.e_step];
#endif
	avr_sample_generator(e_sample, psg.a_volume, output, 0b00000001, a_sample);
	avr_sample_generator(e_sample, psg.b_volume, output, 0b00000010, b_sample);
	avr_sample_generator(e_sample, psg.c_volume, output, 0b00000100, c_sample);
	b_sample >>= 1;

	out_l = a_sample + b_sample;
	out_r = c_sample + b_sample;
}

void avr_write(uint8_t reg, uint8_t data)
{
	if (reg < 14)
	{
		psg.reg[reg] = data;
		if (reg == 0x0D)
		{
			psg.e_shape = (data & 0x0F);
			state.flags |= 0b01000000;
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
	stream.open("sample.psg", std::fstream::binary);
	if (stream) stream.ignore(16); else exit(1);
	skip = 0;

	avr_init();
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

	uint8_t out_l, out_r;
	for (int i = 0; i < BUFFER_SIZE; i += SAMPLE_SIZE)
	{
		avr_update(out_l, out_r);

		data[i] = ((out_l + out_r) / 2);
		
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
