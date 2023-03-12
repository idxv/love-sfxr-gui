-- sfxr.lua
-- original by Tomas Pettersson, ported to Lua by nucular

--[[
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
]]--

--[[--
A port of the sfxr sound effect synthesizer to pure Lua, designed to be used
together with the *awesome* [LÖVE](https://love2d.org) game framework.
]]--
-- @module sfxr
local sfxr = {}
local bit = bit32 or require("bit")

local strunpack, strpack
if love then
	strunpack = love.data.unpack
	strpack = function(fmt, ...) return love.data.pack("string", fmt, ...) end
else strunpack, strpack = string.unpack, string.pack end

-- Constants

--- The module version (SemVer format)
-- @within Constants
sfxr.VERSION = "0.0.2"

--- [Waveform](https://en.wikipedia.org/wiki/Waveform) constants
-- @within Constants
-- @field SQUARE [square wave](https://en.wikipedia.org/wiki/Square_wave) (`= 0`)
-- @field SAWTOOTH [sawtooth wave](https://en.wikipedia.org/wiki/Sawtooth_wave) (`= 1`)
-- @field SINE [sine wave](https://en.wikipedia.org/wiki/Sine_wave) (`= 2`)
-- @field NOISE [white noise](https://en.wikipedia.org/wiki/White_noise) (`= 3`)
sfxr.WAVEFORM = {
	SQUARE = 0,
	[0] = 0,
	SAWTOOTH = 1,
	[1] = 1,
	SINE = 2,
	[2] = 2,
	NOISE = 3,
	[3] = 3
}

--- [Sampling rate](https://en.wikipedia.org/wiki/Sampling_(signal_processing)#Sampling_rate) constants
-- (use the number values directly, these are just for lookup)
-- @within Constants
-- @field 22050 22.05 kHz (`= 22050`)
-- @field 44100 44.1 kHz (`= 44100`)
sfxr.SAMPLERATE = {
  [22050] = 22050, --- 22.05 kHz
  [44100] = 44100 --- 44.1 kHz
}

--- [Bit depth](https://en.wikipedia.org/wiki/Audio_bit_depth) constants
-- (use the number values directly, these are just for lookup)
-- @within Constants
-- @field 0 floating point bit depth, -1 to 1 (`= 0`)
-- @field 8 unsigned 8 bit, 0x00 to 0xFF (`= 8`)
-- @field 16 unsigned 16 bit, 0x0000 to 0xFFFF (`= 16`)
sfxr.BITDEPTH = {
	[16] = 16,
	[8] = 8
}

sfxr.VOLUME = 0.5
sfxr.SUPERSAMPLING = 8

-- Utilities

local floor, ceil = math.floor, math.ceil
local min, max = math.min, math.max
local sin = math.sin
local abs = math.abs

local INF = math.huge
local PI = math.pi

--- Truncate a number to an unsigned integer.
-- @tparam number n a (signed) number
-- @treturn int the number, truncated and unsigned
local function trunc(n)
	return n >= 0 and floor(n) or -floor(-n)
end

--- Set the random seed and initializes the generator.
-- @tparam number seed the random seed
local function setseed(seed)
	math.randomseed(seed)
	for i=0, 5 do
		math.random()
	end
end

--- Return a random number between low and high.
-- @tparam number low the lower bound
-- @tparam number high the upper bound
-- @treturn number a random number where `low < n < high`
local function random(low, high)
	return low + math.random() * (high - low)
end

--- Return a random boolean weighted towards false by n.
-- w = 1: uniform distribution
-- w = n: false is n times as likely as true
-- Note: n < 0 do not work, use `not maybe(w)` instead
-- @tparam[opt=1] number w the weight towards false
-- @treturn bool a random boolean
local function maybe(w)
	return trunc(random(0, w or 1)) == 0
end

--- Clamp x between a and b.
-- @tparam number x the number
-- @tparam number a the lower bound
-- @tparam number b the upper bound
-- @treturn number the number where `a <= x <= b`
local function clamp(x, a, b)
	return min(max(x, a), b)
end

--- Copy a table (shallow) or a primitive.
-- @param t a table or primitive
-- @return a copy of t
local function shallowcopy(t)
	if type(t) == "table" then
		local t2 = {}
		for k,v in pairs(t) do
			t2[k] = v
		end
		return t2
	else
		return t
	end
end

--- Recursively merge table t2 into t1.
-- @tparam tab t1 a table
-- @tparam tab t2 a table to merge into t1
-- @treturn tab t1
local function mergetables(t1, t2)
	for k, v in pairs(t2) do
		if type(v) == "table" then
			if type(t1[k] or false) == "table" then
				mergetables(t1[k] or {}, t2[k] or {})
			else
				t1[k] = v
			end
		else
			t1[k] = v
		end
	end
	return t1
end

--- Construct and return a new @{Sound} instance.
-- @treturn Sound a Sound instance
function sfxr.newSound(...)
	local instance = setmetatable({}, sfxr.Sound)
	instance:__init(...)
	return instance
end

--- The main Sound class.
-- @type Sound
sfxr.Sound = {}
sfxr.Sound.__index = sfxr.Sound

--- Initialize the Sound instance.
-- Called by @{sfxr.newSound|the constructor}.
function sfxr.Sound:__init()
	self:resetParameters()
end

--- Set all parameters to their default values.
-- Called by @{sfxr.Sound:__init|the initializer}.
function sfxr.Sound:resetParameters()
	--- Repeat speed:
	-- Times to repeat the frequency slide over the course of the envelope
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Parameters
	self.repeatspeed = 0.0
	
	--- Additional gain (*default* 0.5)
	-- @within Volume
	self.soundvolume = 0.5
	
	--- The base @{WAVEFORM|waveform} (*default* @{WAVEFORM|SQUARE})
	-- @within Parameters
	self.waveform = sfxr.WAVEFORM.SQUARE

	--- Attack time:
	-- Time the sound takes to reach its peak amplitude
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Envelope
	self.envelope_attack = 0.0
	--- Sustain time:
	-- Time the sound stays on its peak amplitude
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Envelope
	self.envelope_sustain = 0.3
	--- Sustain punch:
	-- Amount by which the sound peak amplitude is increased at the start of the
	-- sustain time
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Envelope
	self.envelope_punch = 0.0
	--- Decay time:
	-- Time the sound takes to decay after its sustain time
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Envelope
	self.envelope_decay = 0.4

	--- Start frequency:
	-- Base tone of the sound, before sliding
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Frequency
	self.frequency_start = 0.3
	--- Min frequency:
	-- Tone below which the sound will get cut off
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Frequency
	self.frequency_min = 0.0
	--- Slide:
	-- Amount by which the frequency is increased or decreased over time
	-- (*default* 0.0, *min* -1.0, *max* 1.0)
	-- @within Frequency
	self.frequency_slide = 0.0
	--- Delta slide:
	-- Amount by which the slide is increased or decreased over time
	-- (*default* 0.0, *min* -1.0, *max* 1.0)
	-- @within Frequency
	self.frequency_dslide = 0.0

	--- Vibrato depth:
	-- Amount of amplitude modulation
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Vibrato
	self.vibrato_depth = 0.0
	--- Vibrato speed:
	-- Oscillation speed of the vibrato
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Vibrato
	self.vibrato_speed = 0.0
	--- Vibrato delay:
	-- Unused and unimplemented
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Vibrato
	self.vibrato_delay = 0.0

	--- Change amount:
	-- Amount by which the frequency is changed mid-sound
	-- (*default* 0.0, *min* -1.0, *max* 1.0)
	-- @within Change
	self.change_amount = 0.0
	--- Change speed:
	-- Time before the frequency change happens
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Change
	self.change_speed = 0.0

	--- Square duty:
	-- Width of the square wave pulse cycle (doesn't affect other waveforms)
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Duty
	self.duty_ratio = 0.0
	--- Duty sweep:
	-- Amount by which the square duty is increased or decreased over time
	-- (*default* 0.0, *min* -1.0, *max* 1.0)
	-- @within Duty
	self.duty_sweep = 0.0

	--- Phaser offset:
	-- Amount by which the phaser signal is offset from the sound
	-- (*default* 0.0, *min* -1.0, *max* 1.0)
	-- @within Phaser
	self.phaser_offset = 0.0
	--- Phaser sweep:
	-- Amount by which the phaser offset is increased or decreased over time
	-- (*default* 0.0, *min* -1.0, *max* 1.0)
	-- @within Phaser
	self.phaser_sweep = 0.0

	--- Lowpass filter cutoff:
	-- Lower bound for frequencies allowed to pass through this filter
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Lowpass
	self.lowpass_cutoff = 1.0
	--- Lowpass filter cutoff sweep:
	-- Amount by which the LP filter cutoff is increased or decreased
	-- over time
	-- (*default* 0.0, *min* -1.0, *max* 1.0)
	-- @within Lowpass
	self.lowpass_sweep = 0.0
	--- Lowpass filter resonance:
	-- Amount by which certain resonant frequencies near the cutoff are
	-- increased
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Lowpass
	self.lowpass_resonance = 0.0
	--- Highpass filter cutoff:
	-- Upper bound for frequencies allowed to pass through this filter
	-- (*default* 0.0, *min* 0.0, *max* 1.0)
	-- @within Highpass
	self.highpass_cutoff = 0.0
	--- Highpass filter cutoff sweep:
	-- Amount by which the HP filter cutoff is increased or decreased
	-- over time
	-- (*default* 0.0, *min* -1.0, *max* 1.0)
	-- @within Highpass
	self.highpass_sweep = 0.0
end

--- Clamp all parameters within their sane ranges.
function sfxr.Sound:sanitizeParameters()
	self.repeatspeed = clamp(self.repeatspeed, 0, 1)
	self.soundvolume = clamp(self.soundvolume, 0, 1)
	self.waveform = clamp(self.waveform, 0, #sfxr.WAVEFORM)

	self.envelope_attack = clamp(self.envelope_attack, 0, 1)
	self.envelope_sustain = clamp(self.envelope_sustain, 0, 1)
	self.envelope_punch = clamp(self.envelope_punch, 0, 1)
	self.envelope_decay = clamp(self.envelope_decay, 0, 1)

	self.frequency_start = clamp(self.frequency_start, 0, 1)
	self.frequency_min = clamp(self.frequency_min, 0, 1)
	self.frequency_slide = clamp(self.frequency_slide, -1, 1)
	self.frequency_dslide = clamp(self.frequency_dslide, -1, 1)

	self.vibrato_depth = clamp(self.vibrato_depth, 0, 1)
	self.vibrato_speed = clamp(self.vibrato_speed, 0, 1)
	self.vibrato_delay = clamp(self.vibrato_delay, 0, 1)

	self.change_amount = clamp(self.change_amount, -1, 1)
	self.change_speed = clamp(self.change_speed, 0, 1)

	self.duty_ratio = clamp(self.duty_ratio, 0, 1)
	self.duty_sweep = clamp(self.duty_sweep, -1, 1)

	self.phaser_offset = clamp(self.phaser_offset, -1, 1)
	self.phaser_sweep = clamp(self.phaser_sweep, -1, 1)

	self.lowpass_cutoff = clamp(self.lowpass_cutoff, 0, 1)
	self.lowpass_sweep = clamp(self.lowpass_sweep, -1, 1)
	self.lowpass_resonance = clamp(self.lowpass_resonance, 0, 1)
	self.highpass_cutoff = clamp(self.highpass_cutoff, 0, 1)
	self.highpass_sweep = clamp(self.highpass_sweep, -1, 1)
end

--- Generate the sound and yield the sample data.
-- @treturn function() a generator that yields the next sample when called
-- @usage for s in sound:generate() do
--   -- do something with s
-- end
function sfxr.Sound:generate()
	-- Initialize all locals
	local fperiod, maxperiod, period
	local slide, dslide
	local square_duty, square_slide
	local chg_mod, chg_time, chg_limit

	local phaserbuffer = {}
	local noisebuffer = {}

	-- Initialize the sample buffers
	for i=1, 1024 do
		phaserbuffer[i] = 0
	end

	for i=1, 32 do
		noisebuffer[i] = random(-1, 1)
	end

	--- Reset the sound period
	local function reset()
		fperiod = 100 / (self.frequency_start^2 + 0.001)
		maxperiod = 100 / (self.frequency_min^2 + 0.001)
		period = trunc(fperiod)

		slide = 1.0 - self.frequency_slide^3 * 0.01
		dslide = -self.frequency_dslide^3 * 0.000001

		square_duty = 0.5 - self.duty_ratio * 0.5
		square_slide = -self.duty_sweep * 0.00005

		if self.change_amount >= 0 then
			chg_mod = 1.0 - self.change_amount^2 * 0.9
		else
			chg_mod = 1.0 + self.change_amount^2 * 10
		end

		chg_time = 0
		if self.change_speed == 1 then
			chg_limit = 0
		else
			chg_limit = trunc((1 - self.change_speed)^2 * 20000 + 32)
		end
	end

	local phase = 0
	reset()

	local second_sample = false

	local env_vol = 0
	local env_stage = 1
	local env_time = 0
	local env_length = {self.envelope_attack^2 * 100000,
		self.envelope_sustain^2 * 100000,
	self.envelope_decay^2 * 100000}

	local fphase = self.phaser_offset^2 * 1020
	if self.phaser_offset < 0 then fphase = -fphase end
	local dphase = self.phaser_sweep^2
	if self.phaser_sweep < 0 then dphase = -dphase end
	local ipp = 0

	local iphase = abs(trunc(fphase))

	local fltp = 0
	local fltdp = 0
	local fltw = self.lowpass_cutoff^3 * 0.1
	local fltw_d = 1 + self.lowpass_sweep * 0.0001
	local fltdmp = 5 / (1 + self.lowpass_resonance^2 * 20) * (0.01 + fltw)
	fltdmp = clamp(fltdmp, -INF, 0.8)
	local fltphp = 0
	local flthp = self.highpass_cutoff^2 * 0.1
	local flthp_d = 1 + self.highpass_sweep * 0.0003

	local vib_phase = 0
	local vib_speed = self.vibrato_speed^2 * 0.01
	local vib_amp = self.vibrato_depth * 0.5

	local rep_time = 0
	local rep_limit = trunc((1 - self.repeatspeed)^2 * 20000 + 32)
	if self.repeatspeed == 0 then
		rep_limit = 0
	end

	-- The main closure (returned as a generator)

	local function next()
		-- Repeat when needed
		rep_time = rep_time + 1
		if rep_limit ~= 0 and rep_time >= rep_limit then
			rep_time = 0
			reset()
		end

		-- Update the change time and apply it if needed
		chg_time = chg_time + 1
		if chg_limit ~= 0 and chg_time >= chg_limit then
			chg_limit = 0
			fperiod = fperiod * chg_mod
		end

		-- Apply the frequency slide and stuff
		slide = slide + dslide
		fperiod = fperiod * slide

		if fperiod > maxperiod then
			fperiod = maxperiod
			-- Fail if the minimum frequency is too small
			if (self.frequency_min > 0) then
				return nil
			end
		end

		-- Vibrato
		local rfperiod = fperiod
		if vib_amp > 0 then
			vib_phase = vib_phase + vib_speed
			-- Apply to the frequency period
			rfperiod = fperiod * (1.0 + sin(vib_phase) * vib_amp)
		end

		-- Update the period
		period = trunc(rfperiod)
		if (period < 8) then period = 8 end

		-- Update the square duty
		square_duty = clamp(square_duty + square_slide, 0, 0.5)

		-- Volume envelopes

		env_time = env_time + 1

		if env_time > env_length[env_stage] then
			env_time = 0
			env_stage = env_stage + 1
			-- After the decay stop generating
			if env_stage == 4 then
				return nil
			end
		end

		-- Attack, Sustain, Decay/Release
		local env_len = env_length[env_stage]
		env_vol = env_time / env_len
		
		if env_vol == env_vol then
			if env_stage == 2 then
				env_vol = 1 + (1 - env_vol)^1 * 2 * self.envelope_punch
			elseif env_stage == 3 then
				env_vol = 1 - env_vol
			end
		else
			env_vol = 0
			--print("!!!")
		end

		-- Phaser

		fphase = fphase + dphase
		iphase = clamp(abs(trunc(fphase)), -INF, 1023)

		-- Filter stuff

		if flthp_d ~= 0 then
			flthp = clamp(flthp * flthp_d, 0.00001, 0.1)
		end

		-- And finally the actual tone generation and supersampling

		local ssample = 0
		for si = 0, sfxr.SUPERSAMPLING - 1 do
			local sample = 0

			phase = phase + 1

			-- fill the noise buffer every period
			if phase >= period then
				--phase = 0
				phase = phase % period
				if self.waveform == sfxr.WAVEFORM.NOISE then
					for i = 1, 32 do
						noisebuffer[i] = random(-1, 1)
					end
				end
			end

			-- Tone generators ahead

			local fp = phase / period

			-- Square, including square duty
			if self.waveform == sfxr.WAVEFORM.SQUARE then
				if fp < square_duty then
					sample = 0.5
				else
					sample = -0.5
				end

			-- Sawtooth
		elseif self.waveform == sfxr.WAVEFORM.SAWTOOTH then
			sample = 1 - fp * 2

			-- Sine
		elseif self.waveform == sfxr.WAVEFORM.SINE then
			sample = sin(fp * 2 * PI)

			-- Pitched white noise
		elseif self.waveform == sfxr.WAVEFORM.NOISE then
			sample = noisebuffer[trunc(phase * 32 / period) % 32 + 1]
		end

			-- Apply the lowpass filter to the sample

			local pp = fltp
			fltw = clamp(fltw * fltw_d, 0, 0.1)
			if self.lowpass_cutoff ~= 1 then
				fltdp = fltdp + (sample - fltp) * fltw
				fltdp = fltdp - fltdp * fltdmp
			else
				fltp = sample
				fltdp = 0
			end
			fltp = fltp + fltdp

			-- Apply the highpass filter to the sample

			fltphp = fltphp + (fltp - pp)
			fltphp = fltphp - (fltphp * flthp)
			sample = fltphp

			-- Apply the phaser to the sample

			phaserbuffer[bit.band(ipp, 1023) + 1] = sample
			sample = sample + phaserbuffer[bit.band(ipp - iphase + 1024, 1023) + 1]
			ipp = bit.band(ipp + 1, 1023)

			-- Accumulation and envelope application
			ssample = ssample + sample * env_vol
		end

		-- Apply the volumes
		ssample = (ssample / sfxr.SUPERSAMPLING) * sfxr.VOLUME
		ssample = ssample * (2 * self.soundvolume)

		-- Hard limit
		return clamp(ssample, -1, 1)
	end

	return next
end

--- Get the maximum sample limit allowed by the current envelope_
-- Does not take any other limits into account, so the returned count might be
-- higher than samples actually generated. Still useful though.
-- @tparam[opt=44100] SAMPLERATE rate the sampling rate
function sfxr.Sound:getEnvelopeLimit(rate)
	rate = rate or 44100

	local limit = trunc(
		self.envelope_attack^2 * 100000
		+ self.envelope_sustain^2 * 100000
		+ self.envelope_decay^2 * 100000
		+ 2
		)
	
	return ceil(limit / (rate / 44100))
end

--- Generate the sound into a table.
-- @tparam[opt] {} tab the table to synthesize into
-- @treturn {number,...} the table filled with sample data
-- @treturn int the number of written samples (== #tab)
function sfxr.Sound:generateTable(tab)
	-- this could really use table pre-allocation, but Lua doesn't provide that
	local t = tab or {}
	local i = 1
	for v in self:generate() do
		t[i] = v
		i = i + 1
	end
	return t, i
end

--- Randomize all sound parameters
-- @within Randomization
-- @tparam[opt] number seed a random seed
function sfxr.Sound:randomize(seed)
	if seed then setseed(seed) end

	local waveform = self.waveform
	self:resetParameters()
	self.waveform = waveform

	if maybe() then
		self.repeatspeed = random(0, 1)
	end

	if maybe() then
		self.frequency_start = random(-1, 1)^3 + 0.5
	else
		self.frequency_start = random(-1, 1)^2
	end
	self.frequency_limit = 0
	self.frequency_slide = random(-1, 1)^5
	if self.frequency_start > 0.7 and self.frequency_slide > 0.2 then
		self.frequency_slide = -self.frequency_slide
	elseif self.frequency_start < 0.2 and self.frequency_slide <-0.05 then
		self.frequency_slide = -self.frequency_slide
	end
	self.frequency_dslide = random(-1, 1)^3

	self.duty_ratio = random(-1, 1)
	self.duty_sweep = random(-1, 1)^3

	self.vibrato_depth = random(-1, 1)^3
	self.vibrato_speed = random(-1, 1)
	self.vibrato_delay = random(-1, 1)

	self.envelope_attack = random(-1, 1)^3
	self.envelope_sustain = random(-1, 1)^2
	self.envelope_punch = random(-1, 1)^2
	self.envelope_decay = random(-1, 1)

	if self.envelope_attack + self.envelope_sustain + self.envelope_decay < 0.2 then
		self.envelope_sustain = self.envelope_sustain + 0.2 + random(0, 0.3)
		self.envelope_decay = self.envelope_decay + 0.2 + random(0, 0.3)
	end

	self.lowpass_resonance = random(-1, 1)
	self.lowpass_cutoff = 1 - random(0, 1)^3
	self.lowpass_sweep = random(-1, 1)^3
	if self.lowpass_cutoff < 0.1 and self.lowpass_sweep < -0.05 then
		self.lowpass_sweep = -self.lowpass_sweep
	end
	self.highpass_cutoff = random(0, 1)^3
	self.highpass_sweep = random(-1, 1)^5

	self.phaser_offset = random(-1, 1)^3
	self.phaser_sweep = random(-1, 1)^3

	self.change_speed = random(-1, 1)
	self.change_amount = random(-1, 1)

	self:sanitizeParameters()
end

--- Mutate all sound parameters
-- @within Randomization
-- @tparam[opt=1] number amount by how much to mutate the parameters
-- @tparam[opt] number seed a random seed
-- @tparam[changefreq=true] bool changefreq whether to change the frequency parameters
function sfxr.Sound:mutate(amount, seed, changefreq)
	if seed then setseed(seed) end
	local amount = (amount or 1)
	local a = amount / 20
	local b = (1 - a) * 10
	local changefreq = (changefreq == nil) and true or changefreq

	if changefreq == true then
		if maybe(b) then self.frequency_start = self.frequency_start + random(-a, a) end
		if maybe(b) then self.frequency_slide = self.frequency_slide + random(-a, a) end
		if maybe(b) then self.frequency_dslide = self.frequency_dslide + random(-a, a) end
	end

	if maybe(b) then self.duty_ratio = self.duty_ratio + random(-a, a) end
	if maybe(b) then self.duty_sweep = self.duty_sweep + random(-a, a) end

	if maybe(b) then self.vibrato_depth = self.vibrato_depth + random(-a, a) end
	if maybe(b) then self.vibrato_speed = self.vibrato_speed + random(-a, a) end
	if maybe(b) then self.vibrato_delay = self.vibrato_delay + random(-a, a) end

	if maybe(b) then self.envelope_attack = self.envelope_attack + random(-a, a) end
	if maybe(b) then self.envelope_sustain = self.envelope_sustain + random(-a, a) end
	if maybe(b) then self.envelope_punch = self.envelope_punch + random(-a, a) end
	if maybe(b) then self.envelope_decay = self.envelope_decay + random(-a, a) end

	if maybe(b) then self.lowpass_resonance = self.lowpass_resonance + random(-a, a) end
	if maybe(b) then self.lowpass_cutoff = self.lowpass_cutoff + random(-a, a) end
	if maybe(b) then self.lowpass_sweep = self.lowpass_sweep + random(-a, a) end
	if maybe(b) then self.highpass_cutoff = self.highpass_cutoff + random(-a, a) end
	if maybe(b) then self.highpass_sweep = self.highpass_sweep + random(-a, a) end

	if maybe(b) then self.phaser_offset = self.phaser_offset + random(-a, a) end
	if maybe(b) then self.phaser_sweep = self.phaser_sweep + random(-a, a) end

	if maybe(b) then self.change_speed = self.change_speed + random(-a, a) end
	if maybe(b) then self.change_amount = self.change_amount + random(-a, a) end

	if maybe(b) then self.repeatspeed = self.repeatspeed + random(-a, a) end

	self:sanitizeParameters()
end

--- Randomize all sound parameters to generate a "pick up" sound
-- @within Randomization
-- @tparam[opt] number seed a random seed
function sfxr.Sound:randomPickup(seed)
	if seed then setseed(seed) end
	self:resetParameters()
	self.frequency_start = random(0.4, 0.9)
	self.envelope_attack = 0
	self.envelope_sustain = random(0, 0.1)
	self.envelope_punch = random(0.3, 0.6)
	self.envelope_decay = random(0.1, 0.5)

	if maybe() then
		self.change_speed = random(0.5, 0.7)
		self.change_amount = random(0.2, 0.6)
	end
end

--- Randomize all sound parameters to generate a laser sound
-- @within Randomization
-- @tparam[opt] number seed a random seed
function sfxr.Sound:randomLaser(seed)
	if seed then setseed(seed) end
	self:resetParameters()
	self.waveform = trunc(random(0, 3))
	if self.waveform == sfxr.WAVEFORM.SINE and maybe() then
		self.waveform = trunc(random(0, 1))
	end

	if maybe(2) then
		self.frequency_start = random(0.3, 0.9)
		self.frequency_min = random(0, 0.1)
		self.frequency_slide = random(-0.65, -0.35)
	else
		self.frequency_start = random(0.5, 1)
		self.frequency_min = clamp(self.frequency_start - random(0.2, 0.4), 0.2, INF)
		self.frequency_slide = random(-0.35, -0.15)
	end

	if maybe() then
		self.duty_ratio = random(0, 0.5)
		self.duty_sweep = random(0, 0.2)
	else
		self.duty_ratio = random(0.4, 0.9)
		self.duty_sweep = random(-0.7, 0)
	end

	self.envelope_attack = 0
	self.envelope_sustain = random(0.1, 0.3)
	self.envelope_decay = random(0, 0.4)

	if maybe() then
		self.envelope_punch = random(0, 0.3)
	end

	if maybe(2) then
		self.phaser_offset = random(0, 0.2)
		self.phaser_sweep = random(-0.2, 0)
	end

	if maybe() then
		self.highpass_cutoff = random(0, 0.3)
	end
end

--- Randomize all sound parameters to generate an explosion sound
-- @within Randomization
-- @tparam[opt] number seed a random seed
function sfxr.Sound:randomExplosion(seed)
	if seed then setseed(seed) end
	self:resetParameters()
	self.waveform = sfxr.WAVEFORM.NOISE

	if maybe() then
		self.frequency_start = random(0.1, 0.5)
		self.frequency_slide = random(-0.1, 0.3)
	else
		self.frequency_start = random(0.2, 0.9)
		self.frequency_slide = random(-0.2, -0.4)
	end
	self.frequency_start = self.frequency_start^2

	if maybe(4) then
		self.frequency_slide = 0
	end
	if maybe(2) then
		self.repeatspeed = random(0.3, 0.8)
	end

	self.envelope_attack = 0
	self.envelope_sustain = random(0.1, 0.4)
	self.envelope_punch = random(0.2, 0.8)
	self.envelope_decay = random(0, 0.5)

	if maybe() then
		self.phaser_offset = random(-0.3, 0.6)
		self.phaser_sweep = random(-0.3, 0)
	end
	if maybe() then
		self.vibrato_depth = random(0, 0.7)
		self.vibrato_speed = random(0, 0.6)
	end
	if maybe(2) then
		self.change_speed = random(0.6, 0.9)
		self.change_amount = random(-0.8, 0.8)
	end
end

--- Randomize all sound parameters to generate a "power up" sound
-- @within Randomization
-- @tparam[opt] number seed a random seed
function sfxr.Sound:randomPowerup(seed)
	if seed then setseed(seed) end
	self:resetParameters()
	if maybe() then
		self.waveform = sfxr.WAVEFORM.SAWTOOTH
	else
		self.duty_ratio = random(0, 0.6)
	end

	if maybe() then
		self.frequency_start = random(0.2, 0.5)
		self.frequency_slide = random(0.1, 0.5)
		self.repeatspeed = random(0.4, 0.8)
	else
		self.frequency_start = random(0.2, 0.5)
		self.frequency_slide = random(0.05, 0.25)
		if maybe() then
			self.vibrato_depth = random(0, 0.7)
			self.vibrato_speed = random(0, 0.6)
		end
	end
	self.envelope_attack = 0
	self.envelope_sustain = random(0, 0.4)
	self.envelope_decay = random(0.1, 0.5)
end

--- Randomize all sound parameters to generate a hit sound
-- @within Randomization
-- @tparam[opt] number seed a random seed
function sfxr.Sound:randomHit(seed)
	if seed then setseed(seed) end
	self:resetParameters()
	self.waveform = trunc(random(0, 3))

	if self.waveform == sfxr.WAVEFORM.SINE then
		self.waveform = sfxr.WAVEFORM.NOISE
	elseif self.waveform == sfxr.WAVEFORM.SQUARE then
		self.duty_ratio = random(0, 0.6)
	end

	self.frequency_start = random(0.2, 0.8)
	self.frequency_slide = random(-0.7, -0.3)
	self.envelope_attack = 0
	self.envelope_sustain = random(0, 0.1)
	self.envelope_decay = random(0.1, 0.3)

	if maybe() then
		self.highpass_cutoff = random(0, 0.3)
	end
end

--- Randomize all sound parameters to generate a jump sound
-- @within Randomization
-- @tparam[opt] number seed a random seed
function sfxr.Sound:randomJump(seed)
	if seed then setseed(seed) end
	self:resetParameters()
	self.waveform = sfxr.WAVEFORM.SQUARE

	self.duty_value = random(0, 0.6)
	self.frequency_start = random(0.3, 0.6)
	self.frequency_slide = random(0.1, 0.3)

	self.envelope_attack = 0
	self.envelope_sustain = random(0.1, 0.4)
	self.envelope_decay = random(0.1, 0.3)

	if maybe() then
		self.highpass_cutoff = random(0, 0.3)
	end
	if maybe() then
		self.lowpass_cutoff = random(0.4, 1)
	end
end

--- Randomize all sound parameters to generate a "blip" sound
-- @within Randomization
-- @tparam[opt] number seed a random seed
function sfxr.Sound:randomBlip(seed)
	if seed then setseed(seed) end
	self:resetParameters()
	self.waveform = trunc(random(0, 2))

	if self.waveform == sfxr.WAVEFORM.SQUARE then
		self.duty_ratio = random(0, 0.6)
	end

	self.frequency_start = random(0.2, 0.6)
	self.envelope_attack = 0
	self.envelope_sustain = random(0.1, 0.2)
	self.envelope_decay = random(0, 0.2)
	self.highpass_cutoff = 0.1
end

--- Generate and export the audio data to a PCM WAVE file.
-- @within Serialization
-- @tparam ?string|file|love.filesystem.File f a path or file in `wb`-mode
-- (passed files will not be closed)
-- @tparam[opt=44100] SAMPLERATE rate the sampling rate
-- @tparam[opt=0] BITDEPTH depth the bit depth
-- @raise "invalid sampling rate: x", "invalid bit depth: x"
function sfxr.Sound:exportWAV(rate, depth)
	local rate = rate or 44100
	local depth = depth or 16
	assert(sfxr.SAMPLERATE[rate], "invalid sampling rate: " .. tostring(rate))
	assert(sfxr.BITDEPTH[depth], "invalid bit depth: " .. tostring(depth))

	local content = {}
	local function pack(fmt, ...)
		table.insert(content, select("#", ...) == 0 and fmt or (strpack(fmt, ...)))
	end

	-- These will hold important file positions
	local pos_fsize
	local pos_csize

	-- Start the file by writing the RIFF header
	pack("RIFF")
	pack("") -- remaining file size, will be replaced later
	pos_fsize = #content

	pack("WAVE") -- type

	-- Write the format chunk
	pack("fmt ")
	pack("<i4i2i2i4i4i2i2",
		16, -- chunk size
		1, -- compression code (1 = PCM)
		1, -- channel number
		rate, -- sampling rate
		rate * depth / 8, -- bytes per second
		depth / 8, -- block alignment
		depth -- bits per sample
	)

	-- Write the header of the data chunk
	pack("data")
	pack("") -- chunk size, will be replaced later
	pos_csize = #content

	-- Aand write the actual sample data
	local samples = 0

	local packsample
	if depth == 16 then
		packsample = function(v) table.insert(content, strpack("<i2", trunc(v * 32000))) end
	else
		packsample = function(v) table.insert(content, strpack("<B", trunc(v * 127 + 128))) end
	end
	
	if rate == 44100 then
		for v in self:generate() do
			samples = samples + 1
			packsample(v)
		end
	else
		local iter = self:generate()
		while true do
			local v1 = iter()
			local v2 = v1 and iter()
			if not v2 then break end
			samples = samples + 1
			packsample(0.5 * (v1 + v2))
		end
	end

	-- Seek back to the stored positions
	content[pos_fsize] = strpack("<i4", pos_csize - 4 + samples * depth / 8) -- remaining file size
	content[pos_csize] = strpack("<i4", samples * depth / 8) -- chunk size

	return table.concat(content, "")
end

--- Save the sound parameters to a file as a Lua table
-- @within Serialization
-- @tparam ?string|file|love.filesystem.File f a path or file in `w`-mode
-- (passed files will not be closed)
-- @tparam[opt=true] bool minify whether to minify the output or not
function sfxr.Sound:save()
	local content = {}
	local pack = function(fmt, ...)
		table.insert(content, select("#", ...) == 0 and fmt or fmt:format(...))
	end

	-- we'll compare the current parameters with the defaults
	local defaults = sfxr.newSound()

	pack("return {")
		for k, v in pairs(defaults) do
			if v ~= self[k] then
				pack(" %s = %f,", k, self[k])
			end
		end
	pack("}, \"%s\"", sfxr.VERSION)

	return table.concat(content, "\n")
end

--- Load the sound parameters from a string containing a lua table
-- @within Serialization
-- @tparam ?string s a string containing a lua table
-- @raise "incompatible version: x.x.x"
function sfxr.Sound:load(s)
	local params, version = assert(loadstring(s))()
	-- check version compatibility
	if version < sfxr.VERSION then
		error("incompatible version: loaded " .. tostring(version) .. " wanted >= " .. tostring(sfxr.VERSION))
	end

	self:resetParameters()
	-- merge the loaded table into the own
	mergetables(self, params)

	if close then
		f:close()
	end
end

--- Save the sound parameters to a file in the sfxr binary format (version 102)
-- @within Serialization
-- @tparam ?string|file|love.filesystem.File f a path or file in `wb`-mode
-- (passed files will not be closed)
function sfxr.Sound:saveBinary()
	local content = {}
	local function pack(fmt, ...)
		table.insert(content, select("#", ...) == 0 and fmt or (strpack(fmt, ...)))
	end

	pack('\x66\x00\x00\x00') -- version 102
	assert(self.waveform < 256)
	pack(string.char(self.waveform) .. '\x00\x00\x00')
	pack("<f", self.soundvolume)

	pack("<ffffff",
		self.frequency_start,
		self.frequency_min,
		self.frequency_slide,
		self.frequency_dslide,
		self.duty_ratio,
		self.duty_sweep
		)

	pack("<fff",
		self.vibrato_depth,
		self.vibrato_speed,
		self.vibrato_delay
		)

	pack("<ffff",
		self.envelope_attack,
		self.envelope_sustain,
		self.envelope_decay,
		self.envelope_punch
		)

	pack('\x00') -- unused filter_on boolean

	pack("<fffff",
		self.lowpass_resonance,
		self.lowpass_cutoff,
		self.lowpass_sweep,

		self.highpass_cutoff,
		self.highpass_sweep
		)

	pack("<fffff",
		self.phaser_offset,
		self.phaser_sweep,

		self.repeatspeed,

		self.change_speed,
		self.change_amount
		)

	return table.concat(content, "")
end

--- Load the sound parameters from a data string in the sfxr binary format
-- (version 100-102)
-- @within Serialization
-- @tparam ?string s a data string in the sfxr binary format
-- (passed files will not be closed)
-- @raise "incompatible version: x", "unexpected file length"
function sfxr.Sound:loadBinary(s)
	self:resetParameters()

	local off = 1

	local function readFloat()
		local f = strunpack("<f", s, off)
		off = off + 4
		return f
	end

	-- Start reading the string

	local version = s:byte(off)
	off = off + 4
	if version < 100 or version > 102 then
		error("incompatible version: " .. tostring(version))
	end

	self.waveform = s:byte(off)
	off = off + 4
	self.soundvolume = version==102 and readFloat() or 0.5

	self.frequency_start = readFloat()
	self.frequency_min = readFloat()
	self.frequency_slide = readFloat()
	self.frequency_dslide = version>=101 and readFloat() or 0

	self.duty_ratio = readFloat()
	self.duty_sweep = readFloat()

	self.vibrato_depth = readFloat()
	self.vibrato_speed = readFloat()
	self.vibrato_delay = readFloat()

	self.envelope_attack = readFloat()
	self.envelope_sustain = readFloat()
	self.envelope_decay = readFloat()
	self.envelope_punch = readFloat()

	off = off + 1 -- filter_on - seems to be ignored in the C++ version
	self.lowpass_resonance = readFloat()
	self.lowpass_cutoff = readFloat()
	self.lowpass_sweep = readFloat()
	self.highpass_cutoff = readFloat()
	self.highpass_sweep = readFloat()

	self.phaser_offset = readFloat()
	self.phaser_sweep = readFloat()

	self.repeatspeed = readFloat()

	if version >= 101 then
		self.change_speed = readFloat()
		self.change_amount = readFloat()
	end

	if off-1 > s:len() then
		error("unexpected file length")
	end
	self:sanitizeParameters()
end

return sfxr
