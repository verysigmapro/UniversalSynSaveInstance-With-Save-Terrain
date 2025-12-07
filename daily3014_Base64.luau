--[=[
	Cryptography library: Base64
	
	Return type: buffer
	Example usage:
		local Input = buffer.fromstring("Hello World")
		local Encoded = Base64.Encode(Input)
		local Decoded = Base64.Decode(Encoded)
--]=]

--!strict
--!optimize 2
--!native

local b_create = buffer.create
local b_len = buffer.len
local b_readu8 = buffer.readu8
local b_readu16 = buffer.readu16
local b_readu32 = buffer.readu32
local b_writeu8 = buffer.writeu8
local b_writeu16 = buffer.writeu16
local b_writeu32 = buffer.writeu32

local PADDING_CHARACTER = 61

local ALPHABET_BYTES = b_create(64)
do
	local Ch = {
		65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
		81,82,83,84,85,86,87,88,89,90,
		97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,
		113,114,115,116,117,118,119,120,121,122,
		48,49,50,51,52,53,54,55,56,57,
		43,47
	}
	for i=1,64 do
		b_writeu8(ALPHABET_BYTES, i-1, Ch[i])
	end
end

local ENCODE_A = b_create(131072)
local ENCODE_B = b_create(131072)
do
	for byte0=0,255 do
		local sext0 = byte0 // 4
		local sext0char = b_readu8(ALPHABET_BYTES, sext0)
		local byte0low = byte0 % 4
		for byte1=0,255 do
			local sext1 = byte0low * 16 + (byte1 // 16)
			local idx = (byte0 + byte1 * 256) * 2
			b_writeu16(ENCODE_A, idx, sext0char + b_readu8(ALPHABET_BYTES, sext1) * 256)
		end
	end
	for byte1=0,255 do
		local nib = (byte1 % 16) * 4
		for byte2=0,255 do
			local sext2 = nib + (byte2 // 64)
			local sext3 = byte2 % 64
			local idx = (byte1 + byte2 * 256) * 2
			b_writeu16(ENCODE_B, idx, b_readu8(ALPHABET_BYTES, sext2) + b_readu8(ALPHABET_BYTES, sext3) * 256)
		end
	end
end

local DECODE_TET = b_create(256)
do
	for i=1,64 do
		b_writeu8(DECODE_TET, b_readu8(ALPHABET_BYTES, i-1), i-1)
	end
	b_writeu8(DECODE_TET, PADDING_CHARACTER, 254)
end

local DECODE_PAIR = b_create(131072)
do
	for c0=0,255 do
		local d0 = b_readu8(DECODE_TET, c0)
		for c1=0,255 do
			local d1 = b_readu8(DECODE_TET, c1)
			local key = (c0 + c1 * 256) * 2
			if d0 < 64 and d1 < 64 then
				b_writeu16(DECODE_PAIR, key, d0 * 64 + d1)
			elseif d0 == 254 or d1 == 254 then
				b_writeu16(DECODE_PAIR, key, 254)
			end
		end
	end
end

local PAIR_A_BYTE1_HIGH = b_create(4096)
local PAIR_B_BYTE1_LOW = b_create(4096)
local PAIR_A_BYTE0 = b_create(4096)
local PAIR_B_BYTE2 = b_create(4096)
do
	for p=0,4095 do
		local s0 = p // 64
		local s1 = p % 64
		b_writeu8(PAIR_A_BYTE0, p, s0 * 4 + (s1 // 16))
		b_writeu8(PAIR_A_BYTE1_HIGH, p, (s1 % 16) * 16)
	end
	for p=0,4095 do
		local s2 = p // 64
		local s3 = p % 64
		b_writeu8(PAIR_B_BYTE1_LOW, p, s2 // 4)
		b_writeu8(PAIR_B_BYTE2, p, (s2 % 4) * 64 + s3)
	end
end

local function Encode(Input: buffer): buffer
	local L = b_len(Input)
	if L == 0 then return b_create(0) end
	local Out = b_create(((L + 2) // 3) * 4)
	local i = 0
	local o = 0
	local FullEnd = L - (L % 3)
	local UnrollStop = L - 13

	local encA = ENCODE_A
	local encB = ENCODE_B
	local alpha = ALPHABET_BYTES

	while i <= UnrollStop do
		local w0 = b_readu32(Input, i)
		local w1 = b_readu32(Input, i + 3)
		local w2 = b_readu32(Input, i + 6)
		local w3 = b_readu32(Input, i + 9)

		local kA0 = w0 % 65536
		local kB0 = (w0 // 256) % 65536
		b_writeu32(Out, o, b_readu16(encA, kA0 * 2) + b_readu16(encB, kB0 * 2) * 0x10000)

		local kA1 = w1 % 65536
		local kB1 = (w1 // 256) % 65536
		b_writeu32(Out, o + 4, b_readu16(encA, kA1 * 2) + b_readu16(encB, kB1 * 2) * 0x10000)

		local kA2 = w2 % 65536
		local kB2 = (w2 // 256) % 65536
		b_writeu32(Out, o + 8, b_readu16(encA, kA2 * 2) + b_readu16(encB, kB2 * 2) * 0x10000)

		local kA3 = w3 % 65536
		local kB3 = (w3 // 256) % 65536
		b_writeu32(Out, o + 12, b_readu16(encA, kA3 * 2) + b_readu16(encB, kB3 * 2) * 0x10000)

		i += 12
		o += 16
	end

	while i < FullEnd do
		local kA = b_readu16(Input, i)
		local b2 = b_readu8(Input, i + 2)
		local kB = (kA // 256) + b2 * 256
		b_writeu32(Out, o, b_readu16(encA, kA * 2) + b_readu16(encB, kB * 2) * 0x10000)
		i += 3
		o += 4
	end

	local rem = L - i
	if rem == 1 then
		local b0 = b_readu8(Input, i)
		local s0 = b0 // 4
		local s1 = (b0 % 4) * 16
		local pair = b_readu8(alpha, s0) + b_readu8(alpha, s1) * 256
		b_writeu16(Out, o, pair)
		b_writeu8(Out, o + 2, PADDING_CHARACTER)
		b_writeu8(Out, o + 3, PADDING_CHARACTER)
	elseif rem == 2 then
		local b0 = b_readu8(Input, i)
		local b1 = b_readu8(Input, i + 1)
		local kA = b0 + b1 * 256
		b_writeu16(Out, o, b_readu16(encA, kA * 2))
		local s2 = (b1 % 16) * 4
		b_writeu8(Out, o + 2, b_readu8(alpha, s2))
		b_writeu8(Out, o + 3, PADDING_CHARACTER)
	end

	return Out
end

local function Decode(Input: buffer): buffer
	local L = b_len(Input)
	if L == 0 then return b_create(0) end

	local padding = 0
	if b_readu8(Input, L - 1) == PADDING_CHARACTER then
		padding = 1
		if b_readu8(Input, L - 2) == PADDING_CHARACTER then
			padding = 2
		end
	end

	local Out = b_create((L // 4) * 3 - padding)
	local i = 0
	local o = 0
	local BodyEnd = L - 4
	local UnrollStop = BodyEnd - 8

	local decPair = DECODE_PAIR
	local pA0 = PAIR_A_BYTE0
	local pAHigh = PAIR_A_BYTE1_HIGH
	local pBLow = PAIR_B_BYTE1_LOW
	local p2 = PAIR_B_BYTE2
	local dtet = DECODE_TET

	while i <= UnrollStop do
		local w0 = b_readu32(Input, i)
		local w1 = b_readu32(Input, i + 4)

		local pa0 = b_readu16(decPair, (w0 % 65536) * 2)
		local pb0 = b_readu16(decPair, (w0 // 65536) * 2)
		b_writeu8(Out, o, b_readu8(pA0, pa0))
		b_writeu8(Out, o + 1, b_readu8(pAHigh, pa0) + b_readu8(pBLow, pb0))
		b_writeu8(Out, o + 2, b_readu8(p2, pb0))

		local pa1 = b_readu16(decPair, (w1 % 65536) * 2)
		local pb1 = b_readu16(decPair, (w1 // 65536) * 2)
		b_writeu8(Out, o + 3, b_readu8(pA0, pa1))
		b_writeu8(Out, o + 4, b_readu8(pAHigh, pa1) + b_readu8(pBLow, pb1))
		b_writeu8(Out, o + 5, b_readu8(p2, pb1))

		i += 8
		o += 6
	end

	while i <= BodyEnd - 4 do
		local w = b_readu32(Input, i)
		local pa = b_readu16(decPair, (w % 65536) * 2)
		local pb = b_readu16(decPair, (w // 65536) * 2)
		b_writeu8(Out, o, b_readu8(pA0, pa))
		b_writeu8(Out, o + 1, b_readu8(pAHigh, pa) + b_readu8(pBLow, pb))
		b_writeu8(Out, o + 2, b_readu8(p2, pb))
		i += 4
		o += 3
	end

	do
		local c0 = b_readu8(Input, i)
		local c1 = b_readu8(Input, i + 1)
		local c2 = b_readu8(Input, i + 2)
		local c3 = b_readu8(Input, i + 3)
		local d0 = b_readu8(dtet, c0)
		local d1 = b_readu8(dtet, c1)

		if c2 == PADDING_CHARACTER then
			local pa = d0 * 64 + d1
			b_writeu8(Out, o, b_readu8(pA0, pa))
		elseif c3 == PADDING_CHARACTER then
			local d2 = b_readu8(dtet, c2)
			local pa = d0 * 64 + d1
			local pb = d2 * 64
			b_writeu8(Out, o, b_readu8(pA0, pa))
			b_writeu8(Out, o + 1, b_readu8(pAHigh, pa) + (pb // 256))
		else
			local d2 = b_readu8(dtet, c2)
			local d3 = b_readu8(dtet, c3)
			local pa = d0 * 64 + d1
			local pb = d2 * 64 + d3
			b_writeu8(Out, o, b_readu8(pA0, pa))
			b_writeu8(Out, o + 1, b_readu8(pAHigh, pa) + b_readu8(pBLow, pb))
			b_writeu8(Out, o + 2, b_readu8(p2, pb))
		end
	end

	return Out
end

return {
	Encode = Encode,
	Decode = Decode
}
