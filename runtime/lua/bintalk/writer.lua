---Write dynamic size
---@param v number @size
---@param b string[] @buffer.
local function type_dsize(v, b)
	if v <= 0X3F then
		b[#b+1] = string.char(v)
	elseif v <= 0X3FFF then
		v = v | (1<<14)
		local v1 = (v & 0XFF00)>>8
		local v2 = (v & 0XFF)
		b[#b+1] = string.char(v1, v2)
	elseif v <= 0X3FFFFF then
		v = v | (2<<22)
		local v1 = (v & 0XFF0000)>>16
		local v2 = (v & 0XFF00)>>8
		local v3 = (v & 0XFF)
		b[#b+1] = string.char(v1, v2, v3)
	elseif v <= 0X3FFFFFFF then
		v = v | (3<<30)
		local v1 = (v & 0XFF000000)>>24
		local v2 = (v & 0XFF0000)>>16
		local v3 = (v & 0XFF00)>>8
		local v4 = (v & 0XFF)
		b[#b+1] = string.char(v1, v2, v3, v4)
	else
		error("Invalid dynamic size")
	end
end

-- Write int64 as string
---@param v number @value
---@param b string[] @buffer.
local function type_int64(v, b)
	b[#b+1] = string.pack("<i8", v)
end

-- Write uint64 as string
---@param v number @value
---@param b string[] @buffer.
local function type_uint64(v, b)
	b[#b+1] = string.pack("<I8", v)
end

-- Write double
---@param v number @size
---@param b string[] @buffer.
local function type_double(v, b)
	b[#b+1] = string.pack("<d", v)
end

-- Write float
---@param v number @size
---@param b string[] @buffer.
local function type_float(v, b)
	b[#b+1] = string.pack("<f", v)
end

-- Write int32
---@param v number @size
---@param b string[] @buffer.
local function type_int32(v, b)
	b[#b+1] = string.pack("<i4", v)
end

-- Write uint32
---@param v number @size
---@param b string[] @buffer.
local function type_uint32(v, b)
	b[#b+1] = string.pack("<I4", v)
end

-- Write int16
---@param v number @size
---@param b string[] @buffer.
local function type_int16(v, b)
	b[#b+1] = string.pack("<i2", v)
end

-- Write uint16
---@param v number @size
---@param b string[] @buffer.
local function type_uint16(v, b)
	b[#b+1] = string.pack("<I2", v)
end

-- Write int8
---@param v number @size
---@param b string[] @buffer.
local function type_int8(v, b)
	b[#b+1] = string.pack("<b", v)
end

-- Write uint8
---@param v number @size
---@param b string[] @buffer.
local function type_uint8(v, b)
	b[#b+1] = string.pack("<B", v)
end

-- Write bool
---@param v number @size
---@param b string[] @buffer.
local function type_bool(v, b)
	b[#b+1] = string.pack("<B", (v and 1 or 0))
end

-- Write string
---@param v number @size
---@param b string[] @buffer.
local function type_string(v, b)
	assert(type(v) == "string")
	type_dsize(#v, b)
	b[#b+1] = v
end

-- Write enum
---@param v number @size
---@param b string[] @buffer.
local function type_enum(v, b)
	type_uint8(v, b)
end

-- Write enum16
---@param v number @size
---@param b string[] @buffer.
local function type_enum16(v, b)
	type_uint16(v, b)
end

-- Write binary
---@param v number @size
---@param b string[] @buffer.
local function type_binary(v, b)
	assert(type(v) == "string")
	type_dsize(#v, b)
	b[#b+1] = v
end

-- Write array of type t
---@param t fun()
---@param v number @size
---@param b string[] @buffer.
local function type_array(t, v, b)
	assert(type(v) == "table")
	type_dsize(#v, b)
	for i = 1, #v do
		t(v[i], b)
	end
end

BintalkWriter = 
{
	int64	= type_int64,
	uint64 = type_uint64,
	double = type_double,
	float	= type_float,
	int32	= type_int32,
	uint32 = type_uint32,
	int16	= type_int16,
	uint16 = type_uint16,
	int8	= type_int8,
	uint8	= type_uint8,
	bool	= type_bool,
	string	= type_string,
	binary	= type_binary,
	enum	= type_enum,
	enum16	= type_enum16,

	array	= type_array,
}
