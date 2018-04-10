
-- Write dynamic size
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
local function type_int64(v, b)
	assert(type(v) == "string")
	assert(#v == 8)
	b[#b+1] = v
end

-- Write uint64 as string
local function type_uint64(v, b)
	assert(type(v) == "string")
	assert(#v == 8)
	b[#b+1] = v
end

-- Write double
local function type_double(v, b)
	b[#b+1] = string.pack("<d", v)
end

-- Write float
local function type_float(v, b)
	b[#b+1] = string.pack("<f", v)
end

-- Write int32
local function type_int32(v, b)
	b[#b+1] = string.pack("<l", v)
end

-- Write uint32
local function type_uint32(v, b)
	b[#b+1] = string.pack("<L", v)
end

-- Write int16
local function type_int16(v, b)
	b[#b+1] = string.pack("<h", v)
end

-- Write uint16
local function type_uint16(v, b)
	b[#b+1] = string.pack("<H", v)
end

-- Write int8
local function type_int8(v, b)
	b[#b+1] = string.pack("<b", v)
end

-- Write uint8
local function type_uint8(v, b)
	b[#b+1] = string.pack("<B", v)
end

-- Write bool
local function type_bool(v, b)
	b[#b+1] = string.pack("<B", (v and 1 or 0))
end

-- Write string
local function type_string(v, b)
	assert(type(v) == "string")
	type_dsize(#v, b)
	b[#b+1] = v
end

-- Write enum
local function type_enum(v, b)
	type_uint8(v, b)
end

-- Write enum16
local function type_enum16(v, b)
	type_uint16(v, b)
end
-- Write binary
local function type_binary(v, b)
	assert(type(v) == "string")
	type_dsize(#v, b)
	b[#b+1] = v
end

-- Write message id
local function type_mid(v, b)
	type_uint16(v, b)
end

-- Write array of type t
local function type_array(t, v, b)
	assert(type(v) == "table")
	type_dsize(#v, b)
	for i = 1, #v do
		t(v[i], b)
	end
end

BTK_Writer = 
{
	type_int64	= type_int64,
	type_uint64 = type_uint64,
	type_double = type_double,
	type_float	= type_float,
	type_int32	= type_int32,
	type_uint32 = type_uint32,
	type_int16	= type_int16,
	type_uint16 = type_uint16,
	type_int8	= type_int8,
	type_uint8	= type_uint8,
	type_bool	= type_bool,
	type_string	= type_string,
	type_enum	= type_enum,
	type_enum16	= type_enum16,
	type_binary	= type_binary,
	type_mid	= type_mid,
	type_array	= type_array,
}
