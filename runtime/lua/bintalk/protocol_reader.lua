
-- Read dynamic size
local function type_dsize(b, p)
	local bb = string.byte(b, p)
	p = p + 1
	n = (bb&0XC0)>>6
	s = (bb&0X3F)
	while n > 0 do
		bb = string.byte(b, p)
		p = p + 1
		s = (s<<8)|bb
		n = n - 1
	end
	return s, p
end

-- Read int64 as 8 byte string
local function type_int64(b, p, valMax)
	local r = string.sub(b, p, p+7)
	return r, p+8
end

-- Read uint64 as 8 byte string
local function type_uint64(b, p, valMax)
	local r = string.sub(b, p, p+7)
	return r, p+8
end

-- Read double
local function type_double(b, p, valMax)
	return string.unpack("<d", b, p)
end

-- Read float
local function type_float(b, p, valMax)
	return string.unpack("<f", b, p)
end

-- Read int32
local function type_int32(b, p, valMax)
	return string.unpack("<l", b, p)
	--local a,b,c,d = string.byte(b, p, p+3)
	--return a + b * 0X100 + c * 0X10000 + d * 0X1000000
end

-- Read uint32
local function type_uint32(b, p, valMax)
	return string.unpack("<L", b, p)
end

-- Read int32
local function type_int16(b, p, valMax)
	return string.unpack("<h", b, p)
end

-- Read uint16
local function type_uint16(b, p, valMax)
	return string.unpack("<H", b, p)
end

-- Read int8
local function type_int8(b, p, valMax)
	return string.unpack("<b", b, p)
end

-- Read uint8
local function type_uint8(b, p, valMax)
	return string.unpack("<B", b, p)
end

-- Read bool
local function type_bool(b, p, valMax)
	local r
	r, p = string.unpack("<B", b, p)
	return (r == 0 and false or true), p
end

-- Read string
local function type_string(b, p, valMax)
	local l
	l, p = type_dsize(b, p)
	if l > valMax then
		error("Invalid string length")
	end
	local r = string.sub(b, p, p+l-1)
	return r, p+l
end

-- Read enum
local function type_enum(b, p, valMax)
	return type_uint8(b, p, 0)
end

-- Read enum16
local function type_enum16(b, p, valMax)
	return type_uint16(b, p, 0)
end

-- Read binary
local function type_binary(b, p, valMax)
	return type_string(b, p, valMax)
end

-- Read message id.
local function type_mid(b, p, valMax)
	return type_uint16(b, p, 0)
end

-- Read array of type t.
local function type_array(b, p, t, arrMax, valMax)
	local l
	l, p = type_dsize(b, p)
	if l > arrMax then
		error("Invalid array length")
	else
		local arr = {}
		for i = 1, l do
			local r
			r, p = t(b, p, valMax)
			arr[i] = r
		end
		return arr, p
	end
end

BTK_Reader = 
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
