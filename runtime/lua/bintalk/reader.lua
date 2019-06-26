---Read dynamic size
---@param b string @buffer.
---@param p number @buffer pointer.
---@return number, number
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

---Read int64 as 8 byte string
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_int64(b, p)
	return string.unpack("<i8", b, p)
end

---Read uint64 as 8 byte string
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_uint64(b, p)
	return string.unpack("<I8", b, p)
end

---Read double
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_double(b, p)
	return string.unpack("<d", b, p)
end

---Read float
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_float(b, p)
	return string.unpack("<f", b, p)
end

---Read int32
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_int32(b, p)
	return string.unpack("<i4", b, p)
end

---Read uint32
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_uint32(b, p)
	return string.unpack("<I4", b, p)
end

---Read int32
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_int16(b, p)
	return string.unpack("<i2", b, p)
end

---Read uint16
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_uint16(b, p)
	return string.unpack("<I2", b, p)
end

---Read int8
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_int8(b, p)
	return string.unpack("<b", b, p)
end

---Read uint8
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_uint8(b, p)
	return string.unpack("<B", b, p)
end

---Read bool
---@param b string @buffer
---@param p number @buffer pointer.
---@return boolean, number
local function type_bool(b, p)
	local r
	r, p = string.unpack("<B", b, p)
	if r == 0 then
		return false, p
	else
		return true, p
	end
end

---Read string
---@param b string @buffer
---@param p number @buffer pointer.
---@param valMax number @max value.
---@return string, number
local function type_string(b, p, valMax)
	local l
	l, p = type_dsize(b, p)
	if valMax and l > valMax then
		error("Invalid string length")
	end
	local r = string.sub(b, p, p+l-1)
	return r, p+l
end

---Read enum
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_enum(b, p)
	return type_uint8(b, p, 0)
end

---Read enum16
---@param b string @buffer
---@param p number @buffer pointer.
---@return number, number
local function type_enum16(b, p)
	return type_uint16(b, p)
end

---Read binary
---@param b string @buffer
---@param p number @buffer pointer.
---@param valMax number @max value.
---@return string, number
local function type_binary(b, p, valMax)
	return type_string(b, p, valMax)
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

BintalkReader = 
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
