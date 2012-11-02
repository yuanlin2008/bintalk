import struct

def type_dsize(b, p):
    bb = struct.unpack('B', b[p:p+1])[0]
    p += 1
    n = (bb & 0XC0)>>6
    s = (bb & 0X3F)
    while n > 0:
        bb = struct.unpack('B', b[p:p+1])[0]
        p += 1
        s = (s<<8)|bb
        n -= 1
    return s, p

def type_number(f, n, b, p):
	return struct.unpack(f, b[p:p+n])[0], p+n
    
def type_int64(b, p, valMax):
    return type_number('<q', 8, b, p)

def type_uint64(b, p, valMax):
    return type_number('<Q', 8, b, p)

def type_double(b, p, valMax):
    return type_number('<d', 8, b, p)

def type_float(b, p, valMax):
    return type_number('<f', 4, b, p)

def type_int32(b, p, valMax):
    return type_number('<i', 4, b, p)

def type_uint32(b, p, valMax):
    return type_number('<I', 4, b, p)

def type_int16(b, p, valMax):
    return type_number('<h', 2, b, p)

def type_uint16(b, p, valMax):
    return type_number('<H', 2, b, p)

def type_int8(b, p, valMax):
    return type_number('<b', 1, b, p)

def type_uint8(b, p, valMax):
    return type_number('<B', 1, b, p)

def type_bool(b, p, valMax):
	if b[p] == '\000':
		return False, p+1
	else:
		return True, p+1

def type_string(b, p, valMax):
	l, p = type_dsize(b, p)
	if l > valMax:
		raise
	return b[p:p+l], p+l
    
def type_enum(b, p, valMax):
    e, p = type_uint8(b, p, 0)
    if e > valMax:
        raise
    return e, p
    
def type_binary(b, p, valMax):
	l, p = type_dsize(b, p)
	if l > valMax:
		raise
	return b[p:p+l], p+l

def read_mid(b, p):
    return type_uint16(b, p, 0);

def read(b, p, t, arrMax, valMax):
    if arrMax > 0:
		s, p = type_dsize(b, p)
		if s > arrMax:
			raise
		else:
			arr = []
			while s > 0:
				i, p = t(b, p, valMax)
				arr.append(i)
				s -= 1
			return arr, p
    else:
        return t(b, p, valMax)
