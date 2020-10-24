import struct

def type_dsize(v, b):
    if v <= 0X3F:
        b.append(struct.pack('>B', v))
    elif v <= 0X3FFF:
        v |= (1<<14)
        b.append(struct.pack('>H', v))
    elif v <= 0X3FFFFF:
        v |= (2<<22)
        b.append(struct.pack('>I', v)[1:])
    elif v <= 0X3FFFFFFF:
        v |= (3<<30)
        b.append(struct.pack('>I', v))

def type_int64(v, b):
	b.append(struct.pack('<q', v))
    
def type_uint64(v, b):
	b.append(struct.pack('<Q', v))
    
def type_double(v, b):
	b.append(struct.pack('<d', v))
    
def type_float(v, b):
	b.append(struct.pack('<f', v))
    
def type_int32(v, b):
	b.append(struct.pack('<i', v))
    
def type_uint32(v, b):
	b.append(struct.pack('<I', v))
    
def type_int16(v, b):
	b.append(struct.pack('<h', v))
    
def type_uint16(v, b):
	b.append(struct.pack('<H', v))
    
def type_int8(v, b):
	b.append(struct.pack('<b', v))
    
def type_uint8(v, b):
	b.append(struct.pack('<B', v))
    
def type_bool(v, b):
	if v:
		b.append('\001')
	else:
		b.append('\000')
        
def type_string(v, b):
	l = len(v)
	type_dsize(l, b)
	b.append(v)
    
def type_enum16(v, b):
    type_uint16(v, b)

def type_binary(v, b):
	l = len(v)
	type_dsize(l, b)
	b.append(v)

def write_mid(v, b):
    type_uint16(v, b)
    
def write(t, a, v, b):
	if a:
		l = len(v)
		type_dsize(l, b)
		i = 0
		while i < l:
			t(v[i], b)
			i += 1
	else:
		return t(v, b)
