#ifndef __BINTALK_ProtocolReader_h__
#define __BINTALK_ProtocolReader_h__

#include <vector>
#include "bintalk/BinaryReader.h"

namespace bintalk
{

#define BINTALK_PROTOCOLREADER_SINGLE(T)\
	static bool read##T(BinaryReader* r, T& v, UINT32 maxArray, UINT32 maxValue)\
	{\
		return r->read(&v, sizeof(T));\
	}
#define BINTALK_PROTOCOLREADER_ARRAY(T)\
	static bool read##T##A(BinaryReader* r, std::vector<T>& v, UINT32 maxArray, UINT32 maxValue)\
	{\
		UINT32 s;\
		if(!readDynSize(r, s) || s > maxArray) return false;\
		v.resize(s);\
		for(UINT32 i = 0; i < s; i++) if(!read##T(r, v[i], maxArray, maxValue)) return false;\
		return true;\
	}
#define BINTALK_PROTOCOLREADER_SIMPLE(T)\
	BINTALK_PROTOCOLREADER_SINGLE(T);\
	BINTALK_PROTOCOLREADER_ARRAY(T);

class ProtocolReader
{
public:
	static bool readMID(BinaryReader* r, MID& mid)
	{
		return r->read(&mid, sizeof(MID));
	}
	BINTALK_PROTOCOLREADER_SIMPLE(INT64);
	BINTALK_PROTOCOLREADER_SIMPLE(UINT64);
	BINTALK_PROTOCOLREADER_SIMPLE(DOUBLE);
	BINTALK_PROTOCOLREADER_SIMPLE(FLOAT);
	BINTALK_PROTOCOLREADER_SIMPLE(INT32);
	BINTALK_PROTOCOLREADER_SIMPLE(UINT32);
	BINTALK_PROTOCOLREADER_SIMPLE(INT16);
	BINTALK_PROTOCOLREADER_SIMPLE(UINT16);
	BINTALK_PROTOCOLREADER_SIMPLE(INT8);
	BINTALK_PROTOCOLREADER_SIMPLE(UINT8);
	// BOOL
	static bool readBOOL(BinaryReader* r, BOOL& v, UINT32 maxArray, UINT32 maxValue)
	{
		UINT8 v1 = v?1:0;
		return r->read(&v, sizeof(BOOL));
	}
	static bool readBOOLA(BinaryReader* r, std::vector<BOOL>& v, UINT32 maxArray, UINT32 maxValue)
	{
		UINT32 s;
		if(!readDynSize(r, s) || s > maxArray) return false;
		v.resize(s);
		for(UINT32 i = 0; i < s; i++) 
		{
			BOOL b;
			if(!readBOOL(r, b, 0, maxValue)) 
				return false;
			v[i] = b;
		}
		return true;
	}
	// Enum
	template<class T>
	static bool readENUM(BinaryReader* r, T& v, UINT32 maxArray, UINT32 maxValue)
	{ 
		UINT8 e;
		if(!r->read(&e, sizeof(UINT8)) || (UINT32)e > maxValue)
			return false;
		v = (T)e;
		return true;
	}
	// Enum Array.
	template<class T>
	static bool readENUMA(BinaryReader* r, std::vector<T>& v, UINT32 maxArray, UINT32 maxValue)
	{	
		UINT32 s;
		if(!readDynSize(r, s) || s > maxArray) return false;
		v.resize(s);
		for(UINT32 i = 0; i < s; i++) if(!readENUM(r, v[i], 0, maxValue)) return false;
		return true;
	}
	// String.
	static bool readSTRING(BinaryReader* r, STRING& v, UINT32 maxArray, UINT32 maxValue)
	{
		UINT32 len;
		if(!readDynSize(r, len) || len > maxValue)
			return false;
		v.resize(len);
		return r->read((void*)v.c_str(), len);
	}
	BINTALK_PROTOCOLREADER_ARRAY(STRING);
	// User
	template<class T>
	static bool readUSER(BinaryReader* r, T& v, UINT32 maxArray, UINT32 maxValue)
	{
		v.deserialize(r);
		return true;
	}
	template<class T>
	static bool readUSERA(BinaryReader* r, std::vector<T>& v, UINT32 maxArray, UINT32 maxValue)
	{	
		UINT32 s;
		if(!readDynSize(r, s) || s > maxArray) return false;
		v.resize(s);
		for(UINT32 i = 0; i < s; i++) if(!readUSER(r, v[i], 0, maxValue)) return false;
		return true;
	}
	// Binary.
	static bool readBINARY(BinaryReader* r, BINARY& v, UINT32 maxArray, UINT32 maxValue)
	{
		UINT32 len;
		if(!readDynSize(r, len) || len > maxValue)
			return false;
		v.resize(len);
		if(len > 0)
			return r->read((void*)(&(v[0])), len);
		return true;
	}
	BINTALK_PROTOCOLREADER_ARRAY(BINARY);
	// Dynamic size.
	static bool readDynSize(BinaryReader* r, UINT32& s)
	{
		s = 0;
		UINT8 b;
		if(!r->read(&b, sizeof(UINT8)))
			return false;
		size_t n = (b & 0XC0)>>6;
		s = (b & 0X3F);
		for(size_t i = 0; i < n; i++)
		{
			if(!r->read(&b, sizeof(UINT8)))
				return false;
			s = (s<<8)|b;
		}
		return true;
	}
};

}

#endif// __BINTALK_ProtocolReader_h__