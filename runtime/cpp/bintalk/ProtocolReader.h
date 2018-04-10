#ifndef __BINTALK_ProtocolReader_h__
#define __BINTALK_ProtocolReader_h__

#include <vector>
#include "bintalk/BinaryReader.h"

namespace bintalk
{

class ProtocolReader
{
public:
	static bool readMID(BinaryReader* r, MID& mid);
#define BINTALK_PROTOCOLREADER_DECL(T)\
	static bool read##T(BinaryReader* r, T& v, UINT32 maxArray, UINT32 maxValue);\
	static bool read##T##A(BinaryReader* r, std::vector<T>& v, UINT32 maxArray, UINT32 maxValue);
	BINTALK_PROTOCOLREADER_DECL(INT64);
	BINTALK_PROTOCOLREADER_DECL(UINT64);
	BINTALK_PROTOCOLREADER_DECL(DOUBLE);
	BINTALK_PROTOCOLREADER_DECL(FLOAT);
	BINTALK_PROTOCOLREADER_DECL(INT32);
	BINTALK_PROTOCOLREADER_DECL(UINT32);
	BINTALK_PROTOCOLREADER_DECL(INT16);
	BINTALK_PROTOCOLREADER_DECL(UINT16);
	BINTALK_PROTOCOLREADER_DECL(INT8);
	BINTALK_PROTOCOLREADER_DECL(UINT8);
	BINTALK_PROTOCOLREADER_DECL(BOOL);
	BINTALK_PROTOCOLREADER_DECL(STRING);
	BINTALK_PROTOCOLREADER_DECL(BINARY);
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
	template<class T>
	static bool readENUMA(BinaryReader* r, std::vector<T>& v, UINT32 maxArray, UINT32 maxValue)
	{	
		UINT32 s;
		if(!readDynSize(r, s) || s > maxArray) return false;
		v.resize(s);
		for(UINT32 i = 0; i < s; i++) if(!readENUM(r, v[i], 0, maxValue)) return false;
		return true;
	}
	// Enum16
	template<class T>
	static bool readENUM16(BinaryReader* r, T& v, UINT32 maxArray, UINT32 maxValue)
	{ 
		UINT16 e;
		if(!readUINT16(r, e, maxArray_, maxValue_))
			return false;
		if((UINT32)e > maxValue)
			return false;
		v = (T)e;
		return true;
	}
	template<class T>
	static bool readENUM16A(BinaryReader* r, std::vector<T>& v, UINT32 maxArray, UINT32 maxValue)
	{	
		UINT32 s;
		if(!readDynSize(r, s) || s > maxArray) return false;
		v.resize(s);
		for(UINT32 i = 0; i < s; i++) if(!readENUM16(r, v[i], 0, maxValue)) return false;
		return true;
	}
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
	// Dynamic size.
	static bool readDynSize(BinaryReader* r, UINT32& s);
};

}

#endif// __BINTALK_ProtocolReader_h__