#ifndef __BINTALK_ProtocolWriter_h__
#define __BINTALK_ProtocolWriter_h__

#include <vector>
#include "bintalk/BinaryWriter.h"

namespace bintalk
{

#define BINTALK_PROTOCOLWRITER_SINGLE(T)\
	static void write##T(BinaryWriter* w, T v)\
	{\
		w->write(&v, sizeof(T));\
	}
#define BINTALK_PROTOCOLWRITER_ARRAY(T)\
	static void write##T##A(BinaryWriter* w, const std::vector<T>& v)\
	{\
		UINT32 s = (UINT32)v.size();\
		writeDynSize(w, s);\
		for(UINT32 i = 0; i < s; i++)\
			write##T(w, v[i]);\
	}
#define BINTALK_PROTOCOLWRITER_SIMPLE(T)\
	BINTALK_PROTOCOLWRITER_SINGLE(T);\
	BINTALK_PROTOCOLWRITER_ARRAY(T);

class ProtocolWriter
{
public:
	static void writeMID(BinaryWriter* w, MID mid)
	{
		w->write(&mid, sizeof(MID));
	}
	BINTALK_PROTOCOLWRITER_SIMPLE(INT64);
	BINTALK_PROTOCOLWRITER_SIMPLE(UINT64);
	BINTALK_PROTOCOLWRITER_SIMPLE(DOUBLE);
	BINTALK_PROTOCOLWRITER_SIMPLE(FLOAT);
	BINTALK_PROTOCOLWRITER_SIMPLE(INT32);
	BINTALK_PROTOCOLWRITER_SIMPLE(UINT32);
	BINTALK_PROTOCOLWRITER_SIMPLE(INT16);
	BINTALK_PROTOCOLWRITER_SIMPLE(UINT16);
	BINTALK_PROTOCOLWRITER_SIMPLE(INT8);
	BINTALK_PROTOCOLWRITER_SIMPLE(UINT8);
	// BOOL.
	static void writeBOOL(BinaryWriter* w, BOOL v)
	{
		UINT8 v1 = v?1:0;
		w->write(&v1, sizeof(UINT8));
	}
	BINTALK_PROTOCOLWRITER_ARRAY(BOOL);
	// Enum
	template<class T>
	static void writeENUM(BinaryWriter* w, T v)
	{ 
		UINT8 e = (UINT8)v;
		w->write(&e, sizeof(UINT8));
	}
	template<class T>
	static void writeENUMA(BinaryWriter* w, const std::vector<T>& v)
	{ 
		UINT32 s = (UINT32)v.size();
		writeDynSize(w, s);
		for(UINT32 i = 0; i < s; i++)
			writeENUM(w, v[i]);
	}
	// String
	static void writeSTRING(BinaryWriter* w, const STRING& v)
	{
		UINT32 s = (UINT32)v.size();
		writeDynSize(w, s);
		w->write(v.c_str(), s);
	}
	BINTALK_PROTOCOLWRITER_ARRAY(STRING);
	// User
	template<class T>
	static void writeUSER(BinaryWriter* w, const T& v)
	{
		v.serialize(w);
	}
	template<class T>
	static void writeUSERA(BinaryWriter* w, const std::vector<T>& v)
	{	
		UINT32 s = (UINT32)v.size();
		writeDynSize(w, s);
		for(UINT32 i = 0; i < s; i++)
			writeUSER(w, v[i]);
	}
	// Binary
	static void writeBINARY(BinaryWriter* w, const BINARY& v)
	{
		UINT32 s = (UINT32)v.size();
		writeDynSize(w, s);
		if(s > 0)
			w->write(&(v[0]), s);
	}
	BINTALK_PROTOCOLWRITER_ARRAY(BINARY);
	// Dynamic size.
	static void writeDynSize(BinaryWriter* w, UINT32 s)
	{
		UINT8* p = (UINT8*)(&s);
		UINT8 n = 0;
		if(s <= 0X3F)
			n = 0;
		else if(s <= 0X3FFF)
			n = 1;
		else if(s <= 0X3FFFFF)
			n = 2;
		else if(s <= 0X3FFFFFFF)
			n = 3;
		p[n] |= (n<<6);
		for(int i = (int)n; i >= 0; i--)
			w->write(p+i, sizeof(UINT8));
	}
};

}

#endif// __BINTALK_ProtocolWriter_h__