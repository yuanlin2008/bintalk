#ifndef __BINTALK_ProtocolWriter_h__
#define __BINTALK_ProtocolWriter_h__

#include <vector>
#include "bintalk/BinaryWriter.h"

namespace bintalk
{

class ProtocolWriter
{
public:
	static void writeMID(BinaryWriter* w, MID mid);
#define BINTALK_PROTOCOLWRITER_DECL(T)\
	static void write##T(BinaryWriter* w, const T& v);\
	static void write##T##A(BinaryWriter* w, const std::vector<T>& v);
	BINTALK_PROTOCOLWRITER_DECL(INT64);
	BINTALK_PROTOCOLWRITER_DECL(UINT64);
	BINTALK_PROTOCOLWRITER_DECL(DOUBLE);
	BINTALK_PROTOCOLWRITER_DECL(FLOAT);
	BINTALK_PROTOCOLWRITER_DECL(INT32);
	BINTALK_PROTOCOLWRITER_DECL(UINT32);
	BINTALK_PROTOCOLWRITER_DECL(INT16);
	BINTALK_PROTOCOLWRITER_DECL(UINT16);
	BINTALK_PROTOCOLWRITER_DECL(INT8);
	BINTALK_PROTOCOLWRITER_DECL(UINT8);
	BINTALK_PROTOCOLWRITER_DECL(BOOL);
	BINTALK_PROTOCOLWRITER_DECL(STRING);
	BINTALK_PROTOCOLWRITER_DECL(BINARY);
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
	// Dynamic size.
	static void writeDynSize(BinaryWriter* w, UINT32 s);
};

}

#endif// __BINTALK_ProtocolWriter_h__