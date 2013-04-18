#include "Config.h"
#include "ProtocolWriter.h"

namespace bintalk
{

void ProtocolWriter::writeMID(BinaryWriter* w, MID mid)
{
#if BINTALK_BIG_ENDIAN
	mid = swapEndian<MID>(mid);
#endif//BINTALK_BIG_ENDIAN
	w->write(&mid, sizeof(MID));
}

#if BINTALK_BIG_ENDIAN
	#define BINTALK_PROTOCOLWRITER_SINGLE_IMP(T)\
		void ProtocolWriter::write##T(BinaryWriter* w, const T& v) \
		{ T vv = swapEndian<T>(v); w->write(&vv, sizeof(T)); }
#else //BINTALK_BIG_ENDIAN 
	#define BINTALK_PROTOCOLWRITER_SINGLE_IMP(T)\
		void ProtocolWriter::write##T(BinaryWriter* w, const T& v) \
		{ w->write(&v, sizeof(T)); }
#endif //BINTALK_BIG_ENDIAN 

#define BINTALK_PROTOCOLWRITER_ARRAY_IMP(T)\
void ProtocolWriter::write##T##A(BinaryWriter* w, const std::vector<T>& v)\
{\
	UINT32 s = (UINT32)v.size();\
	writeDynSize(w, s);\
	for(UINT32 i = 0; i < s; i++)\
		write##T(w, v[i]);\
}
#define BINTALK_PROTOCOLWRITER_SIMPLE_IMP(T)\
	BINTALK_PROTOCOLWRITER_SINGLE_IMP(T);\
	BINTALK_PROTOCOLWRITER_ARRAY_IMP(T);

BINTALK_PROTOCOLWRITER_SIMPLE_IMP(INT64);
BINTALK_PROTOCOLWRITER_SIMPLE_IMP(UINT64);
BINTALK_PROTOCOLWRITER_SIMPLE_IMP(DOUBLE);
BINTALK_PROTOCOLWRITER_SIMPLE_IMP(FLOAT);
BINTALK_PROTOCOLWRITER_SIMPLE_IMP(INT32);
BINTALK_PROTOCOLWRITER_SIMPLE_IMP(UINT32);
BINTALK_PROTOCOLWRITER_SIMPLE_IMP(INT16);
BINTALK_PROTOCOLWRITER_SIMPLE_IMP(UINT16);
BINTALK_PROTOCOLWRITER_SIMPLE_IMP(INT8);
BINTALK_PROTOCOLWRITER_SIMPLE_IMP(UINT8);

void ProtocolWriter::writeBOOL(BinaryWriter* w, const BOOL& v)
{
	UINT8 v1 = v?1:0;
	w->write(&v1, sizeof(UINT8));
}
BINTALK_PROTOCOLWRITER_ARRAY_IMP(BOOL);
void ProtocolWriter::writeSTRING(BinaryWriter* w, const STRING& v)
{
	UINT32 s = (UINT32)v.size();
	writeDynSize(w, s);
	w->write(v.c_str(), s);
}
BINTALK_PROTOCOLWRITER_ARRAY_IMP(STRING);
void ProtocolWriter::writeBINARY(BinaryWriter* w, const BINARY& v)
{
	UINT32 s = (UINT32)v.size();
	writeDynSize(w, s);
	if(s > 0)
		w->write(&(v[0]), s);
}
BINTALK_PROTOCOLWRITER_ARRAY_IMP(BINARY);
// Dynamic size.
void ProtocolWriter::writeDynSize(BinaryWriter* w, UINT32 s)
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

}