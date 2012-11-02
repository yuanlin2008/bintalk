#ifndef __BINTALK_BinaryReader_h__
#define __BINTALK_BinaryReader_h__

#include "bintalk/Common.h"

namespace bintalk
{

class BinaryReader
{
public:
	virtual bool read(void* data, size_t len) = 0;
};

}

#endif// __BINTALK_BinaryReader_h__