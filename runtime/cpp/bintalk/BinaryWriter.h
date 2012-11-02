#ifndef __BINTALK_BinaryWriter_h__
#define __BINTALK_BinaryWriter_h__

#include "bintalk/Common.h"

namespace bintalk
{

class BinaryWriter
{
public:
	virtual void write(const void* data, size_t len) = 0;
};

}

#endif// __BINTALK_BinaryWriter_h__