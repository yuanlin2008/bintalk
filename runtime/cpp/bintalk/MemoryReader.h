#ifndef __BINTALK_MemoryReader_h__
#define __BINTALK_MemoryReader_h__

#include "bintalk/BinaryReader.h"

namespace bintalk
{

/** Read binary data from memory. */
class MemoryReader : public BinaryReader
{
public:
	MemoryReader(BINARY* d, size_t p = 0):data_(d),ptr_(p) {}
	BINARY*	data()	{ return data_; }
	size_t	ptr()	{ return ptr_; }
	virtual bool read(void* data, size_t len);
private:
	BINARY*		data_;
	size_t		ptr_;
};

}

#endif//__BINTALK_MemoryReader_h__