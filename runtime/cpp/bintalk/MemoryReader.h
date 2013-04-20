#ifndef __BINTALK_MemoryReader_h__
#define __BINTALK_MemoryReader_h__

#include "bintalk/BinaryReader.h"

namespace bintalk
{

/** Read binary data from a memory buffer. */
class MemoryReader : public BinaryReader
{
public: 
	MemoryReader (void* data, size_t len);
	virtual bool read(void* data, size_t len);
private:
	char*	data_;
	size_t	len_;
	size_t	ptr_;
};


}

#endif//__BINTALK_MemoryReader_h__