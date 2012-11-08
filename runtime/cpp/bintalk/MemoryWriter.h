#ifndef __BINTALK_MemoryWriter_h__
#define __BINTALK_MemoryWriter_h__

#include "bintalk/BinaryWriter.h"

namespace bintalk
{

	/** Write binary data to memory. */
	class MemoryWriter : public BinaryWriter
	{
	public:
		MemoryWriter(BINARY* d, size_t p = 0):data_(d),ptr_(p) {}
		BINARY*	data()	{ return data_; }
		size_t	ptr()	{ return ptr_; }
		virtual void write(const void* data, size_t len);
	private:
		BINARY*		data_;
		size_t		ptr_;
	};

}

#endif//__BINTALK_MemoryWriter_h__