#ifndef __BINTALK_MemoryWriter_h__
#define __BINTALK_MemoryWriter_h__

#include "bintalk/BinaryWriter.h"

namespace bintalk
{

	/** Write binary data to memory. */
	class MemoryWriter : public BinaryWriter
	{
	public:
		MemoryWriter(BINARY* d):data_(d) {}
		BINARY*	data()	{ return data_; }
		virtual void write(const void* data, size_t len);
	private:
		BINARY*		data_;
	};

}

#endif//__BINTALK_MemoryWriter_h__