#include "MemoryWriter.h"
#include <cstring>

namespace bintalk
{

void MemoryWriter::write(const void* data, size_t len)
{
	if(ptr_ + len > data_->size())
		data_->resize(ptr_ + len);
	memcpy(&(data_[ptr_]), data, len);
	ptr_ += len;
}

}