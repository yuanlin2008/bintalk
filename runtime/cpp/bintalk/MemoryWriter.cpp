#include "MemoryWriter.h"
#include <cstring>

namespace bintalk
{

void MemoryWriter::write(const void* data, size_t len)
{
	size_t p = data_->size();
	data_->resize(p + len);
	::memcpy(&((*data_)[p]), data, len);
}

}