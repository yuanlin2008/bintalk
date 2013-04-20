#include "MemoryReader.h"
#include <cstring>

namespace bintalk
{

MemoryReader::MemoryReader(void* data, size_t len):
data_((char*)data),
len_(len),
ptr_(0)
{
}

bool MemoryReader::read(void* data, size_t len)
{
	if(ptr_ + len > len_)
		return false;
	::memcpy(data, data_ + ptr_, len);
	ptr_ += len;
	return true;
}

}