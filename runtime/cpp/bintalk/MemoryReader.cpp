#include "MemoryReader.h"
#include <cstring>

namespace bintalk
{

bool MemoryReader::read(void* data, size_t len)
{
	if(ptr_ + len >= data_->size())
		return false;
	memcpy(data, &(data_[ptr_]), len);
	ptr_ += len;
	return true;
}

}