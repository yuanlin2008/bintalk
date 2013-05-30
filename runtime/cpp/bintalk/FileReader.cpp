#include "FileReader.h"

namespace bintalk
{

bool FileReader::read(void* data, size_t len)
{
	size_t r = fread(data, 1, len, f_);
	return (r == len)?true:false;
}

}