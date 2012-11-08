#include "FileWriter.h"

namespace bintalk
{

void FileWriter::write(const void* data, size_t len)
{
	fwrite(data, len, 1, f_);
}

}