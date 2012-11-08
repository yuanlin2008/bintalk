#ifndef __BINTALK_FileReader_h__
#define __BINTALK_FileReader_h__

#include "bintalk/BinaryReader.h"
#include <cstdio>

namespace bintalk
{

/** Read binary data from a file. */
class FileReader : public BinaryReader
{
public:
	FileReader(FILE* f):f_(f){}
	virtual bool read(void* data, size_t len);
private:
	FILE* f_;
};

}

#endif//__BINTALK_FileReader_h__