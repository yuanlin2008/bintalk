#ifndef __BINTALK_FileWriter_h__
#define __BINTALK_FileWriter_h__

#include "bintalk/BinaryWriter.h"
#include <cstdio>

namespace bintalk
{

/** Write binary data to a file. */
class FileWriter : public BinaryWriter
{
public:
	FileWriter(FILE* f):f_(f){}
	virtual void write(void* data, size_t len);
private:
	FILE* f_;
};

}

#endif//__BINTALK_FileWriter_h__