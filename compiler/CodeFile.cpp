#include <stdarg.h>
#include "CodeFile.h"

CodeFile::CodeFile(const std::string& filename):
file_(NULL),
ind_(0),
listItemNum_(0),
listNL_(false)
{
	file_ = fopen(filename.c_str(), "w");
	if(!file_)
	{
		std::string es = "failed to open file ";
		es += filename;
		throw es.c_str();
	}
}

CodeFile::~CodeFile()
{
	fclose(file_);
}

void CodeFile::indent(const char* s)
{
	if(s)
		output(s);
	ind_++;
}

void CodeFile::recover(const char* s)
{
	ind_--;
	if(s)
		output(s);
}

#define PRINT_INDENT(F, I)\
	for(unsigned int i = 0; i < I; i++)\
		fprintf(F, "\t");

#define VFPRINTF(F, S)\
	va_list arg_ptr;\
	va_start(arg_ptr, S);\
	vfprintf(F, S, arg_ptr);\
	va_end(arg_ptr);

void CodeFile::output(const char* s, ...)
{
	PRINT_INDENT(file_, ind_);
	VFPRINTF(file_, s);
	fprintf(file_, "\n");
}

void CodeFile::listBegin(const char* sep, bool nl, const char* s, ...)
{
	PRINT_INDENT(file_, ind_);
	VFPRINTF(file_, s);
	listSep_ = sep;
	listItemNum_ = 0;
	listNL_ = nl;
}

void CodeFile::listItem(const char* s, ...)
{
	if(listItemNum_ > 0)
		fprintf(file_, "%s", listSep_.c_str());
	listItemNum_++;
	if(listNL_)
	{
		fprintf(file_, "\n");
		PRINT_INDENT(file_, (ind_+1));
	}
	VFPRINTF(file_, s);
}

void CodeFile::listEnd(const char* s, ...)
{
	if(listNL_)
	{
		fprintf(file_, "\n");
		PRINT_INDENT(file_, ind_);
	}
	VFPRINTF(file_, s);
	fprintf(file_, "\n");
}
