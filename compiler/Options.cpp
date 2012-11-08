#include "Config.h"
#include "Options.h"
#include "CodeGenerator.h"
#include <cstdio>
#include <cstring>

Options gOptions;

void Options::showUsage()
{ 
	fprintf(stderr,
		"bintalk compiler %d.%d\n"
		"usage: bintalk [options] [filenames]\n"
		"Available options are:\n"
		"  -o dir	output to directory\n"
		"  -i dirs	import directories(seperated by \";\")\n"
		"  -g gen	code generator name%s\n",
		BINTALK_VERSION_MAJOR,
		BINTALK_VERSION_MINOR,
		CodeGenerator::desc()
		  );
}

#define GET_OPT_PARAM(P) \
	if(++i >= argc) return false;\
	if(argv[i][0] == '-') return false;\
	P = argv[i];

bool Options::parseOptions(int argc, char* argv[])
{
	argc -= 1;
	for(int i = 1; i < argc; i++)
	{
		const char* opt = argv[i];
		if(::strcmp(opt, "-o") == 0)
		{
			GET_OPT_PARAM(output_);
		}
		else if(::strcmp(opt, "-i") == 0)
		{
			std::string imports;
			GET_OPT_PARAM(imports);
			while(1)
			{
				size_t i = imports.find(';');
				if(i == imports.npos)
				{
					importPaths_.push_back(imports);
					break;
				}
				else
				{
					std::string import = imports.substr(0, i);
					importPaths_.push_back(import);
					imports = imports.substr(i+1);
				}
			}
		}
		else if(::strcmp(opt, "-g") == 0)
		{
			GET_OPT_PARAM(generator_);
		}
		else
			return false;
	}
	return true;
}

bool Options::parseInput(const char* filename)
{
	input_ = filename;
	size_t pos = input_.find_last_of("\\/");
	inputFN_ = (pos == std::string::npos)?input_:input_.substr(pos+1);
	importPaths_.insert(importPaths_.begin(), (pos == std::string::npos)?"":input_.substr(0, pos+1));
	pos = inputFN_.rfind(".");
	inputFS_ = (pos == std::string::npos)?inputFN_:inputFN_.substr(0, pos);
	return true;
}

bool Options::parse(int argc, char* argv[])
{
	if(argc <= 1)
		return false;
	if(!parseOptions(argc, argv))
		return false;
	if(!parseInput(argv[argc-1]))
		return false;
	return true;
}
