#include <cstdio>
#include "CodeGenerator.h"
#include "Options.h"

CodeGenerator::CodeGenerator(const std::string& name):
name_(name)
{
	next_ = root_;
	root_ = this;
}

CodeGenerator* CodeGenerator::root_;
const char* CodeGenerator::desc()
{
	static std::string desc;
	desc = "available code generators(";
	CodeGenerator* cg = root_;
	while(cg)
	{
		desc += " ";
		desc += cg->name_;
		cg = cg->next_;
	}
	desc += " )";
	return desc.c_str();
}

bool CodeGenerator::exec()
{
	CodeGenerator* cg = root_;
	while(cg)
	{
		if(cg->name_ == gOptions.generator_)
			break;
		cg = cg->next_;
	}
	if(cg == NULL)
	{
		fprintf(stderr, "invalid generator \"%s\"\n", gOptions.generator_.c_str());
		return false;
	}
	try
	{
		cg->generate();
	}
	catch (const char* e)
	{
		fprintf(stderr, "%s\n", e);
		return false;
	}
	return true;
}
