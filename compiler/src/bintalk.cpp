#include "Options.h"
#include "Context.h"
#include "CodeGenerator.h"

int main(int argc, char *argv[])
{
	if(!gOptions.parse(argc, argv))
		return 1;
	if(!gContext.build())
		return 1;
	if(!CodeGenerator::exec())
		return 1;
	return 0;
}
