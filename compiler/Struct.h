#ifndef __Struct_h__
#define __Struct_h__

#include <vector>
#include "Definition.h"
#include "Field.h"

class Struct : 
	public Definition,
	public FieldContainer
{
public:
	Struct() {}
	virtual Struct*	getStruct()	{ return this; }
};


#endif//__Struct_h__
