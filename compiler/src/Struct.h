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
	Struct():
	super_(NULL)
	{}
	virtual Struct*	getStruct()	
	{ 
		return this; 
	}
	bool findField(const std::string& name)
	{	
		if(FieldContainer::findField(name))
			return true;
		return super_?super_->findField(name):false;
	}
	void getAllFields(std::vector<Field*>& fields)
	{
		if(super_)
			super_->getAllFields(fields);
		for(size_t i = 0; i < fields_.size(); i++)
			fields.push_back(&(fields_[i]));
	}

	Struct*		super_;
};


#endif//__Struct_h__
