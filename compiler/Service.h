#ifndef __Service_h__
#define __Service_h__

#include "Definition.h"
#include "Field.h"

/** Bintalk's method. */
class Method : public FieldContainer
{
public:
	Method():mid_(0) {}
	const char* getNameC() { return name_.c_str(); }
	std::string		name_;
	unsigned int	mid_;
};

/** Bintalk's service definition. */
class Service : public Definition
{
public:
	Service() {}
	virtual Service* getService()	{ return this; }
	bool findMethod(const std::string& name)
	{
		for(size_t i = 0; i < methods_.size(); i++)
			if(methods_[i].name_ == name)
				return true;
		return false;
	}
	std::vector<Method>	methods_;
};


#endif//__Service_h__
