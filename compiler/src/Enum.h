#ifndef __Enum_h__
#define __Enum_h__

#include <vector>
#include <algorithm>
#include "Definition.h"

/** Bintalk's enumeration definition. */
class Enum : public Definition
{
public:
	virtual Enum* getEnum() 
	{ 
		return this; 
	}
	bool findItem(const std::string& item)
	{
		return (std::find(items_.begin(), items_.end(), item) == items_.end())?false:true;
	}

	/** Item names. */
	std::vector<std::string>	items_;
};


#endif//__Enum_h__
