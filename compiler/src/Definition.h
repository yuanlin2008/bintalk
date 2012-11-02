#ifndef __Definition_h__
#define __Definition_h__

#include <string>

// Forward declaration.
class Enum;
class Struct;
class Service;

/** Bintalk's basic definition. */
class Definition
{
public:
	Definition(){}
	virtual ~Definition() {};

	virtual Enum*	getEnum()		{ return NULL; }
	virtual Struct*	getStruct()		{ return NULL; }
	virtual Service*getService()	{ return NULL; }

	const char* getFileC()	{ return file_.c_str(); }
	const char* getNameC()	{ return name_.c_str(); }

	/** The file this definition belongs to. */
	std::string		file_;
	/** The literal name of the definition. */
	std::string		name_;
};


#endif//__Definition_h__