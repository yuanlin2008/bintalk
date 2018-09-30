#ifndef __Definition_h__
#define __Definition_h__

#include <string>
#include <vector>
#include <algorithm>

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


/** Bintalk's field. */
class Field
{
public:
	/** Field Type. */
	enum EFieldType
	{
		FT_INT64,	///< 8 bytes
		FT_UINT64,	///< 8 bytes.
		FT_DOUBLE,	///< 8 bytes.
		FT_FLOAT,	///< 4 bytes
		FT_INT32,	///< 4 bytes
		FT_UINT32,	///< 4 bytes
		FT_INT16,	///< 2 bytes
		FT_UINT16,	///< 2 bytes
		FT_INT8,	///< 1 bytes
		FT_UINT8,	///< 1 bytes
		FT_BOOL,	///< 1 bytes
		FT_STRING,	///
		FT_BINARY,	///
		FT_ENUM,	///< 1 byte
		FT_USER,	///
	};

	Field():
	type_(Field::FT_INT32),
	userType_(NULL),
	maxArray_(0),
	maxValue_(0)
	{}

	const char* getNameC()	{ return name_.c_str(); }
	bool isArray()			{ return (maxArray_ > 0)?true:false; }

	/** name of the field. */
	std::string		name_;
	/** type of the field. */
	EFieldType		type_;
	/** Enum or Struct reference. */
	Definition*		userType_;
	/** max array size of the field. 0 means single field. */
	size_t			maxArray_;
	/** max value of the field, if it make sense. */
	size_t			maxValue_;
};

/** A container to hold a set of field. */
class FieldContainer
{
public:
	bool findField(const std::string& name)
	{
		for(size_t i = 0; i < fields_.size(); i++)
			if(fields_[i].name_ == name)
				return true;
		return false;
	}
	std::vector<Field>	fields_;
};

class Struct : 
	public Definition,
	public FieldContainer
{
public:
	Struct() {}
	virtual Struct*	getStruct()	{ return this; }
};

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
	bool isEnum16()
	{
		return items_.size() > 256;
	}

	/** Item names. */
	std::vector<std::string>	items_;
};


#endif//__Definition_h__