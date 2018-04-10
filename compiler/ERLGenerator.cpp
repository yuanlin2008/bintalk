#include "Options.h"
#include "Context.h"
#include "CodeFile.h"

#include "CodeGenerator.h"
DECLARE_CG(ERLGenerator, erl);

static void generateEnumHRL(CodeFile& f, Enum* e)
{
	f.output("%%%% enum %s", e->getNameC());
	for(size_t i = 0; i < e->items_.size(); i++)
		f.output("-define(%s_%s, %d).", e->getNameC(), e->items_[i].c_str(), i);
}

static const char* getAtom(const char* name)
{
	static std::string s[8];
	static int id = 0;
	std::string& str = s[id];
	if (isupper(name[0]))
	{
		str = "\'";
		str += name;
		str += "\'";
	}
	else
		str = name;
	id++;
	if (id >= 8)
		id = 0;
	return str.c_str();
}

static const char* getFieldTypeName(Field& f)
{
	switch(f.type_)
	{
	case Field::FT_INT64:	return "int64";
	case Field::FT_UINT64: return "uint64";
	case Field::FT_DOUBLE: return "double";
	case Field::FT_FLOAT:	return "float";
	case Field::FT_INT32:	return "int32";
	case Field::FT_UINT32: return "uint32";
	case Field::FT_INT16:	return "int16";
	case Field::FT_UINT16: return "uint16";
	case Field::FT_INT8:	return "int8";
	case Field::FT_UINT8:	return "uint8";
	case Field::FT_BOOL:	return "bool";
	case Field::FT_STRING:	return "string";
	case Field::FT_USER:	return "struct";
	case Field::FT_BINARY:	return "binary";
	case Field::FT_ENUM:	return f.userType_->getEnum()->isEnum16()?"enum16":"enum";
	}
	return "";
}

static const char* getFieldVal(Field& f)
{
	static std::string s;
	s = "0";
	if (f.type_ == Field::FT_STRING || f.type_ == Field::FT_BINARY || f.type_ == Field::FT_ENUM)
	{
		char temp[64];
		sprintf(temp, "16#%X", f.maxValue_);
		s = temp;
	}
	else if (f.type_ == Field::FT_USER)
		s = f.userType_->getNameC();
	return s.c_str();
}

static const char* getFieldDefault(Field& f)
{
	static std::string def;
	if(f.isArray())
		return "[]";
	switch(f.type_)
	{
	case Field::FT_INT64:
	case Field::FT_UINT64:
	case Field::FT_DOUBLE:
	case Field::FT_FLOAT:
	case Field::FT_INT32:
	case Field::FT_UINT32:
	case Field::FT_INT16:
	case Field::FT_UINT16:
	case Field::FT_INT8:
	case Field::FT_ENUM:
	case Field::FT_UINT8:	return "0";
	case Field::FT_BOOL:	return "false";
	case Field::FT_STRING:	return "<<>>";
	case Field::FT_BINARY:	return "<<>>";
	case Field::FT_USER:	
		def = "#"; 
		def += getAtom(f.userType_->name_.c_str()); 
		def += "{}"; break;
	}
	return def.c_str();
}

static const char* getFieldErlType(Field& f)
{
	static std::string s;
	s = "";
	if (f.isArray())
		s = "[";
	switch(f.type_)
	{
	case Field::FT_DOUBLE:
	case Field::FT_FLOAT:
		s += "integer()|float()"; break;
	case Field::FT_UINT64:
	case Field::FT_UINT32:
	case Field::FT_UINT16:
	case Field::FT_UINT8:	
	case Field::FT_ENUM:
		s += "non_neg_integer()"; break;
	case Field::FT_INT64:
	case Field::FT_INT32:
	case Field::FT_INT16:
	case Field::FT_INT8:
		s += "integer()"; break;
	case Field::FT_BOOL:
		s += "boolean()"; break;
	case Field::FT_STRING:
		s += "binary()"; break;
	case Field::FT_BINARY:
		s += "binary()"; break;
	case Field::FT_USER:
		s += "#";
		s += getAtom(f.userType_->name_.c_str());
		s += "{}";
		break;
	}
	if (f.isArray())
		s += "]";
	return s.c_str();
}

static void generateStructHRL(CodeFile& f, Struct* s)
{
	f.output("%%%% struct %s", s->getNameC());
	f.listBegin(",", true, "-record(%s, {", getAtom(s->getNameC()));
	for(size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		f.listItem("%s = %s::%s", getAtom(field.getNameC()), getFieldDefault(field), getFieldErlType(field));
	}
	f.listEnd("}).");
}

static void generateStructSerializeCode(CodeFile& f, Struct* s)
{
	f.output("%% serialize");
	f.output("-spec serialize(#%s{})->iolist().", getAtom(s->getNameC()));
	f.output("serialize(S) ->");
	f.indent();
	f.listBegin(",", true, "[");
	for(size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		if (field.isArray())
			f.listItem("bintalk_prot_writer:write_array(write_%s, S#%s.%s)", 
			getFieldTypeName(field),
			getAtom(s->getNameC()),
			getAtom(field.getNameC()));
		else 
			f.listItem("bintalk_prot_writer:write_%s(S#%s.%s)",
			getFieldTypeName(field),
			getAtom(s->getNameC()),
			getAtom(field.getNameC()));
	}
	f.listEnd("].");
	f.recover();
}

static void generateStructDeserializeCode(CodeFile& f, Struct* s)
{
	f.output("%% deserialize");
	f.output("-spec deserialize(binary())->{#%s{}, binary()}.", getAtom(s->getNameC()));
	f.output("deserialize(B0)->");
	int bNum = 0;
	f.indent();
	for(size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		if (field.isArray())
			f.output("{V_%s, B%d} = bintalk_prot_reader:read_array(read_%s, 16#%X, %s, B%d), ",
				field.getNameC(),
				bNum + 1,
				getFieldTypeName(field),
				field.maxArray_,
				getFieldVal(field),
				bNum);
		else
			f.output("{V_%s, B%d} = bintalk_prot_reader:read_%s(%s, B%d), ",
				field.getNameC(),
				bNum + 1,
				getFieldTypeName(field),
				getFieldVal(field),
				bNum);
		bNum++;
	}
	f.listBegin(",", true, "{#%s{", getAtom(s->getNameC()));
	for(size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		f.listItem("%s = V_%s", getAtom(field.getNameC()), field.getNameC());
	}
	f.listEnd("}, B%d}.", bNum);
	f.recover();
}

static void generateStructModule(Struct* s)
{
	CodeFile f(gOptions.output_ + s->name_ + ".erl");
	f.output("-module(%s).", getAtom(s->getNameC()));
	f.output("-include(\"%s.hrl\").", gOptions.inputFS_.c_str());
	f.output("-export([serialize/1, deserialize/1]).");
	// serialize.
	generateStructSerializeCode(f, s);
	// deserialize.
	generateStructDeserializeCode(f, s);
}

static void generateServiceStubMethod(CodeFile& f, Method& m)
{
	f.listBegin(",", false, "-spec %s(", getAtom(m.getNameC()));
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		f.listItem("%s", getFieldErlType(field));
	}
	f.listEnd(")->iolist().");
	f.listBegin(",", false, "%s(", getAtom(m.getNameC()));
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		f.listItem("V_%s", field.getNameC());
	}
	f.listEnd(") ->");
	f.indent();
	f.listBegin(",", true, "[");
	f.listItem("bintalk_prot_writer:write_mid(%d)", m.mid_);
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		if (field.isArray())
			f.listItem("bintalk_prot_writer:write_array(write_%s, V_%s)", 
			getFieldTypeName(field),
			getAtom(field.getNameC()));
		else 
			f.listItem("bintalk_prot_writer:write_%s(V_%s)",
			getFieldTypeName(field),
			getAtom(field.getNameC()));
	}
	f.listEnd("].");
	f.recover();
}

static void generateServiceStubModule(Service* s)
{
	CodeFile f(gOptions.output_ + s->name_ + "_stub.erl");
	f.output("-module(%s_stub).", getAtom(s->getNameC()));
	f.output("-include(\"%s.hrl\").", gOptions.inputFS_.c_str());
	f.listBegin(",", true, "-export([");
	for(size_t i = 0; i < s->methods_.size(); i++)
	{
		Method& method = s->methods_[i];
		f.listItem("%s/%d", getAtom(method.getNameC()), method.fields_.size());
	}
	f.listEnd("]).");

	for(size_t i = 0; i < s->methods_.size(); i++)
		generateServiceStubMethod(f, s->methods_[i]);
}

static void generateServiceCallback(CodeFile& f, Method& m)
{
	f.listBegin(",", false, "-callback %s(", getAtom(m.getNameC()));
	for (size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		f.listItem("%s", getFieldErlType(field));
	}
	f.listEnd(")->term().");
}

static void generateServiceDispatcherMethod(CodeFile& f, Method& m, bool isLast)
{
	f.output("dispatch(%d, B0, M, S) ->", m.mid_);
	f.indent();
	int bNum = 0;
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		if (field.isArray())
			f.output("{V_%s, B%d} = bintalk_prot_reader:read_array(read_%s, 16#%X, %s, B%d), ",
				field.getNameC(),
				bNum + 1,
				getFieldTypeName(field),
				field.maxArray_,
				getFieldVal(field),
				bNum);
		else
			f.output("{V_%s, B%d} = bintalk_prot_reader:read_%s(%s, B%d), ",
				field.getNameC(),
				bNum + 1,
				getFieldTypeName(field),
				getFieldVal(field),
				bNum);
		bNum++;
	}
	f.listBegin(",", false, "S1 = M:%s(", getAtom(m.getNameC()));
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		f.listItem("V_%s", field.getNameC());
	}
	f.listItem("S");
	f.listEnd("),");
	f.output("{B%d, S1}%s", bNum, isLast?".":";");
	f.recover();
}

static void generateServiceDispatcherModule(CodeFile& hrlFile, Service* s)
{
	hrlFile.output("%%%% %s mid", s->getNameC());
	for(size_t i = 0; i < s->methods_.size(); i++)
		hrlFile.output("-define(%s_%s, %d).", 
		s->getNameC(), 
		s->methods_[i].name_.c_str(), 
		s->methods_[i].mid_);

	CodeFile f(gOptions.output_ + s->name_ + "_dispatcher.erl");
	f.output("-module(%s_dispatcher).", getAtom(s->getNameC()));
	f.output("-include(\"%s.hrl\").", gOptions.inputFS_.c_str());
	f.output("-export([dispatch/3]).");

	f.output("%%%% callbacks.");
	f.output("-callback filter_method(non_neg_integer(), term())->{boolean(), term()}.");
	for(size_t i = 0; i < s->methods_.size(); i++)
	{ 
		Method& m = s->methods_[i];
		f.listBegin(",", false, "-callback %s(", getAtom(m.getNameC()));
		for (size_t fi = 0; fi < m.fields_.size(); fi++)
		{
			Field& field = m.fields_[fi];
			f.listItem("%s", getFieldErlType(field));
		}
		f.listItem("term()");
		f.listEnd(")->term().");
	}

	if(s->methods_.size())
	{
		f.output("%%%% dispatch.");
		f.output("dispatch(B, M, S)->");
		f.indent();
		f.output("{MID, BR} = bintalk_prot_reader:read_mid(B),");
		f.output("{F, S1} = M:filter_method(MID, S),");
		f.output("case F of");
		f.indent();
		f.output("true -> {BR, S1};");
		f.output("false -> dispatch(MID, BR, M, S1)");
		f.recover();
		f.output("end.");
		f.recover();
		for(size_t i = 0; i < s->methods_.size(); i++)
			generateServiceDispatcherMethod(f, s->methods_[i], i == s->methods_.size()-1);
	}
	else
	{
		// no method yet.
		f.output("%%%% dispatch.");
		f.output("dispatch(B, _M, S)->");
		f.indent();
		f.output("{B, S}.");
		f.recover();
	}
}

void ERLGenerator::generate()
{
	CodeFile hrlFile(gOptions.output_ + gOptions.inputFS_ + ".hrl");
	hrlFile.output("-ifndef(%s_HRL).", gOptions.inputFS_.c_str());
	hrlFile.output("-define(%s_HRL, true).", gOptions.inputFS_.c_str());
	for(size_t i = 0; i < gContext.rootImported_.size(); i++)
	{
		std::string incFilename = gContext.rootImported_[i];
		incFilename = incFilename.substr(0,incFilename.find('.'));
		hrlFile.output("-include(\"%s.hrl\").", incFilename.c_str());
	}
	for(size_t i = 0; i < gContext.definitions_.size(); i++)
	{
		Definition* definition = gContext.definitions_[i];
		if(definition->file_ != gOptions.inputFN_)
			continue;
		if (definition->getEnum())
			generateEnumHRL(hrlFile, definition->getEnum());
		else if (definition->getStruct())
		{
			generateStructHRL(hrlFile, definition->getStruct());
			generateStructModule(definition->getStruct());
		}
		else if (definition->getService())
		{
			generateServiceStubModule(definition->getService());
			generateServiceDispatcherModule(hrlFile, definition->getService());
		}
	}

	hrlFile.output("-endif.");
}