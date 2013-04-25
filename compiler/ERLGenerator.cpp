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
	case Field::FT_USER:	return f.userType_->getNameC();
	case Field::FT_BINARY:	return "binary";
	case Field::FT_ENUM:	return "enum";
	}
	return "";
}

static size_t getFieldValMax(Field& f)
{
	if(f.type_ == Field::FT_STRING || f.type_ == Field::FT_BINARY || f.type_ == Field::FT_ENUM)
		return f.maxValue_;
	return 0;
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
	case Field::FT_STRING:	return "\"\"";
	case Field::FT_BINARY:	return "<<>>";
	case Field::FT_USER:	def = "#\'" + f.userType_->name_ + "\'{}"; break;
	}
	return def.c_str();
}

static void generateStructHRL(CodeFile& f, Struct* s)
{
	f.output("%%%% struct %s", s->getNameC());
	std::vector<Field*> fields;
	s->getAllFields(fields);
	f.listBegin(",", true, "-record(\'%s\', {", s->getNameC());
	for(size_t i = 0; i < fields.size(); i++)
	{
		Field* field = fields[i];
		f.listItem("\'%s\' = %s", field->getNameC(), getFieldDefault(*field));
	}
	f.listEnd("}).");
}

static void generateStructSerializeCode(CodeFile& f, Struct* s)
{
	f.output("%% serialize");
	f.output("serialize(S = #\'%s\'{}) ->", s->getNameC());
	f.indent();
	std::vector<Field*> fields;
	s->getAllFields(fields);
	f.listBegin(",", true, "[");
	for(size_t i = 0; i < fields.size(); i++)
	{
		Field* field = fields[i];
		f.listItem("bintalk_prot_writer:write(\'%s\', %s, S#\'%s\'.\'%s\')",
			getFieldTypeName(*field),
			field->isArray()?"true":"false",
			s->getNameC(),
			field->getNameC());
	}
	f.listEnd("].");
	f.recover();
}

static void generateStructDeserializeCode(CodeFile& f, Struct* s)
{
	f.output("%% deserialize");
	f.output("deserialize(B0) when is_binary(B0) ->");
	int bNum = 0;
	std::vector<Field*> fields;
	s->getAllFields(fields);
	f.indent();
	for(size_t i = 0; i < fields.size(); i++)
	{
		Field* field = fields[i];
		f.output("{V_%s, B%d} = bintalk_prot_reader:read(\'%s\', 16#%X, 16#%X, B%d), ",
			field->getNameC(),
			bNum + 1,
			getFieldTypeName(*field),
			field->maxArray_,
			getFieldValMax(*field),
			bNum);
		bNum++;
	}
	f.listBegin(",", true, "{#\'%s\'{", s->getNameC());
	for(size_t i = 0; i < fields.size(); i++)
	{
		Field* field = fields[i];
		f.listItem("\'%s\' = V_%s", field->getNameC(), field->getNameC());
	}
	f.listEnd("}, B%d}.", bNum);
	f.recover();
}

static void generateStructModule(Struct* s)
{
	CodeFile f(gOptions.output_ + s->name_ + ".erl");
	f.output("-module(\'%s\').", s->getNameC());
	f.output("-include(\"%s.hrl\").", gOptions.inputFS_.c_str());
	f.output("-export([serialize/1, deserialize/1]).");
	// serialize.
	generateStructSerializeCode(f, s);
	// deserialize.
	generateStructDeserializeCode(f, s);
}

static void generateServiceStubMethod(CodeFile& f, Method* m)
{
	f.listBegin(",", false, "\'%s\'(", m->getNameC());
	for(size_t i = 0; i < m->fields_.size(); i++)
	{
		Field& field = m->fields_[i];
		f.listItem("V_%s", field.getNameC());
	}
	f.listEnd(") ->");
	f.indent();
	f.listBegin(",", true, "[");
	f.listItem("bintalk_prot_writer:write_mid(%d)", m->mid_);
	for(size_t i = 0; i < m->fields_.size(); i++)
	{
		Field& field = m->fields_[i];
		f.listItem("bintalk_prot_writer:write(\'%s\', %s, V_%s)",
			getFieldTypeName(field),
			field.isArray()?"true":"false",
			field.getNameC());
	}
	f.listEnd("].");
	f.recover();
}

static void generateServiceStubModule(Service* s)
{
	std::vector<Method*> methods;
	s->getAllMethods(methods);

	CodeFile f(gOptions.output_ + s->name_ + "_stub.erl");
	f.output("-module(\'%s_stub\').", s->getNameC());
	f.output("-include(\"%s.hrl\").", gOptions.inputFS_.c_str());
	f.listBegin(",", true, "-export([");
	for(size_t i = 0; i < methods.size(); i++)
	{
		Method* method = methods[i];
		f.listItem("\'%s\'/%d", method->getNameC(), method->fields_.size());
	}
	f.listEnd("]).");

	for(size_t i = 0; i < methods.size(); i++)
		generateServiceStubMethod(f, methods[i]);
}

static void generateServiceProxyMethod(CodeFile& f, Method* m, bool isLast)
{
	f.output("dispatch(%d, B0, M, S) ->", m->mid_);
	f.indent();
	int bNum = 0;
	for(size_t i = 0; i < m->fields_.size(); i++)
	{
		Field& field = m->fields_[i];
		f.output("{V_%s, B%d} = bintalk_prot_reader:read(\'%s\', 16#%X, 16#%X, B%d),",
			field.getNameC(),
			bNum + 1,
			getFieldTypeName(field),
			field.maxArray_,
			getFieldValMax(field),
			bNum);
		bNum++;
	}
	f.listBegin(",", false, "S1 = M:\'%s\'(", m->getNameC());
	for(size_t i = 0; i < m->fields_.size(); i++)
	{
		Field& field = m->fields_[i];
		f.listItem("V_%s", field.getNameC());
	}
	f.listItem("S");
	f.listEnd("),");
	f.output("{B%d, S1}%s", bNum, isLast?".":";");
	f.recover();
}

static void generateServiceProxyModule(Service* s)
{
	std::vector<Method*> methods;
	s->getAllMethods(methods);

	CodeFile f(gOptions.output_ + s->name_ + "_proxy.erl");
	f.output("-module(\'%s_proxy\').", s->getNameC());
	f.output("-include(\"%s.hrl\").", gOptions.inputFS_.c_str());
	f.output("-export([behaviour_info/1, dispatch/3]).");

	f.output("%%%% callbacks.");
	f.output("behaviour_info(callbacks) ->");
	f.indent();
	f.listBegin(",", true, "[");
	for(size_t i = 0; i < methods.size(); i++)
	{
		Method* method = methods[i];
		f.listItem("{%s, %d}", 
			method->getNameC(), 
			method->fields_.size() + 1);
	}
	f.listEnd("];");
	f.recover();
	f.output("behaviour_info(_) -> undefined.");

	if(methods.size())
	{
		f.output("%%%% dispatch.");
		f.output("dispatch(B, M, S) when is_binary(B) ->");
		f.indent();
		f.output("{MID, BR} = bintalk_prot_reader:read_mid(B),");
		f.output("dispatch(MID, BR, M, S).");
		f.recover();
		for(size_t i = 0; i < methods.size(); i++)
			generateServiceProxyMethod(f, methods[i], i == methods.size()-1);
	}
	else
	{
		// no method yet.
		f.output("%%%% dispatch.");
		f.output("dispatch(B, _M, S) when is_binary(B) ->");
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
			generateServiceProxyModule(definition->getService());
		}
	}

	hrlFile.output("-endif.");
}