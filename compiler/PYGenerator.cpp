#include "Options.h"
#include "Context.h"
#include "CodeFile.h"

#include "CodeGenerator.h"
DECLARE_CG(PYGenerator, py);

static void generateEnum(CodeFile& f, Enum* e)
{
	f.output("class %s(object):", e->getNameC());
	f.indent();
	for(size_t i = 0; i < e->items_.size(); i++)
		f.output("%s = %d", e->items_[i].c_str(), i);
	f.recover();
}
static const char* getFieldDefault(Field& f)
{
	static std::string name;
	if(f.isArray())
		name = "[]";
	else if(f.type_ == Field::FT_BOOL)
		name = "False";
	else if(f.type_ == Field::FT_STRING)
		name = "\"\"";
	else if(f.type_ == Field::FT_BINARY)
		name = "\"\"";
	else if(f.type_ == Field::FT_USER)
		name = f.userType_->name_ + "()";
	else
		name = "0";
	return name.c_str();
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
	case Field::FT_ENUM:	return "enum";
	case Field::FT_BINARY:	return "binary";
	}
	return "";
}

static void generateFieldContainerSCode(CodeFile& f, FieldContainer* fc, bool self)
{
	for(size_t i = 0; i < fc->fields_.size(); i++)
	{
		Field& field = fc->fields_[i];
		f.output("protocol_writer.write(protocol_writer.type_%s, %s, %s%s, __b__)",
			getFieldTypeName(field),
			field.isArray()?"True":"False",
			self?"self.":"",
			field.getNameC()
			);
	}
}
static void generateFieldContainerDSCode(CodeFile& f, FieldContainer* fc, bool self)
{
	for(size_t i = 0; i < fc->fields_.size(); i++)
	{
		Field& field = fc->fields_[i];
		f.output("%s%s, __p__= protocol_reader.read(__b__, __p__, protocol_reader.type_%s, 0X%X, 0X%X)",
			self?"self.":"",
			field.getNameC(),
			getFieldTypeName(field),
			field.maxArray_,
			field.maxValue_
			);
	}
}

static void generateStruct(CodeFile& f, Struct* s)
{
	f.output("class %s(object):", s->getNameC());
	f.indent();
	f.output("def __init__(self):");
	f.indent();
	for(size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		f.output("self.%s = %s", field.getNameC(), getFieldDefault(field));
	}
	f.recover();
	f.output("def serialize(self, __b__):");
	f.indent();
	generateFieldContainerSCode(f, s, true);
	f.recover();
	f.output("def deserialize(self, __b__, __p__):");
	f.indent();
	generateFieldContainerDSCode(f, s, true);
	f.output("return __p__");
	f.recover();
	f.recover();
	f.output("def %sWriter(v, b):", s->getNameC());
	f.indent();
	f.output("v.serialize(b)");
	f.recover();
	f.output("protocol_writer.type_%s = %sWriter", s->getNameC(), s->getNameC());
	f.output("def %sReader(b, p, valMax):", s->getNameC());
	f.indent();
	f.output("v = %s()", s->getNameC());
	f.output("p = v.deserialize(b, p)");
	f.output("return v, p");
	f.recover();
	f.output("protocol_reader.type_%s = %sReader", s->getNameC(), s->getNameC());
}

static void generateServiceStubMethod(CodeFile& f, Method& m)
{
	f.listBegin(",", false, "def %s(", m.getNameC());
	f.listItem("self");
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		f.listItem("%s", field.getNameC());
	}
	f.listEnd("):");
	f.indent();
	f.output("__b__ = []");
	f.output("protocol_writer.write_mid(%d, __b__)", m.mid_);
	generateFieldContainerSCode(f, &m, false);
	f.output("self.call(__b__)");
	f.recover();
}

static void generateServiceStub(CodeFile& f, Service* s)
{
	f.output("class %sStub(object):", s->getNameC());
	f.indent();
	for(size_t i = 0; i < s->methods_.size(); i++)
		generateServiceStubMethod(f, s->methods_[i]);
	f.recover();
}

static void generateServiceDispatcherMethod(CodeFile& f, Service* s, Method& m)
{
	f.output("def %s_%s(__b__, __p__, __proxy__):", s->getNameC(), m.getNameC());
	f.indent();
	generateFieldContainerDSCode(f, &m, false);
	f.listBegin(",", false, "__proxy__.%s(", m.getNameC());
	for(size_t i = 0; i < m.fields_.size(); i++)
		f.listItem("%s", m.fields_[i].getNameC());
	f.listEnd(")");
	f.output("return __p__");
	f.recover();
	f.output("%sDispatcher.append(%s_%s)", s->getNameC(), s->getNameC(), m.getNameC());
}

static void generateServiceDispatcher(CodeFile& f, Service* s)
{
	f.output("%sDispatcher = []", s->getNameC());
	for(size_t i = 0; i < s->methods_.size(); i++)
		generateServiceDispatcherMethod(f, s, s->methods_[i]);
	f.output("def dispatch%s(__b__, __p__, __proxy__):", s->getNameC());
	f.indent();
	f.output("__id__, __p__ = protocol_reader.read_mid(__b__, __p__)");
	f.output("return %sDispatcher[__id__](__b__, __p__, __proxy__)", s->getNameC());
	f.recover();
}

static void generateService(CodeFile& f, Service* s)
{
	generateServiceStub(f, s);
	generateServiceDispatcher(f, s);
}

void PYGenerator::generate()
{
	std::string fn = gOptions.output_ + gOptions.inputFS_ + ".py";
	CodeFile f(fn);
	f.output("from bintalk import protocol_writer");
	f.output("from bintalk import protocol_reader");
	for(std::set<std::string>::iterator iter = gContext.imported_.begin();
		iter != gContext.imported_.end(); ++iter)
	{
		std::string incFilename = *iter;
		incFilename = incFilename.substr(0,incFilename.find('.'));
		f.output("from %s import *", incFilename.c_str());
	}
	for(size_t i = 0; i < gContext.definitions_.size(); i++)
	{
		Definition* definition = gContext.definitions_[i];
		if(definition->file_ != gOptions.inputFN_)
			continue;
		if (definition->getEnum())
			generateEnum(f, definition->getEnum());
		else if (definition->getStruct())
			generateStruct(f, definition->getStruct());
		else if (definition->getService())
			generateService(f, definition->getService());
	}
}