#include "Options.h"
#include "Context.h"
#include "CodeFile.h"

#include "CodeGenerator.h"
DECLARE_CG(UE4Generator, ue4);

static void generateEnumDecl(CodeFile& f, Enum* e)
{
	f.output("enum %s", e->getNameC());
	f.indent("{");
	for (size_t i = 0; i < e->items_.size(); i++)
		f.output("%s,", e->items_[i].c_str());
	f.recover("};");
}

static const char* getFieldCppType(Field& f)
{
	static std::string name;
	switch (f.type_)
	{
	case Field::FT_INT64:	name = "int64"; break;
	case Field::FT_UINT64: name = "uint64"; break;
	case Field::FT_DOUBLE: name = "double"; break;
	case Field::FT_FLOAT:	name = "float"; break;
	case Field::FT_INT32:	name = "int32"; break;
	case Field::FT_UINT32: name = "uint32"; break;
	case Field::FT_INT16:	name = "int16"; break;
	case Field::FT_UINT16: name = "uint16"; break;
	case Field::FT_INT8:	name = "int8"; break;
	case Field::FT_UINT8:	name = "uint8"; break;
	case Field::FT_BOOL:	name = "bool"; break;
	case Field::FT_STRING: name = "FString"; break;
	case Field::FT_BINARY: name = "TArray<uint8>"; break;
	case Field::FT_ENUM:
	case Field::FT_USER:	name = f.userType_->name_; break;
	}
	if (f.isArray()) name = "TArray<" + name + ">";
	return name.c_str();
}

static const char* getFieldCppDefault(Field& f)
{
	static std::string name;
	if (f.isArray())
		return NULL;
	name = "";
	switch (f.type_)
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
	case Field::FT_UINT8:	name = "0"; break;
	case Field::FT_BOOL:	name = "false"; break;
	case Field::FT_ENUM:	name = "(" + f.userType_->name_ + ")(0)"; break;
	case Field::FT_STRING:
	case Field::FT_BINARY:
	case Field::FT_USER: return NULL;
	}
	return name.c_str();
}

static const char* getFieldFuncName(Field& f)
{
	static std::string n;
	switch (f.type_)
	{
	case Field::FT_INT64:	n = "int64"; break;
	case Field::FT_UINT64:	n = "uint64"; break;
	case Field::FT_DOUBLE: n = "double"; break;
	case Field::FT_FLOAT:	n = "float"; break;
	case Field::FT_INT32:	n = "int32"; break;
	case Field::FT_UINT32: n = "uint32"; break;
	case Field::FT_INT16:	n = "int16"; break;
	case Field::FT_UINT16:	n = "uint16"; break;
	case Field::FT_INT8:	n = "int8"; break;
	case Field::FT_UINT8:	n = "uint8"; break;
	case Field::FT_BOOL:	n = "bool"; break;
	case Field::FT_ENUM:	n = f.userType_->getEnum()->isEnum16() ? "ENUM16" : "ENUM"; break;
	case Field::FT_STRING:	n = "FString"; break;
	case Field::FT_BINARY:	n = "Binary"; break;
	case Field::FT_USER:	n = "USER"; break;
	}
	if (f.isArray()) n += "A";
	return n.c_str();
}

static void generateStructDecl(CodeFile& f, Struct* s)
{
	f.output("struct %s", s->getNameC());
	f.indent("{");
	for (size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		f.output("%s %s;", getFieldCppType(field), field.getNameC());
	}
	f.output("%s();", s->getNameC());
	f.output("void serialize(Bintalk::BinaryWriter* w) const;");
	f.output("bool deserialize(Bintalk::BinaryReader* r);");
	f.recover("};");
}

static void generateFieldContainerSCode(CodeFile& f, FieldContainer* fc)
{
	for (size_t i = 0; i < fc->fields_.size(); i++)
	{
		Field& field = fc->fields_[i];
		f.output("__w__->write%s(%s);", getFieldFuncName(field), field.getNameC());
	}
}

static void generateFieldContainerDSCode(CodeFile& f, FieldContainer* fc)
{
	for (size_t i = 0; i < fc->fields_.size(); i++)
	{
		Field& field = fc->fields_[i];
		f.output("if(!__r__->read%s(%s, 0X%X, 0X%X)) return false;", getFieldFuncName(field), field.getNameC(), field.maxArray_, field.maxValue_);
	}
}

static void generateStructImp(CodeFile& f, Struct* s)
{
	f.output("%s::%s()", s->getNameC(), s->getNameC());
	f.indent("{");
	for (size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		if (!getFieldCppDefault(field))
			continue;
		f.output("%s = %s;", field.getNameC(), getFieldCppDefault(field));
	}
	f.recover("}");
	f.output("void %s::serialize(Bintalk::BinaryWriter* __w__) const", s->getNameC());
	f.indent("{");
	generateFieldContainerSCode(f, s);
	f.recover("}");
	f.output("bool %s::deserialize(Bintalk::BinaryReader* __r__)", s->getNameC());
	f.indent("{");
	generateFieldContainerDSCode(f, s);
	f.output("return true;");
	f.recover("}");
}

static void generateHFile()
{
	std::string fn = gOptions.output_ + gOptions.inputFS_ + ".h";
	CodeFile f(fn);
	f.output("#pragma once");
	f.output("#include \"Bintalk.h\"");
	for (size_t i = 0; i < gContext.rootImported_.size(); i++)
	{
		std::string incFN = gContext.rootImported_[i];
		incFN = incFN.substr(0, incFN.find('.'));
		f.output("#include \"%s.h\"", incFN.c_str());
	}
	for (size_t i = 0; i < gContext.definitions_.size(); i++)
	{
		Definition* definition = gContext.definitions_[i];
		if (definition->file_ != gOptions.inputFN_)
			continue;
		if (definition->getEnum())
			generateEnumDecl(f, definition->getEnum());
		else if (definition->getStruct())
			generateStructDecl(f, definition->getStruct());
	}
}

static void generateCPPFile()
{
	CodeFile f(gOptions.output_ + gOptions.inputFS_ + ".cpp");
	f.output("#include \"%s.h\"", gOptions.inputFS_.c_str());
	for (size_t i = 0; i < gContext.definitions_.size(); i++)
	{
		Definition* definition = gContext.definitions_[i];
		if (definition->file_ != gOptions.inputFN_)
			continue;
		if (definition->getStruct())
			generateStructImp(f, definition->getStruct());
	}
}

void UE4Generator::generate()
{
	generateHFile();
	generateCPPFile();
} 