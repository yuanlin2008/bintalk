#include "Options.h"
#include "Context.h"
#include "CodeFile.h"

#include "CodeGenerator.h"
DECLARE_CG(CppGenerator, cpp);

static void generateEnumDecl(CodeFile& f, Enum* e)
{
	f.output("enum %s", e->getNameC());
	f.indent("{");
	for(size_t i = 0; i < e->items_.size(); i++)
		f.output("%s,", e->items_[i].c_str());
	f.recover("};");
}

static const char* getFieldCppType(Field& f)
{
	static std::string name;
	switch(f.type_)
	{
	case Field::FT_INT64:	name = "bintalk::INT64"; break;
	case Field::FT_UINT64: name = "bintalk::UINT64"; break;
	case Field::FT_DOUBLE: name = "bintalk::DOUBLE"; break;
	case Field::FT_FLOAT:	name = "bintalk::FLOAT"; break;
	case Field::FT_INT32:	name = "bintalk::INT32"; break;
	case Field::FT_UINT32: name = "bintalk::UINT32"; break;
	case Field::FT_INT16:	name = "bintalk::INT16"; break;
	case Field::FT_UINT16: name = "bintalk::UINT16"; break;
	case Field::FT_INT8:	name = "bintalk::INT8"; break;
	case Field::FT_UINT8:	name = "bintalk::UINT8"; break;
	case Field::FT_BOOL:	name = "bintalk::BOOL"; break;
	case Field::FT_STRING: name = "bintalk::STRING"; break;
	case Field::FT_BINARY: name = "bintalk::BINARY"; break;
	case Field::FT_ENUM:
	case Field::FT_USER:	name = f.userType_->name_; break;
	}
	if(f.isArray()) name = "std::vector<" + name + ">";
	return name.c_str();
}

static const char* getFieldCppParamType(Field& f, bool isS)
{
	static std::string name;
	name = getFieldCppType(f);
	if(f.isArray() || f.type_ == Field::FT_STRING || f.type_ == Field::FT_BINARY || f.type_ == Field::FT_USER)
	{
		if(isS) name = "const " + name;
		name += "&";
	}
	return name.c_str();
}

static const char* getFieldCppDefault(Field& f)
{
	static std::string name;
	if(f.isArray())
		return NULL;
	name = "";
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
	switch(f.type_)
	{
	case Field::FT_INT64:	n = "INT64";break;
	case Field::FT_UINT64:	n = "UINT64";break;
	case Field::FT_DOUBLE: n = "DOUBLE";break;
	case Field::FT_FLOAT:	n = "FLOAT";break;
	case Field::FT_INT32:	n = "INT32";break;
	case Field::FT_UINT32: n = "UINT32";break;
	case Field::FT_INT16:	n = "INT16";break;
	case Field::FT_UINT16:	n = "UINT16";break;
	case Field::FT_INT8:	n = "INT8";break;
	case Field::FT_UINT8:	n = "UINT8";break;
	case Field::FT_BOOL:	n = "BOOL";break;
	case Field::FT_ENUM:	n = "ENUM";break;
	case Field::FT_STRING:	n = "STRING";break;
	case Field::FT_BINARY:	n = "BINARY";break;
	case Field::FT_USER:	n = "USER";break;
	}
	if(f.isArray()) n += "A";
	return n.c_str();
}

static void generateStubMethodDecl(CodeFile& f, Method& m)
{
	f.listBegin(",", false, "void %s(", m.getNameC());
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		f.listItem("%s %s", getFieldCppParamType(field, true), field.getNameC());
	}
	f.listEnd(");");
}

static void generateProxyMethodDecl(CodeFile& f, Method& m)
{
	f.listBegin(",", false, "virtual bool %s(", m.getNameC());
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		f.listItem("%s %s", getFieldCppParamType(field, false), field.getNameC());
	}
	f.listEnd(") = 0;");
}

static void generateStructDecl(CodeFile& f, Struct* s)
{
	if(s->super_)
		f.output("struct %s : public %s", s->getNameC(), s->super_->getNameC());
	else
		f.output("struct %s", s->getNameC());
	f.indent("{");
	for(size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		f.output("%s %s;", getFieldCppType(field), field.getNameC());
	}
	f.output("%s();", s->getNameC());
	f.output("void serialize(bintalk::BinaryWriter* w) const;");
	f.output("bool deserialize(bintalk::BinaryReader* r);");
	f.recover("};");
}

static void generateStubDecl(CodeFile& f, Service* s)
{
	if(s->super_)
		f.output("class %sStub : public %sStub", s->getNameC(), s->super_->getNameC());
	else
		f.output("class %sStub", s->getNameC());
	f.indent("{");
	f.output("public:");
	for(size_t i = 0; i < s->methods_.size(); i++)
		generateStubMethodDecl(f, s->methods_[i]);
	if(!s->super_)
	{
		f.output("protected:");
		f.output("virtual bintalk::BinaryWriter* methodBegin() = 0;");
		f.output("virtual void methodEnd() = 0;");
	}
	f.recover("};");
}

static void generateProxyDecl(CodeFile& f, Service* s)
{
	if(s->super_)
		f.output("class %sProxy : public %sProxy", s->getNameC(), s->super_->getNameC());
	else
		f.output("class %sProxy", s->getNameC());
	f.indent("{");
	f.output("public:");
	f.output("typedef class %sDispatcher Dispatcher;", s->getNameC());
	for(size_t i = 0; i < s->methods_.size(); i++)	
		generateProxyMethodDecl(f, s->methods_[i]);
	f.recover("};");
}

static void generateDispatcherDecl(CodeFile& f, Service* s)
{
	if(s->super_)
		f.output("class %sDispatcher : public %sDispatcher", s->getNameC(), s->super_->getNameC());
	else
		f.output("class %sDispatcher", s->getNameC());
	f.indent("{");
	f.output("public:");
	f.output("typedef class %sProxy Proxy;", s->getNameC());
	f.output("static bool dispatch(bintalk::BinaryReader* reader, %sProxy* p);", s->getNameC());
	f.output("protected:");
	for(size_t i = 0; i < s->methods_.size(); i++)
		f.output("static bool %s(bintalk::BinaryReader* r, %sProxy* p);", s->methods_[i].getNameC(), s->getNameC());
	f.recover("};");
}

static void generateFieldContainerSCode(CodeFile& f, FieldContainer* fc)
{
	for(size_t i = 0; i < fc->fields_.size(); i++)
	{
		Field& field = fc->fields_[i];
		f.output("bintalk::ProtocolWriter::write%s(__w__, %s);",
			getFieldFuncName(field),
			field.getNameC());
	}
}

static void generateFieldContainerDSCode(CodeFile& f, FieldContainer* fc)
{
	for(size_t i = 0; i < fc->fields_.size(); i++)
	{
		Field& field = fc->fields_[i];
		f.output("if(!bintalk::ProtocolReader::read%s(__r__, %s, 0X%X, 0X%X)) return false;",
			getFieldFuncName(field),
			field.getNameC(),
			field.maxArray_,
			field.maxValue_);
	}
}

static void generateStructImp(CodeFile& f, Struct* s)
{
	f.listBegin(",", true, "%s::%s():", s->getNameC(), s->getNameC());
	for(size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		if(!getFieldCppDefault(field))
			continue;
		f.listItem("%s(%s)", field.getNameC(), getFieldCppDefault(field));
	}
	f.listEnd("{}");
	f.output("void %s::serialize(bintalk::BinaryWriter* __w__) const", s->getNameC());
	f.indent("{");
	if(s->super_)
		f.output("%s::serialize(__w__);", s->super_->getNameC());
	generateFieldContainerSCode(f, s);
	f.recover("}");
	f.output("bool %s::deserialize(bintalk::BinaryReader* __r__)", s->getNameC());
	f.indent("{");
	if(s->super_)
		f.output("if(!%s::deserialize(__r__)) return false;",s-> super_->getNameC());
	generateFieldContainerDSCode(f, s);
	f.output("return true;");
	f.recover("}");
}

static void generateStubMethodImp(CodeFile& f, Service*s, Method& m)
{
	f.listBegin(",", false, "void %sStub::%s(", s->getNameC(), m.getNameC());
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		f.listItem("%s %s", getFieldCppParamType(field, true), field.getNameC());
	}
	f.listEnd(")");
	f.indent("{");
	f.output("bintalk::BinaryWriter* __w__ = methodBegin();");
	f.output("bintalk::MID __mid__ = %d;", m.mid_);
	f.output("bintalk::ProtocolWriter::writeMID(__w__, __mid__);");
	generateFieldContainerSCode(f, &m);
	f.output("methodEnd();");
	f.recover("}");
}

static void generateStubImp(CodeFile& f, Service* s)
{
	for(size_t i = 0; i < s->methods_.size(); i++)
		generateStubMethodImp(f, s, s->methods_[i]);
}

static void generateDispatcherMethodImp(CodeFile&f, Method& m, const std::string& svcName)
{
	f.output("bool %sDispatcher::%s(bintalk::BinaryReader* __r__, %sProxy* __p__)", svcName.c_str(), m.getNameC(), svcName.c_str());
	f.indent("{");
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		if(getFieldCppDefault(field))
			f.output("%s %s=%s;", getFieldCppType(field), field.getNameC(), getFieldCppDefault(field));
		else
			f.output("%s %s;", getFieldCppType(field), field.getNameC());
	}
	generateFieldContainerDSCode(f, &m);
	f.listBegin(",", false, "return __p__->%s(", m.getNameC());
	for(size_t i = 0; i < m.fields_.size(); i++)
	{
		Field& field = m.fields_[i];
		f.listItem("%s", field.getNameC());
	}
	f.listEnd(");");
	f.recover("}");
}

static void generateDispatcherImp(CodeFile& f, Service* s)
{
	for(size_t i = 0; i < s->methods_.size(); i++)
		generateDispatcherMethodImp(f, s->methods_[i], s->name_);

	f.output("bool %sDispatcher::dispatch(bintalk::BinaryReader* __r__, %sProxy* __p__)", s->getNameC(), s->getNameC());
	f.indent("{");
	f.output("bintalk::MID __mid__;");
	f.output("if(!bintalk::ProtocolReader::readMID(__r__, __mid__)) return false;");
	f.output("switch(__mid__)");
	f.indent("{");
	std::vector<Method*> methods;
	s->getAllMethods(methods);
	for(size_t i = 0; i < methods.size(); i++)
	{
		Method* method = methods[i];
		f.output("case %d:{if(!%s(__r__, __p__)) return false;}break;", i, method->getNameC());
	}
	f.output("default: return false;");
	f.recover("}");
	f.output("return true;");
	f.recover("}");
}

static void generateHFile()
{
	std::string fn = gOptions.output_ + gOptions.inputFS_ + ".h";
	CodeFile f(fn);
	f.output("#ifndef __%s_h__", gOptions.inputFS_.c_str());
	f.output("#define __%s_h__", gOptions.inputFS_.c_str());
	f.output("#include \"bintalk/ProtocolWriter.h\"");
	f.output("#include \"bintalk/ProtocolReader.h\"");
	for(size_t i = 0; i < gContext.rootImported_.size(); i++)
	{
		std::string incFN = gContext.rootImported_[i];
		incFN = incFN.substr(0,incFN.find('.'));
		f.output("#include \"%s.h\"", incFN.c_str());
	}
	for(size_t i = 0; i < gContext.definitions_.size(); i++)
	{
		Definition* definition = gContext.definitions_[i];
		if(definition->file_ != gOptions.inputFN_)
			continue;
		if (definition->getEnum())
			generateEnumDecl(f, definition->getEnum());
		else if (definition->getStruct())
			generateStructDecl(f, definition->getStruct());
		else if (definition->getService())
		{
			generateStubDecl(f, definition->getService());
			generateProxyDecl(f, definition->getService());
			generateDispatcherDecl(f, definition->getService());
		}
	}
	f.output("#endif");
}

static void generateCPPFile()
{
	CodeFile f(gOptions.output_ + gOptions.inputFS_ + ".cpp");
	f.output("#include \"%s.h\"", gOptions.inputFS_.c_str());
	for(size_t i = 0; i < gContext.definitions_.size(); i++)
	{
		Definition* definition = gContext.definitions_[i];
		if(definition->file_ != gOptions.inputFN_)
			continue;
		if (definition->getStruct())
			generateStructImp(f, definition->getStruct());
		else if (definition->getService())
		{
			generateStubImp(f, definition->getService());
			generateDispatcherImp(f, definition->getService());
		}
	}
}

void CppGenerator::generate()
{
	generateHFile();
	generateCPPFile();
}
