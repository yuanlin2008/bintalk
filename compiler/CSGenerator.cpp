#include "Options.h"
#include "Context.h"
#include "CodeFile.h"

#include "CodeGenerator.h"
DECLARE_CG(CSGenerator, cs);

static void generateEnum(CodeFile& f, Enum* e)
{
	f.output("public enum %s : %s", e->getNameC(), e->isEnum16()?"ushort":"byte");
	f.indent("{");
	for(size_t i = 0; i < e->items_.size(); i++)
		f.output("%s,", e->items_[i].c_str());
	f.recover("}");

	f.output("namespace bintalk");
	f.indent("{");
	f.output("public static partial class ProtocolReader");
	f.indent("{");
	f.output("public static bool read(bintalk.IReader r, ref %s v, uint maxValue)", e->getNameC());
	f.indent("{");
	f.output("%s e = 0;", e->isEnum16()?"ushort":"byte");
	f.output("if(!read(r, ref e, 0)) return false;");
	f.output("v = (%s)e;", e->getNameC());
	f.output("return true;");
	f.recover("}");
	f.recover("}");
	f.output("public static partial class ProtocolWriter");
	f.indent("{");
	f.output("public static void write(bintalk.IWriter w, %s v)", e->getNameC());
	f.indent("{");
	f.output("write(w, (%s)v);", e->isEnum16()?"ushort":"byte");
	f.recover("}");
	f.recover("}");
	f.recover("}");
}

static const char* getFieldInnerTypeName(Field& f)
{
	switch(f.type_)
	{
	case Field::FT_INT64: return "long"; 
	case Field::FT_UINT64: return "ulong"; 
	case Field::FT_DOUBLE: return "double"; 
	case Field::FT_FLOAT: return "float"; 
	case Field::FT_INT32: return "int"; 
	case Field::FT_UINT32: return "uint"; 
	case Field::FT_INT16: return "short"; 
	case Field::FT_UINT16: return "ushort"; 
	case Field::FT_INT8: return "sbyte"; 
	case Field::FT_UINT8: return "byte"; 
	case Field::FT_BOOL: return "bool"; 
	case Field::FT_STRING: return "string"; 
	case Field::FT_BINARY: return "byte[]"; 
	case Field::FT_USER:
	case Field::FT_ENUM: return f.userType_->getNameC(); 
	}
	return NULL;
}

static void getFieldTypeName(Field& f, std::string& name)
{
	name = getFieldInnerTypeName(f);
	if(f.isArray())
		name = "List<" + name + ">";
}

static void getFieldInnerDefault(Field& f, std::string& name)
{
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
	case Field::FT_UINT8: name = "0"; break;
	case Field::FT_BOOL: name = "false"; break;
	case Field::FT_STRING: name = "\"\""; break;
	case Field::FT_BINARY: name = "new byte[0]"; break;
	case Field::FT_USER: name = "new "; name += f.userType_->name_; name += "()"; break;
	case Field::FT_ENUM: name = "("; name += f.userType_->name_; name += ")(0)"; break;
	}
}
static void getFieldDefault(Field& f, std::string& name)
{
	if(f.isArray())
	{
		name = "new ";
		std::string tn;
		getFieldTypeName(f, tn);
		name += tn;
		name += "()";
	}
	else
		getFieldInnerDefault(f, name);
}

static void generateFieldSCode(CodeFile& f, Field& field)
{
	if(field.isArray())
	{
		f.indent("{");
		f.output("bintalk.ProtocolWriter.writeDynSize(__w__, (uint)%s.Count);", field.getNameC());
		f.output("foreach(%s __vi__ in %s) bintalk.ProtocolWriter.write(__w__, __vi__);",
			getFieldInnerTypeName(field),
			field.getNameC());
		f.recover("}");
	}
	else
		f.output("bintalk.ProtocolWriter.write(__w__, %s);", field.getNameC());
}

static void generateFieldContainerSCode(CodeFile& f, FieldContainer* fc)
{
	for(size_t i = 0; i < fc->fields_.size(); i++)
		generateFieldSCode(f, fc->fields_[i]);
}

static void generateFieldDSCode(CodeFile& f, Field& field)
{
	if(field.isArray())
	{
		f.indent("{");
		f.output("uint __s__;");
		f.output("if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0X%X) return false;", field.maxArray_);
		f.output("for(uint __i__ = 0; __i__ < __s__; __i__++)");
		f.indent("{");
		std::string idft;
		getFieldInnerDefault(field, idft);
		f.output("%s __vi__ = %s;", getFieldInnerTypeName(field), idft.c_str());
        f.output("if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X%X)) return false;", field.maxValue_);
		f.output("%s.Add(__vi__);", field.getNameC());
		f.recover("}");
		f.recover("}");
	}
	else
		f.output("if(!bintalk.ProtocolReader.read(__r__, ref %s, 0X%X)) return false;", field.getNameC(), field.maxValue_);
}

static void generateFieldContainerDSCode(CodeFile& f, FieldContainer* fc)
{
	for(size_t i = 0; i < fc->fields_.size(); i++)
		generateFieldDSCode(f, fc->fields_[i]);
}

static void generateStruct(CodeFile& f, Struct* s)
{
	f.output("public class %s", s->getNameC());
	f.indent("{");
	// fields.
	for(size_t i = 0; i < s->fields_.size(); i++)
	{
		Field& field = s->fields_[i];
		std::string tn;
		getFieldTypeName(field, tn);
		std::string dft;
		getFieldDefault(field, dft);
		f.output("public %s %s= %s;", 
			tn.c_str(),
			field.getNameC(),
			dft.c_str());
	}
	// serialize code.
	f.output("public void serialize(bintalk.IWriter __w__)");
	f.indent("{");
	generateFieldContainerSCode(f, s);
	f.recover("}");
	// deserialize code.
	f.output("public bool deserialize(bintalk.IReader __r__)");
	f.indent("{");
	generateFieldContainerDSCode(f, s);
	f.output("return true;");
	f.recover("}");
	f.recover("}");
	////////////////////////////////////////////////////////////////////////////////
	f.output("namespace bintalk");
	f.indent("{");
	f.output("public static partial class ProtocolReader");
	f.indent("{");
	f.output("public static bool read(bintalk.IReader r, ref %s v, uint maxValue)", s->getNameC());
	f.indent("{");
	f.output("return v.deserialize(r);");
	f.recover("}");
	f.recover("}");
	f.output("public static partial class ProtocolWriter");
	f.indent("{");
	f.output("public static void write(bintalk.IWriter w, %s v)", s->getNameC());
	f.indent("{");
	f.output("v.serialize(w);");
	f.recover("}");
	f.recover("}");
	f.recover("}");
}

void CSGenerator::generate()
{
	CodeFile f(gOptions.output_ + gOptions.inputFS_ + ".cs");
	f.output("using System.Collections.Generic;");
	for(size_t i = 0; i < gContext.definitions_.size(); i++)
	{
		Definition* definition = gContext.definitions_[i];
		if(definition->file_ != gOptions.inputFN_)
			continue;
		if (definition->getEnum())
			generateEnum(f, definition->getEnum());
		else if (definition->getStruct())
			generateStruct(f, definition->getStruct());
	}
}
