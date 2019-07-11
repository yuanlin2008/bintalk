#include "Options.h"
#include "Context.h"
#include "CodeFile.h"

#include "CodeGenerator.h"
DECLARE_CG(LUAGenerator, lua);

static void generateEnum(CodeFile& f, Enum* e)
{
    const char* name = e->getNameC();
    f.output("local %s = {", name);
    f.indent();
    for (size_t i = 0; i < e->items_.size(); i++)
        f.output("%s = %d,", e->items_[i].c_str(), i);
    f.recover();
    f.output("}");
    f.output("%s = { _enum = %s }", name, name);
    f.output("BintalkTypes.%s = setmetatable(%s, BintalkTypes._enum_mt)", name, name);
}

static const char* getFieldTypeName(Field& f)
{
    switch (f.type_)
    {
    case Field::FT_INT64:   return "int64";
    case Field::FT_UINT64:  return "uint64";
    case Field::FT_DOUBLE:  return "double";
    case Field::FT_FLOAT:   return "float";
    case Field::FT_INT32:   return "int32";
    case Field::FT_UINT32:  return "uint32";
    case Field::FT_INT16:   return "int16";
    case Field::FT_UINT16:  return "uint16";
    case Field::FT_INT8:    return "int8";
    case Field::FT_UINT8:   return "uint8";
    case Field::FT_BOOL:    return "bool";
    case Field::FT_STRING:  return "string";
    case Field::FT_USER:    return f.userType_->getNameC();
	case Field::FT_ENUM:    return f.userType_->getEnum()->isEnum16()?"enum16":"enum";
    case Field::FT_BINARY:  return "binary";
    }
    return "";
}

static void generateFieldDefaults(CodeFile& f, FieldContainer* fc)
{
    for (size_t i = 0; i < fc->fields_.size(); i++)
    {
        Field& field = fc->fields_[i];
        const char* name = field.getNameC();
        const char* type = getFieldTypeName(field);
        if (field.isArray())
            f.output("%s = BintalkTypes.array(BintalkTypes.%s),", name, type);
        else
            f.output("%s = BintalkTypes.%s(),", name, type);
    }
}
static void generateFieldInitCode(CodeFile& f, FieldContainer* fc)
{
    for (size_t i = 0; i < fc->fields_.size(); i++)
    {
        Field& field = fc->fields_[i];
        const char* name = field.getNameC();
        const char* type = getFieldTypeName(field);
        f.output("if v.%s then", name);
        f.indent();
        if (field.isArray())
            f.output("s.%s = BintalkTypes.array(BintalkTypes.%s, v.%s)", name, type, name);
        else
            f.output("s.%s = BintalkTypes.%s(v.%s)", name, type, name);
        f.recover();
        f.output("end");
    }
}
static void generateFieldContainerSCode(CodeFile& f, FieldContainer* fc)
{
    for (size_t i = 0; i < fc->fields_.size(); i++)
    {
        Field& field = fc->fields_[i];
        if (field.isArray())
            f.output("BintalkWriter.array(BintalkWriter.%s, v.%s, b)", getFieldTypeName(field), field.getNameC());
        else
            f.output("BintalkWriter.%s(v.%s, b)", getFieldTypeName(field), field.getNameC());
    }
}
static void generateFieldContainerDSCode(CodeFile& f, FieldContainer* fc)
{
    for (size_t i = 0; i < fc->fields_.size(); i++)
    {
        Field& field = fc->fields_[i];
        if (field.isArray())
            f.output("v.%s, p = BintalkReader.array(b, p, BintalkReader.%s, 0X%X, 0X%X)", field.getNameC(), getFieldTypeName(field), field.maxArray_, field.maxValue_);
        else
            f.output("v.%s, p = BintalkReader.%s(b, p, 0X%X)", field.getNameC(), getFieldTypeName(field), field.maxValue_);
    }
}

static void generateStruct(CodeFile& f, Struct* s)
{
    const char* name = s->getNameC();

    f.output("local %s = {", name);
    f.indent();
    generateFieldDefaults(f, s);
    f.recover();
    f.output("}");

    f.output("BintalkTypes.%s = function(v)", name);
    f.indent();
    f.output("local _values = {");
    f.indent();
    generateFieldDefaults(f, s);
    f.recover();
    f.output("}");
    f.output("local s = {_defaults = %s, _values = _values}", name);
    f.output("setmetatable(s, BintalkTypes._struct_mt)");
    f.output("if not v then return s end");
    generateFieldInitCode(f, s);
    f.output("return s");
    f.recover();
    f.output("end");

    f.output("BintalkWriter.%s = function(v, b)", s->getNameC());
    f.indent();
    generateFieldContainerSCode(f, s);
    f.recover();
    f.output("end");

    f.output("BintalkReader.%s = function(b, p)", s->getNameC());
    f.indent();
    f.output("local v = BintalkTypes.%s()", s->getNameC());
    generateFieldContainerDSCode(f, s);
    f.output("return v, p");
    f.recover();
    f.output("end");
}

void LUAGenerator::generate()
{
    std::string fn = gOptions.output_ + gOptions.inputFS_ + ".lua";
    CodeFile f(fn);
    f.output("require(\"bintalk.types\")");
    f.output("require(\"bintalk.reader\")");
    f.output("require(\"bintalk.writer\")");
    for (std::set<std::string>::iterator iter = gContext.imported_.begin();
        iter != gContext.imported_.end(); ++iter)
    {
        std::string incFilename = *iter;
        incFilename = incFilename.substr(0, incFilename.find('.'));
        f.output("require(\"btk_gen.%s\")", incFilename.c_str());
    }
    for (size_t i = 0; i < gContext.definitions_.size(); i++)
    {
        Definition* definition = gContext.definitions_[i];
        if (definition->file_ != gOptions.inputFN_)
            continue;
        if (definition->getEnum())
            generateEnum(f, definition->getEnum());
        else if (definition->getStruct())
            generateStruct(f, definition->getStruct());
    }
}