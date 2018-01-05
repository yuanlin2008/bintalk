#include "Options.h"
#include "Context.h"
#include "CodeFile.h"

#include "CodeGenerator.h"
DECLARE_CG(LUAGenerator, lua);

static void generateEnum(CodeFile& f, Enum* e)
{
    f.output("%s =", e->getNameC());
    f.output("{");
    f.indent();
    for (size_t i = 0; i < e->items_.size(); i++)
        f.output("%s = %d,", e->items_[i].c_str(), i);
    f.recover();
    f.output("}");
}
static const char* getFieldDefault(Field& f)
{
    static std::string name;
    if (f.isArray())
        name = "[]";
    else if (f.type_ == Field::FT_BOOL)
        name = "False";
    else if (f.type_ == Field::FT_STRING)
        name = "\"\"";
    else if (f.type_ == Field::FT_BINARY)
        name = "\"\"";
    else if (f.type_ == Field::FT_USER)
        name = f.userType_->name_ + "()";
    else
        name = "0";
    return name.c_str();
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
    case Field::FT_ENUM:    return "enum";
    case Field::FT_BINARY:  return "binary";
    }
    return "";
}

static void generateFieldContainerSCode(CodeFile& f, FieldContainer* fc)
{
    for (size_t i = 0; i < fc->fields_.size(); i++)
    {
        Field& field = fc->fields_[i];
        if (field.isArray())
            f.output("BTK_Writer.type_array(BTK_Writer.type_%s, v.%s, b)", getFieldTypeName(field), field.getNameC());
        else
            f.output("BTK_Writer.type_%s(v.%s, b)", getFieldTypeName(field), field.getNameC());
    }
}
static void generateFieldContainerDSCode(CodeFile& f, FieldContainer* fc)
{
    for (size_t i = 0; i < fc->fields_.size(); i++)
    {
        Field& field = fc->fields_[i];
        if (field.isArray())
            f.output("v.%s, p = BTK_Reader.type_array(b, p, BTK_Reader.type_%s, 0X%X, 0X%X)", field.getNameC(), getFieldTypeName(field), field.maxArray_, field.maxValue_);
        else
            f.output("v.%s, p = BTK_Reader.type_%s(b, p, 0X%X)", field.getNameC(), getFieldTypeName(field), field.maxValue_);
    }
}

static void generateStruct(CodeFile& f, Struct* s)
{
    f.output("BTK_Writer.type_%s = function(v, b)", s->getNameC());
    f.indent();
    generateFieldContainerSCode(f, s);
    f.recover();
    f.output("end");

    f.output("BTK_Reader.type_%s = function(b, p)", s->getNameC());
    f.indent();
    f.output("local v = {}");
    generateFieldContainerDSCode(f, s);
    f.output("return v, p");
    f.recover();
    f.output("end");
}

void LUAGenerator::generate()
{
    std::string fn = gOptions.output_ + gOptions.inputFS_ + ".lua";
    CodeFile f(fn);
    f.output("require(\"bintalk.protocol_reader\")");
    f.output("require(\"bintalk.protocol_writer\")");
    for (std::set<std::string>::iterator iter = gContext.imported_.begin();
        iter != gContext.imported_.end(); ++iter)
    {
        std::string incFilename = *iter;
        incFilename = incFilename.substr(0, incFilename.find('.'));
        f.output("require(\"%s\")", incFilename.c_str());
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