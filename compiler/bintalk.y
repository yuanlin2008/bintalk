/* Declarations */

%{

#include <string>
#define YYSTYPE std::string

void yyerror (const char *);
int yylex (void);

#include "Context.h"

%}

%error-verbose


/*
 * Token types: These are returned by the lexer
 */
%token				TOKEN_IDENTIFIER
%token				TOKEN_ENUM
%token				TOKEN_STRUCT
%token				TOKEN_INT64
%token				TOKEN_UINT64
%token				TOKEN_DOUBLE
%token				TOKEN_FLOAT
%token				TOKEN_INT32
%token				TOKEN_UINT32
%token				TOKEN_INT16
%token				TOKEN_UINT16
%token				TOKEN_INT8
%token				TOKEN_UINT8
%token				TOKEN_BOOL
%token				TOKEN_STRING
%token				TOKEN_BINARY
%token				TOKEN_UINTEGER_LITERAL


%%

/*
 * Production starts here.
 */
start: definitions;

/* definitions */
definitions: 
	definitions definition
	| 
	/* empty */
	;

/*definition*/
definition:
	enumeration
	|
	structure
    ;

/*enumeration*/
enumeration:
	TOKEN_ENUM
	TOKEN_IDENTIFIER	
	{
		// Check enum name.
		if(gContext.findDefinition($2))
		{
			gContext.error("duplicated definition \"%s\".\n", $2.c_str()); 
			YYERROR; 
		};
		
		// Init current enum.
		gContext.curEnum_ = Enum();
		gContext.curEnum_.name_ = $2;
		gContext.curEnum_.file_ = gContext.curFilename_;
	}
	'{' enum_items '}'
	{
		if(gContext.curEnum_.items_.size() > 65536)
		{
			gContext.error("enum \"%s\" has too many items.", gContext.curEnum_.getNameC());
			YYERROR;
		}
		// Add this definition.
		Enum* e = new Enum(gContext.curEnum_);
		gContext.definitions_.push_back(e);
	}
	;

/*enum_items*/
enum_items:
	enum_items enum_item
	|
	/*empty*/
	;

/*enum_item*/	
enum_item:
	TOKEN_IDENTIFIER ','
	{
		// Check enum item name.
		if(gContext.findDefinition($1))
		{
			gContext.error("invalid enum item \"%s\"", $1.c_str());
			YYERROR;
		}
		if(gContext.curEnum_.findItem($1))
		{
			gContext.error("duplicated enum item \"%s\"", $1.c_str());
			YYERROR;
		}
		gContext.curEnum_.items_.push_back($1);
	}
	;

/*structure*/
structure:
	TOKEN_STRUCT
	TOKEN_IDENTIFIER
	{
		// Check struct name.
		if(gContext.findDefinition($2))
		{ 
			gContext.error("duplicated definition \"%s\".\n", $2.c_str()); 
			YYERROR; 
		};
		
		// Init current struct.
		gContext.curStruct_ = Struct();
		gContext.curStruct_.name_ = $2;
		gContext.curStruct_.file_ = gContext.curFilename_;
	}
	'{' struct_fields '}'
	{
		// Add this struct definition.
		Struct* s = new Struct(gContext.curStruct_);
		gContext.definitions_.push_back(s);
	}
	;

/*struct_fields*/
struct_fields:
	struct_fields struct_field
	|
	/* empty */
	;
	
/*struct_field*/
struct_field:
	field_type TOKEN_IDENTIFIER ';'
	{
		// Check field name.
		if(gContext.findDefinition($2))
		{
			gContext.error("invalid field name\"%s\"", $2.c_str());
			YYERROR;
		}
		if(gContext.curStruct_.findField($2))
		{
			gContext.error("duplicated field name\"%s\"", $2.c_str());
			YYERROR;
		}

		gContext.curField_.name_ = $2;
		gContext.curStruct_.fields_.push_back(gContext.curField_);
		gContext.curField_ = Field();
	}
	;
	
	
/*field_type*/
field_type:
	field_data_type opt_array
	;

/*opt_array*/
opt_array:
	'[' ']'
	{
		gContext.curField_.maxArray_ = 0XFFFFFFFF;
	}
	|
	'[' TOKEN_UINTEGER_LITERAL ']'
	{
		gContext.curField_.maxArray_ = ::atoi($2.c_str());
	}
	|
	/*empty*/
	;

/*field_data_type*/
field_data_type:
	TOKEN_INT64		{ gContext.curField_.type_ = (Field::FT_INT64); }
	|
	TOKEN_UINT64	{ gContext.curField_.type_ = (Field::FT_UINT64); }
	|
	TOKEN_DOUBLE	{ gContext.curField_.type_ = (Field::FT_DOUBLE); }
	|
	TOKEN_FLOAT		{ gContext.curField_.type_ = (Field::FT_FLOAT); }
	|
	TOKEN_INT32		{ gContext.curField_.type_ = (Field::FT_INT32); }
	|
	TOKEN_UINT32	{ gContext.curField_.type_ = (Field::FT_UINT32); }
	|
	TOKEN_INT16		{ gContext.curField_.type_ = (Field::FT_INT16); }
	|
	TOKEN_UINT16	{ gContext.curField_.type_ = (Field::FT_UINT16); }
	|
	TOKEN_INT8		{ gContext.curField_.type_ = (Field::FT_INT8); }
	|
	TOKEN_UINT8		{ gContext.curField_.type_ = (Field::FT_UINT8); }
	|
	TOKEN_BOOL		{ gContext.curField_.type_ = (Field::FT_BOOL); }
	|
	TOKEN_STRING	opt_max_value { gContext.curField_.type_ = (Field::FT_STRING); }
	|
	TOKEN_BINARY	opt_max_value { gContext.curField_.type_ = (Field::FT_BINARY); }
	|
	TOKEN_IDENTIFIER	
	{
		Definition* d = gContext.findDefinition($1);
		if(d->getEnum())
		{
			gContext.curField_.type_ = (Field::FT_ENUM); 
			gContext.curField_.maxValue_ = (d->getEnum()->items_.size() - 1);
		}
		else
			gContext.curField_.type_ = (Field::FT_USER); 
		gContext.curField_.userType_ = d;
	}
	;

/* opt_max_value. */
opt_max_value:
	'(' TOKEN_UINTEGER_LITERAL ')'
	{
		gContext.curField_.maxValue_ = (::atoi($2.c_str()));
	}
	|
	/*empty*/
	{
		gContext.curField_.maxValue_ = (0XFFFFFFFF);
	}
	;
		
%%


int yywrap (void)
{
  return 1;
}

void yyerror (const char *msg)
{
	gContext.error(msg);
}
