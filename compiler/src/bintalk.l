/* IMPORT START CONDITION */
%x IMPORT

%{

#include <stdio.h>

/* bison value type. */
#include <string>
#define YYSTYPE std::string
#include "bintalk.tab.hpp"

#include "Options.h"
#include "Context.h"

/* An imported file. */
struct ImportedFile
{
	std::string		fn_;
	int				ln_;
	YY_BUFFER_STATE	bs_;
};

/* A stack used to save imported file states. */
std::vector<ImportedFile>	importStack;

%}

/* SO we don't choke on files that use \r\n */
NL [\r?\n]

%array
%option never-interactive
%option noyywrap


%%

enum            return TOKEN_ENUM;
struct			return TOKEN_STRUCT;
service			return TOKEN_SERVICE;
int64			return TOKEN_INT64;
uint64			return TOKEN_UINT64;
double			return TOKEN_DOUBLE;
float			return TOKEN_FLOAT;
int32			return TOKEN_INT32;
uint32			return TOKEN_UINT32;
int16			return TOKEN_INT16;
uint16			return TOKEN_UINT16;
int8			return TOKEN_INT8;
uint8			return TOKEN_UINT8;
bool			return TOKEN_BOOL;
string			return TOKEN_STRING;
binary			return TOKEN_BINARY;

#import			{	
					/* Begin 'IMPORT' start condition if we met "import" keyword */
					BEGIN(IMPORT);
				}
<IMPORT>[ \t]*		/* eat the white space */
<IMPORT>[^ \t\n]+	{ 
					
					/* got the import file name */
					std::string ifn;
					if(yytext[yyleng-1] == '\r')
						ifn = std::string(yytext, yyleng-1);
					else
						ifn = std::string(yytext, yyleng);
					if(gContext.imported_.find(ifn) != gContext.imported_.end())
					{
						// File is imported by other file, skip it.
						BEGIN(INITIAL);
					}
					else
					{
						// save this filename to imported files.
						gContext.imported_.insert(ifn);

						// Save current file state.
						ImportedFile n;
						n.fn_ = gContext.curFilename_;
						n.ln_ = gContext.lineno_ + 1;	
						n.bs_ = YY_CURRENT_BUFFER;
						importStack.push_back(n);
						// Set new file state.!!!Remove the last \n
						gContext.curFilename_	= ifn;
						gContext.lineno_	= 1;
						if(importStack.size() == 1)
						{
							/* root import */
							gContext.rootImported_.push_back(ifn);
						}
						
						// Open new file to parse.
						yyin = fopen(ifn.c_str(), "r");
						if(!yyin)
						{
							// Iterate all import path to find file.
							for(size_t i = 0; i < gOptions.importPaths_.size(); i++)
							{
								std::string tempfilename = gOptions.importPaths_[i];
								tempfilename += ifn;
								yyin = fopen(tempfilename.c_str(), "r");
								if(yyin)
									break;
							}
							if(!yyin)
							{
								// Can not found imported file.
								fprintf(stderr, "failed to import file \"%s\".\n", ifn.c_str());
								exit(1);
							}
						}
						yy_switch_to_buffer(yy_create_buffer(yyin, YY_BUF_SIZE));
						BEGIN(INITIAL);
					}
				}
<<EOF>>			{
					/* End of file */
					if(importStack.size() == 0)
					{
						// Root file, teminated.
						yyterminate();
					}
					else
					{
						// Recover file state.
						ImportedFile n = importStack.back();
						importStack.pop_back();
						gContext.curFilename_	= n.fn_;
						gContext.lineno_	= n.ln_;
						yy_delete_buffer(YY_CURRENT_BUFFER);
						yy_switch_to_buffer(n.bs_); 
					}
				}

[a-ij-rs-zA-IJ-RS-Z_][a-ij-rs-zA-IJ-RS-Z0-9_]* {
					/* Identifier */
					yylval = std::string(yytext, yyleng);
					return TOKEN_IDENTIFIER;
				}
				
[1-9][0-9]*		{
					/* unsigned int */
					yylval = std::string(yytext, yyleng);
					return TOKEN_UINTEGER_LITERAL;
				}
				
\/\/.*{NL}      {
					/* C++ Comment */
					gContext.lineno_++;
                }

"/*"			{
					/* C Comment. */
					for(;;) {
						char const c = yyinput();
						if (c == '*') {
							char const next = yyinput();
							if (next == '/')
								break;
							else
								unput(next);
						}
						else if (c == '\n') {
							gContext.lineno_++;
						}
					}
				}

[ \t]*          { /*White space.*/ }

{NL}            { /*New Line.*/ gContext.lineno_++;}

.               return yytext[0];

%%

/* subroutines */
