#ifndef __CodeFile_h__
#define __CodeFile_h__

#include <cstdio>
#include <string>

class CodeFile
{
public:
	CodeFile(const std::string& filename);
	~CodeFile();
	void indent(const char* s = NULL);
	void recover(const char* s = NULL);
	/** Output a line of indented code. */
	void output(const char* s, ...);
	// List output.
	void listBegin(const char* sep, bool nl, const char* s, ...);
	void listItem(const char* s, ...);
	void listEnd(const char* s, ...);

private:
	FILE*			file_;
	unsigned int	ind_;
	std::string		listSep_;
	unsigned int	listItemNum_;
	bool			listNL_;
};

#endif// __CodeFile_h__
