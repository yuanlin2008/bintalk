#ifndef __Context_h__
#define __Context_h__

#include <string>
#include <vector>
#include <set>
#include <map>

#include "Definition.h"

class Context
{
public:
	Context();
	~Context();

	/** 编译源文件. */
	bool build();

	/** 根据名称查找一个定义. 
	*/
	Definition* findDefinition(const std::string& name);

	/** 输出当前文件行编译错误. */
	void error(const char* e, ...);

	/** 所有已经被包含过的文件名称，防止重复包含. */
	std::set<std::string>		imported_;
	/** 根文件直接包含的文件名称. */
	std::vector<std::string>	rootImported_;
	/** 当前被解析的文件名称. */
	std::string					curFilename_;
	/** 当前被解析文件的行号. */
	int							lineno_;
	/** 这个源文件包含的所有arpc定义. */
	std::vector<Definition*>	definitions_;
	// 当前编译状态.
	Enum		curEnum_;
	Struct		curStruct_;
	Field		curField_;
};

extern Context gContext;

#endif//__Context_h__