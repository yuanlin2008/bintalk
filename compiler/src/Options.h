#ifndef __Options_h__
#define __Options_h__

#include <string>
#include <vector>

class Options
{
public:
	/** 源文件完整路径名称. */
	std::string		input_;
	std::string		inputFN_;
	std::string		inputFS_;
	/** 生成代码文件的目录. */
	std::string		output_;
	/** 用于寻找import文件的目录. */
	std::vector<std::string>	importPaths_;
	/** 代码生成器名称 .*/
	std::string		generator_;

	bool parse(int argc, char* argv[]);
};

extern Options gOptions;

#endif// __Options_h__