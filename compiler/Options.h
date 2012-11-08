#ifndef __Options_h__
#define __Options_h__

#include <string>
#include <vector>

class Options
{
public:
	std::string		input_;
	std::string		inputFN_;
	std::string		inputFS_;
	std::string		output_;
	std::vector<std::string>	importPaths_;
	std::string		generator_;

	bool parse(int argc, char* argv[]);
	void showUsage();
private:
	bool parseOptions(int argc, char* argv[]);
	bool parseInput(const char* filename);
};

extern Options gOptions;

#endif// __Options_h__