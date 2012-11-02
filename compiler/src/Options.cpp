#include "Config.h"
#include "Options.h"
#include "CodeGenerator.h"
#include <iostream>
#include <sstream>
#define BOOST_FILESYSTEM_VERSION 2
#include <boost/filesystem/path.hpp>
#include <boost/program_options.hpp>
namespace po = boost::program_options;

Options gOptions;

bool Options::parse(int argc, char* argv[])
{
	std::stringstream descStr;
	descStr<<"bintalk compiler. version "<<BINTALK_VERSION_MAJOR<<"."<<BINTALK_VERSION_MINOR;
	// parse program options.
	po::options_description desc(descStr.str());
	desc.add_options()
		("help,h", "produce help message.")
		("input,i", po::value<std::string>(), "input file name.")
		("output,o", po::value<std::string>(), "output directory.")
		("gen,g", po::value<std::string>(), CodeGenerator::desc())
		("import", po::value<std::string>(), "directories(separated by \";\") to find imported files.")
		;
	try 
	{
		po::variables_map vm;        
		po::store(po::parse_command_line(argc, argv, desc), vm);
		po::notify(vm);    

		if(vm.count("help")) 
		{
			std::cout << desc << "\n";
			return false;
		}

		// input source file name.
		input_ = vm["input"].as<std::string>();
		boost::filesystem::path filepath(input_);
		inputFN_ = filepath.filename();
		inputFS_ = filepath.stem();

		// output directory name.
		if(vm.count("output"))
			output_ = vm["output"].as<std::string>();
		else
			output_ = "./";
		// import directories.
		importPaths_.push_back(filepath.parent_path().string() + "/");
		if(vm.count("import"))
		{
			std::string imports = vm["import"].as<std::string>();
			while(1)
			{
				size_t i = imports.find(';');
				if(i == imports.npos)
				{
					importPaths_.push_back(imports);
					break;
				}
				else
				{
					std::string import = imports.substr(0, i);
					importPaths_.push_back(import);
					imports = imports.substr(i+1);
				}
			}
		}

		// code generator.
		generator_ = vm["gen"].as<std::string>();
	}
	catch(...) 
	{
		std::cout << desc << "\n";
		return false;
	}
	return true;
}