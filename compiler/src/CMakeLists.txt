########################################################################
# bintalk compiler
########################################################################
cmake_minimum_required(VERSION 2.6)
project(bintalk)

# Add binary tree to the search path for include files.
include_directories(${PROJECT_SOURCE_DIR})
include_directories(${PROJECT_BINARY_DIR})

# The version number.
set(BINTALK_VERSION_MAJOR 1)
set(BINTALK_VERSION_MINOR 0)

# configure a header file to pass some of the CMake settings.
configure_file("Config.h.in" "Config.h")

# find flex
find_program(FLEX flex)

# find bison
find_program(BISON bison)

# bintalk.l
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/lex.yy.cpp
	COMMAND flex -o${CMAKE_CURRENT_BINARY_DIR}/lex.yy.cpp ${CMAKE_CURRENT_SOURCE_DIR}/bintalk.l 
	MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/bintalk.l 
	)

# bintalk.y
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/bintalk.tab.cpp ${CMAKE_CURRENT_BINARY_DIR}/bintalk.tab.hpp
	COMMAND bison -d -o ${CMAKE_CURRENT_BINARY_DIR}/bintalk.tab.cpp ${CMAKE_CURRENT_SOURCE_DIR}/bintalk.y 
	MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/bintalk.y 
	)

# sources.
set(PARSER_FILES
	${CMAKE_CURRENT_BINARY_DIR}/lex.yy.cpp
	${CMAKE_CURRENT_BINARY_DIR}/bintalk.tab.hpp
	${CMAKE_CURRENT_BINARY_DIR}/bintalk.tab.cpp
	bintalk.y
	bintalk.l
	)
set(SPEC_FILES
	Definition.h
	Enum.h
	Field.h
	Service.h
	Struct.h
	)
set(GEN_FILES
	CppGenerator.cpp
	PYGenerator.cpp
	ERLGenerator.cpp
	CSGenerator.cpp
	)
set(MISC_FILES
	bintalk.cpp
	CodeFile.h
	CodeFile.cpp
	CodeGenerator.h
	CodeGenerator.cpp
	Options.h
	Options.cpp
	Context.h
	Context.cpp
	)

source_group( parser FILES ${PARSER_FILES})
source_group( spec FILES ${SPEC_FILES})
source_group( gen FILES ${GEN_FILES})
source_group( misc FILES ${MISC_FILES})

add_executable(bintalk ${PARSER_FILES} ${SPEC_FILES} ${GEN_FILES} ${MISC_FILES})

# install bintalk
install(TARGETS bintalk DESTINATION bin)
