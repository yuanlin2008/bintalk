########################################################################
# bintalk compiler
########################################################################

# to find "Config.h"
include_directories(${PROJECT_BINARY_DIR})
# to find generated files.
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})


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
