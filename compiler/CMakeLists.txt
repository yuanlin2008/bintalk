########################################################################
# bintalk compiler
########################################################################

# find bison & flex.
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
BISON_TARGET(BINTALK_PARSER bintalk.y ${CMAKE_CURRENT_BINARY_DIR}/bintalk.tab.cpp)
FLEX_TARGET(BINTALK_SCANNER bintalk.l ${CMAKE_CURRENT_BINARY_DIR}/lex.yy.cpp)
ADD_FLEX_BISON_DEPENDENCY(BINTALK_SCANNER BINTALK_PARSER)

# to find generated files.
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
# make generated files include source files.
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# sources.
set(PARSER_FILES
	${BISON_BINTALK_PARSER_OUTPUTS}
	${FLEX_BINTALK_SCANNER_OUTPUTS}
	bintalk.y
	bintalk.l
	)
set(SPEC_FILES
	Definition.h
	)
set(GEN_FILES
	CppGenerator.cpp
	PYGenerator.cpp
	ERLGenerator.cpp
	CSGenerator.cpp
	LUAGenerator.cpp
	UE4Generator.cpp
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
