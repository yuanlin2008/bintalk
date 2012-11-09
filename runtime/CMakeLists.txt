################################################################
# bintalk runtime library.
################################################################

# bintalk c++ runtion library.
include_directories(cpp)
set(CPP_DIR cpp/bintalk)
set(HDR_FILES
	${CPP_DIR}/Common.h
	${CPP_DIR}/BinaryReader.h
	${CPP_DIR}/BinaryWriter.h
	${CPP_DIR}/ProtocolReader.h
	${CPP_DIR}/ProtocolWriter.h
	${CPP_DIR}/FileReader.h
	${CPP_DIR}/FileWriter.h
	${CPP_DIR}/MemoryReader.h
	${CPP_DIR}/MemoryWriter.h
	)
set(CPP_FILES
	${CPP_DIR}/ProtocolReader.cpp
	${CPP_DIR}/ProtocolWriter.cpp
	${CPP_DIR}/FileReader.cpp
	${CPP_DIR}/FileWriter.cpp
	${CPP_DIR}/MemoryReader.cpp
	${CPP_DIR}/MemoryWriter.cpp
	)
add_library(bintalk_cpp STATIC ${HDR_FILES} ${CPP_FILES})
install(TARGETS bintalk_cpp ARCHIVE DESTINATION lib)
install(FILES ${HDR_FILES} DESTINATION include/bintalk)

# bintalk python package.
configure_file(py/setup.py.in ${CMAKE_CURRENT_SOURCE_DIR}/py/setup.py)
