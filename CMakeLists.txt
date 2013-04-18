################################################################
# bintalk
################################################################
cmake_minimum_required(VERSION 2.6)
project(bintalk)

set(CMAKE_DEBUG_POSTFIX "d")

# Test Endianess.
include(TestBigEndian)
test_big_endian(BINTALK_BIG_ENDIAN)

# The version number.
set(BINTALK_VERSION_MAJOR 1)
set(BINTALK_VERSION_MINOR 0)
# configure a header file to pass some of the CMake settings.
configure_file("Config.h.in" "Config.h")
include_directories(${PROJECT_BINARY_DIR})

# compiler.
add_subdirectory(compiler)
# runtime.
add_subdirectory(runtime)
