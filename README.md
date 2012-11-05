bintalk
=======
Introduction
------------
**bintalk** is a lightweight tool which is used to encode/decode structured data to/from binary. Bintalk use a source file which describes the structured data to generate encoding/decoding code of a specified language. Diverse languages can communicate each other freely based on the same set of data descriptions by using the generated code.
Unlike other systems(JSON, XML, ProtocolBuffers...), bintalk is a very small and lightweight tool. The encoded binary data contains no meta data and is small enough to be transmited through network. For each supported language, there is a very efficient runtime library which simply encapsulate the binary operation of the language.
**bintalk** is designed to support as many languages as it could. It's easy to integrate a new language into bintalk, as long as the language support binary operations.

Topology
--------
###compiler
Bintalk compiler is used to generate code for a specified language from a btk source file.

###runtime
Runtime library for each supported language.

Building
--------
Tools needed:
* cmake
* gnu flex & bison

###Unix
* make a "build" dir in your favorite position.
* cd build
* cmake BINTALK_SRC/compiler/src
* make

###Windows
You can use cmake-gui tool to generate visual studio solution files.
* run cmake-gui tool
* specify source code directory
* specify build directory as "BINTALK_SRC/compiler/src"
* Configure
* Generate

Running
-------
bintalk [options] btkfile
* -o dir directory for your generated code.
* -i dirs directories which are use to search import files.
* -g gen code generator name.

Supported Language
------------------
* c++
* c#
* python
* erlang

