bintalk
=======
Introduction
------------
**bintalk**是一个用来对结构化数据进行二进制编码解码的轻量级工具。bintalk使用一个btk源文件对结构化数据进行描述，生成指定语言对数据的编解码代码。使用生成的编解码代码，可以使多种语言基于同一套数据描述进行自由的通信。
与很多现有的同类系统(JSON,XML,ProtocolBuffers...)不同，bintalk是一个非常小巧和轻量级的工具。编码后的二进制数据不包含任何的元数据，体积很小，便于网络传输。对于每种支持的语言都有一个运行时库，用来支持生成的代码。这些库的功能都是对语言层二进制数据处理的简单封装，保障了编解码的高效。
由于功能仅仅是编解码，所以bintalk对于支持语言的依赖性很低。只要语言本身支持二进制数据处理，就可以被加入到bintalk中。支持尽量多的语言之间的通信是bintalk的一个重要的设计目标。

Topology
--------
###compiler
bintalk编译器，用于编译btk文件，生成指定语言代码
###runtime
支持语言的运行时库

Building
--------
构件bintalk需要以下工具:
* cmake
* gnu flex & bison

###Unix
* 在任意地方创建一个build目录
* cd build
* cmake BINTALK_SRC/compiler/src
* make

###Windows
使用cmake-gui工具生成visual studio工程文件。
* 运行cmake-gui工具
* 指定source code目录
* 指定build目录为BINTALK_SRC/compiler/src
* Configure
* Generate

Running
-------
bintalk [options] btkfile
* -o dir 指定生成代码文件的输出目录，默认为当前目录。
* -i dirs 指定import的查找目录，多个目录以";"分割。默认为源文件目录。
* -g gen 代码生成器名称。

Supported Language
------------------
* c++
* c#
* python
* erlang
