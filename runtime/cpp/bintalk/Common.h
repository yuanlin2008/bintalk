#ifndef __Common_h__
#define __Common_h__

#include <vector>
#include <string>

namespace bintalk
{

typedef unsigned short		MID;
typedef signed long long	INT64;
typedef unsigned long long	UINT64;
typedef double				DOUBLE;
typedef float				FLOAT;
typedef int					INT32;
typedef unsigned int		UINT32;
typedef short				INT16;
typedef unsigned short		UINT16;
typedef char				INT8;
typedef unsigned char		UINT8;
typedef bool				BOOL;
typedef std::string			STRING;
typedef std::vector<UINT8>	BINARY;

}

#endif// __Common_h__