#pragma once

#include"CoreMinimal.h"

namespace Bintalk
{
	typedef TArray<uint8> Binary;

	/** Swap endianess of a value */
	template <typename T>
	T swapEndian(T u)
	{
		union
		{
			T u;
			unsigned char u8[sizeof(T)];
		} source, dest;
		source.u = u;
		for (size_t k = 0; k < sizeof(T); k++)
			dest.u8[k] = source.u8[sizeof(T) - k - 1];
		return dest.u;
	}

#if PLATFORM_LITTLE_ENDIAN
#define BINTALK_WRITER_SINGLE(T) void write##T(const T& v) { write(&v, sizeof(T)); }
#else//PLATFORM_LITTLE_ENDIAN
#define BINTALK_WRITER_SINGLE(T) void write##T(const T& v) { T vv = swapEndian(v); write(&vv, sizeof(T)); }
#endif//PLATFORM_LITTLE_ENDIAN

#define BINTALK_WRITER_ARRAY(T)\
	void write##T##A(const TArray<T>& v){\
		int32 s = v.Num();\
		writeDynSize(s);\
		for(int32 i = 0; i < s; i++) write##T(v[i]);}

#define BINTALK_WRITER_TYPE(T) \
	BINTALK_WRITER_SINGLE(T) \
	BINTALK_WRITER_ARRAY(T)


	/**
	 * Binary writer.
	 */
	class BINTALK_API BinaryWriter
	{
	public:	
		/** Interface to write. */
		virtual void write(const void* data, size_t len) = 0;

		BINTALK_WRITER_TYPE(int64);
		BINTALK_WRITER_TYPE(uint64);
		BINTALK_WRITER_TYPE(double);
		BINTALK_WRITER_TYPE(float);
		BINTALK_WRITER_TYPE(int32);
		BINTALK_WRITER_TYPE(uint32);
		BINTALK_WRITER_TYPE(int16);
		BINTALK_WRITER_TYPE(uint16);
		BINTALK_WRITER_TYPE(int8);
		BINTALK_WRITER_TYPE(uint8);

		void writebool(const bool& v)
		{
			uint8 vv = v ? 1 : 0;
			write(&vv, sizeof(uint8));
		}
		BINTALK_WRITER_ARRAY(bool);

		void writeFString(const FString& v)
		{
			int32 s = v.Len();
			writeDynSize(s);
			if (s > 0)
			{
				FTCHARToUTF8 utf8(*v);
				write(utf8.Get(), utf8.Length());
			}
		}
		BINTALK_WRITER_ARRAY(FString);

		void writeBinary(const Binary& v)
		{
			int32 s = v.Num();
			writeDynSize(s);
			if(s > 0)
				write(v.GetData(), s);
		}
		BINTALK_WRITER_ARRAY(Binary);

		// Enum
		template<class T>
		void writeENUM(T v)
		{
			uint8 e = (uint8)v;
			w->write(&e, sizeof(uint8));
		}
		template<class T>
		void writeENUMA(const TArray<T>& v)
		{
			int32 s = v.Num()
			writeDynSize(w, s);
			for (int32 i = 0; i < s; i++)
				writeENUM(v[i]);
		}
		// Enum16
		template<class T>
		void writeENUM16(T v)
		{
			uint16 e = (uint16)v;
			writeuint16(e);
		}
		template<class T>
		void writeENUM16A(const TArray<T>& v)
		{
			int32 s = v.Num();
			writeDynSize(s);
			for (int32 i = 0; i < s; i++)
				writeENUM16(v[i]);
		}
		// User
		template<class T>
		void writeUSER(const T& v)
		{
			v.serialize(w);
		}
		template<class T>
		void writeUSERA(const TArray<T>& v)
		{
			int32 s = v.Num();
			writeDynSize(s);
			for (int32 i = 0; i < s; i++)
				writeUSER(v[i]);
		}
		// Dynamic size.
		void writeDynSize(int32 ds)
		{
			uint32 s = ds > 0 ? (uint32)ds : 0;
			uint8* p = (uint8*)(&s);
			uint8 n = 0;
			if (s <= 0X3F)
				n = 0;
			else if (s <= 0X3FFF)
				n = 1;
			else if (s <= 0X3FFFFF)
				n = 2;
			else if (s <= 0X3FFFFFFF)
				n = 3;
			p[n] |= (n << 6);
			for (int i = (int)n; i >= 0; i--)
				write(p + i, sizeof(uint8));
		}
	};

#if PLATFORM_LITTLE_ENDIAN
#define BINTALK_READER_SINGLE(T) bool read##T(T& v, int32 maxArray, int32 maxValue) { return read(&v, sizeof(T)); }
#else//PLATFORM_LITTLE_ENDIAN
#define BINTALK_READER_SINGLE(T) void read##T(T& v, int32 maxArray, int32 maxValue) { if(!read(&v, sizeof(T))) return false; v = swapEndian(v); return true; }
#endif//PLATFORM_LITTLE_ENDIAN

#define BINTALK_READER_ARRAY(T)\
	bool read##T##A(TArray<T>& v, int32 maxArray, int32 maxValue){\
		int32 s;\
		if (!readDynSize(s) || s > maxArray) return false;\
		v.SetNum(s);\
		for(int32 i = 0; i < s; i++) if(!read##T(v[i], maxArray, maxValue)) return false;\
		return true; }

#define BINTALK_READER_TYPE(T) \
	BINTALK_READER_SINGLE(T)\
	BINTALK_READER_ARRAY(T)

	/**
	 * Binary reader.
	 */
	class BinaryReader
	{
	public:
		virtual bool read(void* data, size_t len) = 0;

		BINTALK_READER_TYPE(int64);
		BINTALK_READER_TYPE(uint64);
		BINTALK_READER_TYPE(double);
		BINTALK_READER_TYPE(float);
		BINTALK_READER_TYPE(int32);
		BINTALK_READER_TYPE(uint32);
		BINTALK_READER_TYPE(int16);
		BINTALK_READER_TYPE(uint16);
		BINTALK_READER_TYPE(int8);
		BINTALK_READER_TYPE(uint8);

		bool readbool(bool& v, int32 maxArray, int maxValue)
		{
			uint8 vv;
			if (!read(&vv, sizeof(uint8))) return false;
			v = vv ? true : false;
			return true;
		}
		BINTALK_READER_ARRAY(bool);

		bool readFString(FString& v, int32 maxArray, int32 maxValue)
		{
			int32 s;
			if (!readDynSize(s) || s > maxValue) return false;
			TArray<uint8> utf8;
			utf8.SetNum(s + 1);
			if (!read(utf8.GetData(), s)) return false;
			utf8[s - 1] = 0;
			v = UTF8_TO_TCHAR((const ANSICHAR*)utf8.GetData());
			return true;
		}
		BINTALK_READER_ARRAY(FString);

		bool readBinary(Binary& v, int32 maxArray, int32 maxValue)
		{
			int32 s;
			if (!readDynSize(s) || s > maxValue) return false;
			v.SetNum(s);
			if (s > 0)
				return read(v.GetData(), s);
			return true;
		}
		BINTALK_READER_ARRAY(Binary);

		// Enum
		template<class T>
		int readENUM(T& v, int32 maxArray, int32 maxValue)
		{
			uint8 e;
			if (!read(&e, sizeof(uint8)) || (int32)e > maxValue)
				return false;
			v = (T)e;
			return true;
		}
		template<class T>
		bool readENUMA(TArray<T>& v, int32 maxArray, int32 maxValue)
		{
			int32 s;
			if (!readDynSize(s) || s > maxArray) return false;
			v.SetNum(s);
			for (int32 i = 0; i < s; i++) if (!readENUM(v[i], 0, maxValue)) return false;
			return true;
		}
		// Enum16
		template<class T>
		bool readENUM16(T& v, int32 maxArray, int32 maxValue)
		{
			uint16 e;
			if (!readuint16(e, maxArray, maxValue))
				return false;
			if ((int32)e > maxValue)
				return false;
			v = (T)e;
			return true;
		}
		template<class T>
		bool readENUM16A(TArray<T>& v, int32 maxArray, int32 maxValue)
		{
			int32 s;
			if (!readDynSize(s) || s > maxArray) return false;
			v.SetNum(s);
			for (int32 i = 0; i < s; i++) if (!readENUM16(v[i], 0, maxValue)) return false;
			return true;
		}
		// User
		template<class T>
		bool readUSER(T& v, int32 maxArray, int32 maxValue)
		{
			v.deserialize(r);
			return true;
		}
		template<class T>
		bool readUSERA(TArray<T>& v, int32 maxArray, int32 maxValue)
		{
			int32 s;
			if (!readDynSize(s) || s > maxArray) return false;
			v.SetNum(s);
			for (int32 i = 0; i < s; i++) if (!readUSER(v[i], 0, maxValue)) return false;
			return true;
		}
		// Dynamic size.
		bool readDynSize(int32& s)
		{
			s = 0;
			uint8 b;
			if (!read(&b, sizeof(uint8)))
				return false;
			size_t n = (b & 0XC0) >> 6;
			s = (b & 0X3F);
			for (size_t i = 0; i < n; i++)
			{
				if (!read(&b, sizeof(UINT8)))
					return false;
				s = (s << 8) | b;
			}
			return true;
		}
	};

}