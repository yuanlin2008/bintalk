using System;
using System.Text;
using System.Collections.Generic;

namespace bintalk
{
    /** This class can read basic types by using a bintalk.IReader object. */
    public static partial class ProtocolReader
    {
        public static bool readMid(bintalk.IReader r, ref ushort v)
        {
            return read(r, ref v, 0);
        }
		// int64
    	public static bool read(bintalk.IReader r, ref long v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(8, out data, out startId)) return false;
            v = BitConverter.ToInt64(data, startId);
            return true;
    	}
		// uint64
    	public static bool read(bintalk.IReader r, ref ulong v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(8, out data, out startId)) return false;
            v = BitConverter.ToUInt64(data, startId);
            return true;
    	}
		// double
    	public static bool read(bintalk.IReader r, ref double v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(8, out data, out startId)) return false;
            v = BitConverter.ToDouble(data, startId);
            return true;
    	}
		// float
    	public static bool read(bintalk.IReader r, ref float v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(4, out data, out startId)) return false;
            v = BitConverter.ToSingle(data, startId);
            return true;
    	}
		// int32
    	public static bool read(bintalk.IReader r, ref int v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(4, out data, out startId)) return false;
            v = BitConverter.ToInt32(data, startId);
            return true;
    	}
		// uint32
    	public static bool read(bintalk.IReader r, ref uint v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(4, out data, out startId)) return false;
            v = BitConverter.ToUInt32(data, startId);
            return true;
    	}
		// int16
    	public static bool read(bintalk.IReader r, ref short v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(2, out data, out startId)) return false;
            v = BitConverter.ToInt16(data, startId);
            return true;
    	}
		// uint16
    	public static bool read(bintalk.IReader r, ref ushort v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(2, out data, out startId)) return false;
            v = BitConverter.ToUInt16(data, startId);
            return true;
    	}
		// int8
    	public static bool read(bintalk.IReader r, ref sbyte v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(1, out data, out startId))
                return false;
            v = (sbyte)data[startId];
            return true;
    	}
		// uint8
    	public static bool read(bintalk.IReader r, ref byte v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(1, out data, out startId))
                return false;
            v = data[startId];
            return true;
    	}
		// bool
    	public static bool read(bintalk.IReader r, ref bool v, uint maxValue)
    	{
            byte[] data; int startId;
            if (!r.read(1, out data, out startId))
                return false;
            v = (data[startId] == 0)?false:true;
            return true;
    	}
        // string.
    	public static bool read(bintalk.IReader r, ref string v, uint maxValue)
    	{
            uint s;
            if (!readDynSize(r, out s) || s > maxValue)
                return false;
            byte[] data; int startId;
            if(!r.read(s, out data, out startId))
                return false;
            v = Encoding.UTF8.GetString(data, startId, (int)s);
            return true;
    	}
        // binary.
    	public static bool read(bintalk.IReader r, ref byte[] v, uint maxValue)
    	{
            uint s;
            if (!readDynSize(r, out s) || s > maxValue)
                return false;
            v = new byte[s];
            byte[] data; int startId;
            if(!r.read(s, out data, out startId))
                return false;
            Array.Copy(data, startId, v, 0, s);
            return true;
    	}
        // dynamic size.
        public static bool readDynSize(bintalk.IReader r, out uint s)
        {
            s = 0;
            byte b = 0;
            if (!read(r, ref b, 0))
                return false;
            uint n = (uint)((b & 0XC0) >> 6);
            s = (uint)(b & 0X3F);
            for (int i = 0; i < n; i++)
            {
                if (!read(r, ref b, 0))
                    return false;
                s = (s << 8) | b;
            }
            return true;
        }
    }
}
