using System;
using System.Text;
using System.Collections.Generic;

namespace bintalk
{
    /** This class can write basic types by using a bintalk.IWriter object. */
    public static partial class ProtocolWriter
    {
        public static void writeMid(bintalk.IWriter w, int v)
        {
            write(w, (ushort)v);
        }
        // int 64
    	public static void write(bintalk.IWriter w, long v)   
        { 
            w.write(BitConverter.GetBytes(v)); 
        }
        // uint64
    	public static void write(bintalk.IWriter w, ulong v)  
        { 
            w.write(BitConverter.GetBytes(v)); 
        }
        // double
    	public static void write(bintalk.IWriter w, double v) 
        { 
            w.write(BitConverter.GetBytes(v)); 
        }
        // float
    	public static void write(bintalk.IWriter w, float v)  
        { 
            w.write(BitConverter.GetBytes(v)); 
        }
        // int32
    	public static void write(bintalk.IWriter w, int v)    
        { 
            w.write(BitConverter.GetBytes(v));
        }
        // uint32
    	public static void write(bintalk.IWriter w, uint v)   
        {
            w.write(BitConverter.GetBytes(v));
        }
        // int16
    	public static void write(bintalk.IWriter w, short v)  
        { 
            w.write(BitConverter.GetBytes(v));
        }
        // uint16
    	public static void write(bintalk.IWriter w, ushort v) 
        {
            w.write(BitConverter.GetBytes(v));
        }
        // int8
    	public static void write(bintalk.IWriter w, sbyte v)  
        {
            w.write(new byte[1]{(byte)v});
        }
        // uint8
    	public static void write(bintalk.IWriter w, byte v)   
        {
            w.write(new byte[1]{v});
        }
        // bool
    	public static void write(bintalk.IWriter w, bool v)   
        {
            write(w, (byte)(v?1:0));
        }
        // string
    	public static void write(bintalk.IWriter w, string v)
    	{
            if (v == null || v.Length == 0)
                writeDynSize(w, 0);
            else
            {
                byte[] str = Encoding.UTF8.GetBytes(v);
                uint len = (uint)str.Length;
                writeDynSize(w, len);
                if (len > 0)
                    w.write(str);
            }
    	}
        // binary.
        public static void write(bintalk.IWriter w, byte[] v) 
        { 
            if (v == null || v.Length == 0)
                writeDynSize(w, 0);
            else
            {
                uint s = (uint)v.Length;
                writeDynSize(w, s);
                w.write(v); 
            }
        }
        // dynamic size.
        public static void writeDynSize(bintalk.IWriter w, uint s)
        {
            byte[] b = BitConverter.GetBytes(s);
            int n = 0;
            if (s <= 0X3F)
                n = 0;
            else if (s <= 0X3FFF)
                n = 1;
    		else if(s <= 0X3FFFFF)
    			n = 2;
    		else if(s <= 0X3FFFFFFF)
    			n = 3;
            b[n] |= (byte)(n << 6);
    		for(int i = n; i >= 0; i--)
    			write(w, b[i]);
        }
    }
}
