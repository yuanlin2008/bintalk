BINTALK_CMD=bintalk
rm -rf gen
mkdir gen
cd gen

# cpp
echo "cpp"
mkdir cpp
cd cpp
BINTALK_CMD -g cpp Import.btk
BINTALK_CMD -g cpp Example.btk
cd ..

# c sharp
echo "cs"
mkdir cs
cd cs
BINTALK_CMD -g cs Import.btk
BINTALK_CMD -g cs Example.btk
cd ..

# erl
echo "erl"
mkdir erl
cd erl
BINTALK_CMD -g erl Import.btk
BINTALK_CMD -g erl Example.btk
cd ..

#py 
echo "py"
mkdir py
cd py
BINTALK_CMD -g py Import.btk
BINTALK_CMD -g py Example.btk
cd ..
