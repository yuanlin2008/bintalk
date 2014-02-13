BINTALK_CMD=../../install/bin/bintalk
rm -rf gen
mkdir gen
cd gen
mkdir cpp
mkdir cs
mkdir erl
mkdir py
cd ..

# cpp
echo "cpp"
$BINTALK_CMD -g cpp -o gen/cpp/ Import.btk
$BINTALK_CMD -g cpp -o gen/cpp/ Example.btk

# c sharp
echo "cs"
$BINTALK_CMD -g cs -o gen/cs/ Import.btk
$BINTALK_CMD -g cs -o gen/cs/ Example.btk

# erl
echo "erl"
$BINTALK_CMD -g erl -o gen/erl/ Import.btk
$BINTALK_CMD -g erl -o gen/erl/ Example.btk

#py 
echo "py"
$BINTALK_CMD -g py -o gen/py/ Import.btk
$BINTALK_CMD -g py -o gen/py/ Example.btk
