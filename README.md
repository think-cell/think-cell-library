range
=====

Compile instructions:

	cmake ./Library/Utilities/Range/
	make

You might need to adapt the CMakeLists.txt in ./Library/Utilities/Range to find your boost include path.
We used boost 1.59 for all testing

The Range specific code is under https://github.com/think-cell/range/tree/master/Library/Utilities/Range 

The tests and examples are in Library/Utilities/Range/*.t.cpp

Tested with:
* clang 3.5

