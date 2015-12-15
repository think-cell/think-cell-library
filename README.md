range
=====

Compile instructions:

	cmake ./Library/Utilities/Range/
	make

If the boost library is not in the default location, you can specify the path with

	cmake -DBOOST_ROOT=/path/to/boost_1_59_0 ./Library/Utilities/Range/

The Range specific code is under https://github.com/think-cell/range/tree/master/Library/Utilities/Range 

The tests and examples are in Library/Utilities/Range/*.t.cpp

Tested with:
* clang 3.5

