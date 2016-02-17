range
=====

Compile instructions:

	cmake ./range/
	make

If the boost library is not in the default location, you can specify the path with

	cmake -DBOOST_ROOT=/path/to/boost_1_59_0 ./range/

The Range specific code is under https://github.com/think-cell/range/tree/master/range 

The tests and examples are in range/*.t.cpp

Tested with:
* clang 3.5

