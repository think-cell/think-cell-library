range
=====

Ranges proposal as described in ISO WG21 N3782 - http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3782.pdf

Compile instructions:

	cmake ./Library/Utilities/Range/
	make

You might need to adapt the CMakeLists.txt in ./Library/Utilities/Range to find your boost include path.
We used boost 1.55 for all testing

The Range specific code is under https://github.com/think-cell/range/tree/master/Library/Utilities/Range 

Tested with:
* clang 3.4
* VC11
* VC12

