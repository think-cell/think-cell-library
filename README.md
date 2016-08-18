range
=====

Compile instructions:

	cmake ./range/
	make

If the boost library is not in the default location, you can specify the path with

	cmake -DBOOST_ROOT=/path/to/boost_1_59_0 ./range/

You need to apply boost_patches/has_range_iterator.patch to your boost library.
This is equivalent to https://github.com/boostorg/range/pull/40 (accepted, but not merged into release yet)

The Range specific code is under https://github.com/think-cell/range/tree/master/range

The tests and examples are in range/*.t.cpp

Tested with:
* clang 3.8 (due to a libc++ bug you need to also include the libc++abi-dev headers)
* msvc 2015

Usage on Windows:

	#define NOMINMAX
	#include <windows.h>
	#define RANGE_PROPOSAL_BUILD_STANDALONE
	#include "range/range.h"


