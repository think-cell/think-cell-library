#pragma once

#include "globaldef.h"

#if PERFORMUNITTESTS

#include "_Assert.h"

// TODOMAC: Remove tchar.h here entirely. UNITTESTDEF(testname) assumes testname is ASCII anyway.
// It is used as an C++ identifier.
#ifdef TC_WIN

#include <tchar.h>
#pragma section("TCTEST$a", read)
#pragma section("TCTEST$m", read)
#pragma section("TCTEST$z", read)

#ifdef _WIN64
#define LINKER_INCLUDE_UNITTEST_PRAGMA(testname) __pragma(comment(linker, "/include:g_ptestinfo"#testname))
#else
#define LINKER_INCLUDE_UNITTEST_PRAGMA(testname) __pragma(comment(linker, "/include:_g_ptestinfo"#testname))
#endif

#define DECLSPEC_ALLOCATE extern "C" __declspec(allocate("TCTEST$m"))

#else

#define TCHAR char16_t
#define _T(x) u ## x

#define LINKER_INCLUDE_UNITTEST_PRAGMA(testname)
#define DECLSPEC_ALLOCATE

#endif

typedef void (*UnitTestProc)();

struct SUnitTestInfo {
	TCHAR const* m_szName;
	SSourceLocation m_srcloc;
	UnitTestProc m_testproc;
};

bool InsideUnitTest();

#define UNITTESTDEF(testname) \
	static void testname##UnitTest(); \
	static SUnitTestInfo const s_testinfo##testname = {_T(#testname), SOURCE_LOCATION, &testname##UnitTest}; \
	DECLSPEC_ALLOCATE \
	SUnitTestInfo const* const g_ptestinfo##testname = &s_testinfo##testname; \
	LINKER_INCLUDE_UNITTEST_PRAGMA(testname) \
	static void testname##UnitTest()

#else
#define UNITTESTDEF(testname) \
	template< typename T > /*function body should not be compiled in release build*/ \
	void testname##UnitTest()
#endif