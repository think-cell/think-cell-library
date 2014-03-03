#pragma once

#include "_Assert.h"
#include <tchar.h>

#pragma section("TCTEST$a", read)
#pragma section("TCTEST$m", read)
#pragma section("TCTEST$z", read)

typedef void (*UnitTestProc)();

struct SUnitTestInfo {
	TCHAR const* m_szName;
	SSourceLocation m_srcloc;
	UnitTestProc m_testproc;
};

#ifdef _WIN64
	#define LINKER_INCLUDE_UNITTEST_PRAGMA(testname) __pragma(comment(linker, "/include:g_ptestinfo"#testname))
#else
	#define LINKER_INCLUDE_UNITTEST_PRAGMA(testname) __pragma(comment(linker, "/include:_g_ptestinfo"#testname))
#endif

#if PERFORMUNITTESTS
bool InsideUnitTest();

#define UNITTESTDEF(testname) \
	static void testname##UnitTest(); \
	static SUnitTestInfo const s_testinfo##testname = {_T(#testname), SOURCE_LOCATION, &testname##UnitTest}; \
	extern "C" __declspec(allocate("TCTEST$m")) \
	SUnitTestInfo const* const g_ptestinfo##testname = &s_testinfo##testname; \
	LINKER_INCLUDE_UNITTEST_PRAGMA(testname) \
	static void testname##UnitTest()

#else
#define UNITTESTDEF(testname) \
	template< typename T > /*function body should not be compiled in release build*/ \
	void testname##UnitTest()
#endif