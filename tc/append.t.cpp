
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"

static_assert(tc::is_appendable<std::basic_string<char>&, char const*>::value);
static_assert(!tc::is_appendable<std::basic_string<char>&, int>::value);
static_assert(tc::is_appendable<std::basic_string<char>&, tc::char16 const*, decltype(tc::as_dec(5))>::value);
static_assert(!tc::is_appendable<std::basic_string<char>&, tc::size_proxy<int>>::value);
static_assert(!tc::is_appendable<std::basic_string<char>&, tc::vector<int>>::value);
static_assert(!tc::is_appendable<std::basic_string<char>&, tc::static_vector<int, 3>>::value);
//static_assert(!tc::is_appendable<std::pair<std::basic_string<char>&, char const*>>::value);
//static_assert(!tc::is_appendable<std::tuple<std::basic_string<char>&, char const*>>::value);
//static_assert(!tc::is_appendable<tc::tuple<std::basic_string<char>&, char const*>>::value);

#ifdef TC_PRIVATE
static_assert(tc::is_appendable<tc::SReportStream&, int>::value);
static_assert(tc::is_appendable<tc::SReportStream&, tc::size_proxy<int>>::value);
static_assert(tc::is_appendable<tc::SReportStream&, char const*, tc::size_proxy<int>, char const*>::value);
#endif

UNITTESTDEF(nonappendable) {
	tc::vector<int> vecnTest{1, 2, 3};
	auto rngTest1=tc::transform(vecnTest, [](auto n) noexcept { return n + 1; });
	static_assert(!tc::is_appendable<std::basic_string<char>&, decltype(rngTest1)>::value);
	auto rngTest2=tc::filter(vecnTest, [](auto const n) noexcept { return n%2==1; });
	static_assert(!tc::is_appendable<std::basic_string<char>&, decltype(rngTest2)>::value);
}

namespace {
	struct SNonReportAppendable final {
		friend bool operator==(SNonReportAppendable const&, int) noexcept {
			return true;
		}
	};
}

UNITTESTDEF(nonreportappendable) {
	int n = 5;
	_ASSERTEQUAL(SNonReportAppendable(), n);
	VERIFYEQUAL(SNonReportAppendable(), n);
#ifdef TC_PRIVATE
	VERIFYPRED(SNonReportAppendable(), _==n);
#endif
}

#ifdef TC_PRIVATE
#include "Library/ErrorReporting/decl.h"

namespace {
	struct STestError1 {
		friend auto debug_output_impl(STestError1 const&) noexcept {
			return tc::concat("STestError1(", -1, ')');
		}
	};
	
	struct STestError2 {
		DECLARE_FRIEND_FN_DEBUG_OUTPUT_IMPL(STestError2)
	};

	DEFINE_FRIEND_FN_DEBUG_OUTPUT_IMPL(STestError2) {
		tc::for_each(tc::concat("STestError2(", 5, ", ", STestError1(), ')'), tc_move(appdr));
	}

	struct STestError3 {};

	auto debug_output_impl(STestError3 const&) noexcept {
		return [](tc::report_appender appdr) noexcept {
			tc::for_each(tc::concat("STestError3(", STestError2(), ", ", nullptr, ')'), tc_move(appdr));
		};
	}
}

UNITTESTDEF(SReportStreamTest) {
	tc::SReportStream rs(eerrlvlNOTRACE);
	char const* pt = nullptr;
	tc::append(rs, UTF16("abc["));
	tc::append(rs, 10);
	tc::append(rs, "][", pt, ']');
	tc::append(rs, STestError3());
	_ASSERTEQUAL(rs.m_str, "abc[10][nullptr]STestError3(STestError2(5, STestError1(-1)), nullptr)");
}
#endif
