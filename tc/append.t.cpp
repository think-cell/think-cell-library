
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"

static_assert(tc::is_appendable<std::string&, char const*>::value);
static_assert(!tc::is_appendable<std::string&, int>::value);
static_assert(tc::is_appendable<std::string&, tc::char16 const*, decltype(tc::as_dec(5))>::value);
static_assert(!tc::is_appendable<std::string&, tc::size_proxy<int>>::value);
static_assert(!tc::is_appendable<std::string&, tc::vector<int>>::value);
static_assert(!tc::is_appendable<std::string&, tc::static_vector<int, 3>>::value);
#ifdef TC_PRIVATE
static_assert(tc::is_appendable<tc::SReportStream&, int>::value);
static_assert(tc::is_appendable<tc::SReportStream&, tc::size_proxy<int>>::value);
static_assert(tc::is_appendable<tc::SReportStream&, char const*, tc::size_proxy<int>, char const*>::value);
#endif

UNITTESTDEF(nonappendable) {
	tc::vector<int> vecnTest{1, 2, 3};
	auto rngTest1=tc::transform(vecnTest, [](auto n) noexcept { return n + 1; });
	static_assert(!tc::is_appendable<std::string&, decltype(rngTest1)>::value);
	auto rngTest2=tc::filter(vecnTest, [](auto const n) noexcept { return n%2==1; });
	static_assert(!tc::is_appendable<std::string&, decltype(rngTest2)>::value);
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
		auto error() const& noexcept {
			return tc::concat("STestError1(", -1, ')');
		}
	};
	
	struct STestError2 {
		void operator()(tc::report_appender appdr) const& noexcept {
			tc::for_each(tc::concat("STestError2(", 5, ", ", STestError1(), ')'), tc_move(appdr));
		}
	};

	struct STestError3 {};

	auto error(STestError3 const&) noexcept {
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
