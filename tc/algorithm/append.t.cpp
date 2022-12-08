
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "append.h"

#include "../unittest.h"
#include "../string/format.h"
#include "../static_vector.h"
#include "../range/filter_adaptor.h"


static_assert(tc::appendable<char const*, tc::string<char>&>);
static_assert(!tc::appendable<int, tc::string<char>&>);
static_assert(tc::appendable<tc::char16 const*, tc::string<char>&>);
static_assert(tc::appendable<decltype(tc::as_dec(5)), tc::string<char>&>);
static_assert(!tc::appendable<tc::size_proxy<int>, tc::string<char>&>);
static_assert(!tc::appendable<tc::vector<int>, tc::string<char>&>);
static_assert(!tc::appendable<tc::static_vector<int, 3>, tc::string<char>&>);

UNITTESTDEF(nonappendable) {
	tc::vector<int> vecnTest{1, 2, 3};
	auto rngTest1=tc::transform(vecnTest, [](auto n) noexcept { return n + 1; });
	static_assert(!tc::appendable<decltype(rngTest1), tc::string<char>&>);
	auto rngTest2=tc::filter(vecnTest, [](auto const n) noexcept { return n%2==1; });
	static_assert(!tc::appendable<decltype(rngTest2), tc::string<char>&>);
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
			return tc::concat("STestError1(", tc::debug_output(-1), ")");
		}
	};
	
	struct STestError2 {
		DECLARE_FRIEND_FN_DEBUG_OUTPUT_IMPL(STestError2)
	};

	DEFINE_FRIEND_FN_DEBUG_OUTPUT_IMPL(STestError2) {
		tc::for_each(tc::concat("STestError2(", tc::debug_output(5), ", ", tc::debug_output(STestError1()), ")"), tc_move(appdr));
	}

	struct STestError3 {};

	auto debug_output_impl(STestError3 const&) noexcept {
		return [](tc::report_appender appdr) noexcept {
			tc::for_each(tc::concat("STestError3(", tc::debug_output(STestError2()), ", ", tc::debug_output(nullptr), ")"), tc_move(appdr));
		};
	}
}

UNITTESTDEF(SReportStreamTest) {
	tc::SReportStream rs;
	char const* pt = nullptr;
	tc::append(rs, UTF16("abc["));
	tc::append(rs, tc::debug_output(10));
	tc::append(rs, "][", tc::debug_output(pt), "]");
	tc::append(rs, tc::debug_output(STestError3()));
	_ASSERTEQUAL(rs.m_str, "abc[10][nullptr]STestError3(STestError2(5, STestError1(-1)), nullptr)");
}
#endif
