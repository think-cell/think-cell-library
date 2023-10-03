
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"

#include "make_c_str.h"

#ifdef TC_PRIVATE
#include "Library/Internationalization/i18n_fwd.h"
#endif

static_assert(tc::has_c_str<std::string const&, char const>);
static_assert(!tc::has_c_str<std::string const&, char>);
static_assert(tc::has_c_str<std::string&, char const>);
static_assert(tc::has_c_str<std::string&, char>);
static_assert(tc::has_c_str<std::string&&, char const>);
static_assert(tc::has_c_str<std::string&&, char>);
static_assert(tc::has_c_str<char const*, char const>);
static_assert(!tc::has_c_str<char const*, char>);
static_assert(tc::has_c_str<char*, char const>);
static_assert(tc::has_c_str<char*, char>);

namespace {
	template<typename Char1, typename Char2> requires std::is_same<Char1, Char2>::value
	bool check_make_c_str_fwd(Char1 const* str1, Char2 const* str2) noexcept {
		return str1 == str2;
	}

	template<typename Char1, typename Char2>
	bool check_make_c_str_fwd(Char1 const*, Char2 const*) noexcept {
		return false;
	}
}

UNITTESTDEF(make_c_str_fwd_test) {
	tc::string<char> str1("ab");
	_ASSERT(check_make_c_str_fwd<char>(tc::make_c_str(str1), tc::as_c_str(str1)));
	_ASSERT(!check_make_c_str_fwd<tc::char16>(tc::make_c_str(tc::convert_enc<tc::char16>(str1)), tc::as_c_str(str1)));
	tc::string<char> const str2("cd");
	_ASSERT(check_make_c_str_fwd<char>(tc::make_c_str(str2), tc::as_c_str(str2)));
	_ASSERT(!check_make_c_str_fwd<tc::char16>(tc::make_c_str(tc::convert_enc<tc::char16>(str2)), tc::as_c_str(str2)));
	char const* str3 = "ef";
	_ASSERT(check_make_c_str_fwd<char>(tc::make_c_str(str3), tc::as_c_str(str3)));
	_ASSERT(!check_make_c_str_fwd<tc::char16>(tc::make_c_str(tc::convert_enc<tc::char16>(str3)), tc::as_c_str(str3)));
	char ach4[] = {"gh"};
	_ASSERT(check_make_c_str_fwd<char>(tc::make_c_str(ach4), tc::as_c_str(ach4)));
	tc::span<char const> str5 = str1;
	_ASSERT(!check_make_c_str_fwd<char>(tc::make_c_str(str5), tc::ptr_begin(str5)));
#ifdef TC_PRIVATE
	tc::uichar const* str6 = UISTR("ij");
	_ASSERTEQUAL(TC_FWD(std::is_same<tc::uichar, char>::value), check_make_c_str_fwd<char>(tc::make_c_str(tc::convert_enc<char>(str6)), tc::as_c_str(str6)));
#endif
}

namespace {
	template<typename Char, typename Str>
	bool check_make_c_str(Char const* str1, Str const& str2) noexcept {
		return tc::equal(str1, str2);
	}
}

UNITTESTDEF(make_c_str_test) {
	tc::string<char> str1("ab");
	auto strLocal1 = tc::make_c_str(str1);
	_ASSERT(check_make_c_str<char>(strLocal1, "ab"));
	_ASSERT(check_make_c_str<char>(tc::make_c_str(tc::string<char>("cd")), "cd"));
	auto strLocal3 = tc::make_c_str(tc::string<char>("ef"));
	_ASSERT(check_make_c_str<char>(strLocal3, "ef"));
#ifdef TC_PRIVATE
	_ASSERT(check_make_c_str<tc::uichar>(tc::make_c_str<tc::uichar>(tc_ascii("ab"), tc_ascii("cd"), tc_ascii("ef")), tc::make_str<tc::uichar>("ab", "cd", "ef")));
	_ASSERT(check_make_c_str<char>(tc::make_c_str<char>(tc_ascii("abcdef")), tc::explicit_cast<tc::string<char>>(UISTR("abcdef"))));
#endif
}

