
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/for_each.hpp>

#ifndef __clang__
	#define MODIFY_WARNING_HELPER(r, data, subseq) __pragma( warning( BOOST_PP_SEQ_ELEM(0, subseq): BOOST_PP_SEQ_ELEM(1, subseq) ) )
	#define MODIFY_WARNINGS(seq) BOOST_PP_SEQ_FOR_EACH(MODIFY_WARNING_HELPER, _, seq)
	#define MODIFY_WARNINGS_BEGIN(seq) \
		__pragma( warning( push ) ) \
		MODIFY_WARNINGS(seq)
	#define MODIFY_WARNINGS_END __pragma( warning( pop ) )
#else
	#define MODIFY_WARNINGS(seq)
	#define MODIFY_WARNINGS_BEGIN(seq)
	#define MODIFY_WARNINGS_END
#endif

#ifdef _MSC_VER
	// https://devblogs.microsoft.com/cppblog/optimizing-the-layout-of-empty-base-classes-in-vs2015-update-2-3/
	#define TC_EMPTY_BASES __declspec(empty_bases)
#else
	#define TC_EMPTY_BASES
#endif

#if 201803L <= __has_cpp_attribute(unlikely)
	// There is also the [[likely]] attribute, but we prefer to only annotate unlikely code paths to avoid redundancy.
	#define TC_UNLIKELY [[unlikely]]
#else
	#define TC_UNLIKELY
#endif


#define THROW(...) noexcept(false)

// MAYTHROW is introduced for those very generic functions which may throw only with a few types, but we cannot
// handle the exceptions in those functions. The exceptions should be handled where the function is called.
#define MAYTHROW noexcept(false)

// #define FOO(x) FOO_(TC_FWD(x))
// Always use TC_FWD instead of parentheses to forward an argument to another macro. If the expansion of x has
// unshielded comma(s), TC_FWD(x) will still be recognized as a single argument by FOO_ and then forward x
// to FOO_ with its comma(s), but without putting extra parentheses around x as FOO_((x)) would do.
#define TC_FWD(...) __VA_ARGS__

namespace tc {
	// Used for explicitly discarding return value from function decorated with [[nodiscard]] attribute.
	// Also used to suppress 'unused variable' warnings, if no better option available.
	template <typename T>
	constexpr void discard(T&&) noexcept{}
}

