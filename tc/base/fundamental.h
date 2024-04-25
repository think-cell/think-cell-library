
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/for_each.hpp>

#include <cstdint>
#include <limits>

#ifdef _MSC_VER
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

#ifdef _MSC_VER
	#define TC_FORCEINLINE [[msvc::forceinline]]
#else
	#define TC_FORCEINLINE [[gnu::always_inline, gnu::nodebug]]
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

#ifdef _MSC_VER
	// MSVC generates null checks when upcasting this (https://godbolt.org/z/o6zhWaKGo).
	#define MSVC_WORKAROUND_THIS (__assume(nullptr != this), this)
#else
	#define MSVC_WORKAROUND_THIS this
#endif

#ifdef _MSC_VER
	// MSVC seems to mistake the .template memfn<...>(...) syntax for the beginning of a template class.
	// Supposedly, this has been fixed in VS2019, but we still see it happening in a few places.
	#define MSVC_TEMPLATE_WORKAROUND
#else
	#define MSVC_TEMPLATE_WORKAROUND template
#endif

#ifdef _MSC_VER
#define MSVC_WORKAROUND // comment out this line for Microsoft internal compiler unit test
#endif

#ifdef MSVC_WORKAROUND
#define IF_MSVC_WORKAROUND(...) __VA_ARGS__
#define IF_NO_MSVC_WORKAROUND(...)
#define IF_MSVC_WORKAROUND_ELSE(argMsvc, argNoMsvc) argMsvc
#else
#define IF_MSVC_WORKAROUND(...)
#define IF_NO_MSVC_WORKAROUND(...) __VA_ARGS__
#define IF_MSVC_WORKAROUND_ELSE(argMsvc, argNoMsvc) argNoMsvc
#endif

#ifndef __clang__ 
#define TC_REQUIRES_CWG2369_WORKAROUND(cond) > requires (cond)
#else
#define TC_REQUIRES_CWG2369_WORKAROUND(cond) , /*not requires because of CWG issue 2369*/ std::enable_if_t<(cond)>* = nullptr>
#endif

#if defined(TC_PRIVATE) && defined(_MSC_VER)
// Please update *.natvis on modification
#define no_adl NQ
#define cont_emplace_back_detail NQ0
#define transform_adaptor_adl NQ1
#define concat_adaptor_adl NQ2
#define RefT_adl NQ3
#define generator_range_output_adaptor_adl NQ4
#define interval_adl NQ5
#define empty_range_adl NQ6
#define select_range_adaptor_adl NQ7
#define tuple_adl NQ8
#define _precT_adl NQ9
#define point_adl NQA
#define counting_iterator_adl NQB
#define cartesian_product_adaptor_adl NQC
#define union_adaptor_adl NQD
#define dense_map_adl NQE
#define append_no_adl NQF
#define explicit_convert_adl NQG
#define for_each_adl NQH
#define size_proxy_adl NQI
#define expanding_invoke_adl NQJ
#define literal_range_adl NQK
#define as_constexpr_no_adl NQL
#define less_key_adl NQM
#define lohi_adl NQN
#define sign_adl NQO
#define adjacent_adaptor_adl NQP
#define cartesian_product_adaptor_detail NQQ
#define reverse_adaptor_adl NQR
#define tuple_detail NQS
#define debug_output_impl_ns NQT
#define debug_output_adl NQU
#define hash_append_detail NQV
#define hash_append_adl NQW
#define enumset_adl NQX
#define make_lazy_adl NQY
#define static_auto_constexpr_no_adl NQZ
#define invoke_detail NQa
#define SNamedPropT_adl NQb
#define _fill_adl NQc
#define integral_as_padded_dec_adl NQd
#define section_entry_detail NQe
#define restricted_enum_adl NQf
#define _linestyle_adl NQg
#define index_iterator_impl NQh
#define SGridline_adl NQi
#define void_generator_type_check_no_adl NQj
#define EnterReportSection_detail NQk
//NQl
#define static_vector_adl NQm
#define com_ptr_adl NQn
#define EDateFormatType_adl NQo
#define value_epsilon_adl NQp
#define generator_range_adl NQq
#define find_first_if_detail NQr
#define convert_enc_impl NQs
#define iterator_facade_adl NQt
#define unique_adaptor_adl NQu
#define strong_typedef_adl NQv
#define explicit_convert_std_array_detail NQw
#endif
