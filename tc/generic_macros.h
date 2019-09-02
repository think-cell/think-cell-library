
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/seq/transform.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/enum.hpp>
#include <boost/preprocessor/seq/pop_back.hpp>
#include <boost/preprocessor/control/iif.hpp>

#define PP_APPLY_MACRO(state, macro, elem) macro(elem)

// expands to "macro(elem1)macro(elem2)...macro(elemN)"
#define PP_CAT_TRANSFORMED_SEQ(macro, seq) \
	BOOST_PP_SEQ_CAT( BOOST_PP_SEQ_TRANSFORM(PP_APPLY_MACRO, macro, seq) )

#define PP_HELPER_DELIMITED_ELEM(state, pair, elem) \
	BOOST_PP_SEQ_ELEM(0, pair) \
	BOOST_PP_SEQ_ELEM(1, pair)(elem)

// expands to "macro(elem1) delimiter macro(elem2) delimiter ... delimiter macro(elemN)"
#define PP_DELIMIT_TRANSFORMED_SEQ(macro, delimiter, seq) \
	macro(BOOST_PP_SEQ_HEAD(seq)) \
	BOOST_PP_IF( \
		BOOST_PP_LESS(1, BOOST_PP_SEQ_SIZE(seq)), \
		BOOST_PP_SEQ_FOR_EACH, \
		BOOST_PP_EAT \
	) ( PP_HELPER_DELIMITED_ELEM, (delimiter)(macro), BOOST_PP_SEQ_TAIL(seq) )

// expands to "macro(elem1) macro(elem2) macro(elemN)"
#define PP_UNPACK_TRANSFORMED_SEQ(macro, seq) \
	BOOST_PP_SEQ_FOR_EACH(PP_APPLY_MACRO, macro, seq)

#define BOOST_PP_SEQ_PUSH_FRONT_SUPPORT_NIL(seq, elem) \
	BOOST_PP_IF(BOOST_PP_SEQ_IS_NIL(seq), (elem), BOOST_PP_SEQ_PUSH_FRONT(seq, elem))

#define BOOST_PP_SEQ_POP_BACK_SUPPORT_NIL(seq) \
	BOOST_PP_IF(BOOST_PP_DEC(BOOST_PP_SEQ_SIZE_SUPPORT_NIL(seq)), BOOST_PP_SEQ_POP_BACK(seq), BOOST_PP_SEQ_NIL)

// expands to "op(elem1), op(elem2), ...., op(elemN)"
#define PP_ENUM_TRANSFORMED_SEQ(op, data, seq) \
	BOOST_PP_SEQ_ENUM_SUPPORT_NIL( \
		BOOST_PP_SEQ_POP_BACK_SUPPORT_NIL( \
			BOOST_PP_SEQ_TRANSFORM(op, data, BOOST_PP_SEQ_PUSH_BACK(seq, (~)(~))) \
		) \
	)

// expands a variable pair into its type:
// ((type)(name)) --> type
#define PP_PAIR_VAR_TYPE(pairTypeName) \
	BOOST_PP_SEQ_ELEM(0, pairTypeName)

// expands a variable pair into its name:
// ((type)(name)) --> name
#define PP_PAIR_VAR_NAME(pairTypeName) \
	BOOST_PP_SEQ_ELEM(1, pairTypeName)

// expands a variable pair:
// ((type)(name)) --> type name
#define PP_PAIR_VAR(pairTypeName) \
	PP_PAIR_VAR_TYPE(pairTypeName) PP_PAIR_VAR_NAME(pairTypeName)

// expands a variable pair into its definition (with semicolon, useful for defining members from a list of pairs)
// ((type)(name)) --> type name;
#define PP_PAIR_VAR_DEFINE(pairTypeName) \
	PP_PAIR_VAR(pairTypeName);

// expands a list of variable pairs into function argument definitions:
// "type1 name1, type2 name2, ... , typeN nameN"
#define PP_PAIRS_TO_FUNC_ARG_DEFS(seqArgs) \
	PP_ENUM_TRANSFORMED_SEQ(PP_APPLY_MACRO, PP_PAIR_VAR, seqArgs)

// expands a list of variable pairs into function call arguments (losing the types):
// "name1, name2, ... , nameN"
#define PP_PAIRS_TO_FUNC_CALL_ARGS(seqArgs) \
	PP_ENUM_TRANSFORMED_SEQ(PP_APPLY_MACRO, PP_PAIR_VAR_NAME, seqArgs)

#define OP_PAIR_TO_SPACE_SEPARATED_PAIR(state, data, pair) \
	BOOST_PP_SEQ_ELEM(0, pair) BOOST_PP_SEQ_ELEM(1, pair)

#define OP_PAIR_TO_1ST_ELEMENT(state, data, pair) \
	BOOST_PP_SEQ_ELEM(0, pair)

#define OP_PAIR_TO_2ND_ELEMENT(state, data, pair) \
	BOOST_PP_SEQ_ELEM(1, pair)

// Macro for accessing nameless union members withing MS structs, which we can't control (can't name the union).
// In Mac nameless union is not allowed, so the union has a name.
#ifdef __clang__
#define WIN32_UNION_MEMBER(_obj_ptr, _member) \
    (_obj_ptr)->DUMMYUNIONNAME._member
#else
#define WIN32_UNION_MEMBER(_obj_ptr, _member) \
    (_obj_ptr)->_member
#endif

#define PP_3RD_ARGUMENT(f, x, t) t

#ifndef __clang__
	#define PP_SEQ_EXPAND(seq) BOOST_PP_SEQ_FOR_EACH(PP_3RD_ARGUMENT, _, seq)
#else
	#define PP_SEQ_EXPAND(seq) BOOST_PP_EXPAND(BOOST_PP_SEQ_FOR_EACH(PP_3RD_ARGUMENT, _, seq))
#endif


// Use if you have to check if an expression compiles, e.g., to check if an operator is defined, a cast is valid etc
#define TC_HAS_EXPR_TEMPLATE_PARAMETER(Type) typename Type
// The following implementation would be more compact and canonical:
// 		#define TC_HAS_EXPR(name, seqArgs, ...) \
// 		template< BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_TRANSFORM(PP_APPLY_MACRO, TC_HAS_EXPR_TEMPLATE_PARAMETER, seqArgs)), typename=void > \
// 		struct BOOST_PP_CAT(has_,name) : std::false_type {}; \
// 		template< BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_TRANSFORM(PP_APPLY_MACRO, TC_HAS_EXPR_TEMPLATE_PARAMETER, seqArgs)) > \
// 		struct BOOST_PP_CAT(has_,name)<BOOST_PP_SEQ_ENUM(seqArgs), tc::void_t<decltype(__VA_ARGS__)>> : std::true_type {};
// but MSVC seems to get confused by pattern matching, when call trees are too deep.
#define TC_HAS_EXPR_EXTERNAL_PARAMETER(Type) BOOST_PP_CAT(External,Type)
#define TC_HAS_EXPR(name, seqArgs, ...) \
template< BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_TRANSFORM(PP_APPLY_MACRO, TC_HAS_EXPR_TEMPLATE_PARAMETER, BOOST_PP_SEQ_TRANSFORM(PP_APPLY_MACRO, TC_HAS_EXPR_EXTERNAL_PARAMETER, seqArgs))) > \
struct BOOST_PP_CAT(has_,name) { \
private: \
	template< typename... > \
	static std::false_type check(...); /*unevaluated-only*/ \
	template< BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_TRANSFORM(PP_APPLY_MACRO, TC_HAS_EXPR_TEMPLATE_PARAMETER, seqArgs)) > \
	static std::true_type check(tc::void_t<decltype(__VA_ARGS__)>*); /*unevaluated-only*/ \
public: \
	static constexpr bool value = std::is_same<std::true_type, decltype(check<BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_TRANSFORM(PP_APPLY_MACRO, TC_HAS_EXPR_EXTERNAL_PARAMETER, seqArgs))>(nullptr))>::value; \
};
