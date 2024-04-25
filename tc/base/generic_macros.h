
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/comparison/equal.hpp>
#include <boost/preprocessor/comparison/less.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/seq/cat.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/enum.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/seq/push_back.hpp>
#include <boost/preprocessor/seq/push_front.hpp>
#include <boost/preprocessor/seq/pop_back.hpp>
#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/seq/transform.hpp>
#include <boost/preprocessor/seq/reverse.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

#define TC_EXPAND(...) __VA_ARGS__

#define TC_PP_APPLY_MACRO(state, macro, elem) macro(elem)

// According to boost reference https://www.boost.org/doc/libs/1_73_0/libs/preprocessor/doc/ref/seq_nil.html,
// BOOST_PP_SEQ_NIL is only a placeholder for BOOST_PP_SEQ_PUSH_BACK.
// We should not use BOOST_PP_SEQ_NIL as an empty sequence when calling BOOST_PP_SEQ_xxx macros (except BOOST_PP_SEQ_PUSH_BACK).
// However, TC_PP_SEQ_xxx macros support BOOST_PP_SEQ_NIL as an empty sequence. We should use TC_PP_SEQ_xxx if the sequence can be NIL. 

#define TC_PP_SEQ_SIZE(seq) BOOST_PP_DEC(BOOST_PP_SEQ_SIZE(BOOST_PP_SEQ_PUSH_BACK(seq,~)))
#define TC_PP_SEQ_IS_NIL(seq) BOOST_PP_EQUAL(TC_PP_SEQ_SIZE(seq),0 )

#define TC_PP_IF(cond, t, f) BOOST_PP_EXPAND(TC_FWD BOOST_PP_IF(cond, (t), (f)))
#define TC_PP_IF_SEQ_NOT_NIL(seq, ...) TC_PP_IF(TC_PP_SEQ_IS_NIL(seq), , TC_FWD(__VA_ARGS__))

#define TC_PP_SEQ_HELPER_PUSH_FRONT_TO_NIL(seq, elem) (elem)
#define TC_PP_SEQ_PUSH_FRONT(seq, elem) \
	BOOST_PP_IF(TC_PP_SEQ_IS_NIL(seq), TC_PP_SEQ_HELPER_PUSH_FRONT_TO_NIL, BOOST_PP_SEQ_PUSH_FRONT)(seq, elem)

#define TC_PP_SEQ_HELPER_TO_NIL(seq) BOOST_PP_SEQ_NIL
#define TC_PP_SEQ_POP_BACK(seq) \
	BOOST_PP_IF(BOOST_PP_DEC(TC_PP_SEQ_SIZE(seq)), BOOST_PP_SEQ_POP_BACK, TC_PP_SEQ_HELPER_TO_NIL)(seq)

#define TC_PP_SEQ_CAT(seq) \
	BOOST_PP_IF(TC_PP_SEQ_IS_NIL(seq), BOOST_PP_EAT, BOOST_PP_SEQ_CAT)(seq)

#define TC_PP_SEQ_FOR_EACH(macro, data, seq) \
	BOOST_PP_IF(TC_PP_SEQ_IS_NIL(seq), BOOST_PP_EAT, BOOST_PP_SEQ_FOR_EACH)(macro, data, seq)

#define TC_PP_SEQ_ENUM(seq) \
	BOOST_PP_IF(TC_PP_SEQ_IS_NIL(seq), BOOST_PP_EAT, BOOST_PP_SEQ_ENUM)(seq)

#define TC_PP_SEQ_TRANSFORM(op, data, seq) \
	BOOST_PP_IF(TC_PP_SEQ_IS_NIL(seq), BOOST_PP_EAT, BOOST_PP_SEQ_TRANSFORM)(op, data, seq)

// expands to "macro(elem1)macro(elem2)...macro(elemN)"
#define TC_PP_CAT_TRANSFORMED_SEQ(macro, seq) \
	BOOST_PP_IF(TC_PP_SEQ_IS_NIL(seq), BOOST_PP_EAT, TC_PP_SEQ_CAT)(TC_PP_SEQ_TRANSFORM(TC_PP_APPLY_MACRO, macro, seq))

// expands to "op(elem1), op(elem2), ...., op(elemN)"
#define TC_PP_ENUM_TRANSFORMED_SEQ(op, data, seq) \
	BOOST_PP_IF(TC_PP_SEQ_IS_NIL(seq), BOOST_PP_EAT, BOOST_PP_SEQ_ENUM)(TC_PP_SEQ_TRANSFORM(op, data, seq))

// expands to "macro(elem1) macro(elem2) macro(elemN)"
#define TC_PP_UNPACK_TRANSFORMED_SEQ(macro, seq) \
	TC_PP_SEQ_FOR_EACH(TC_PP_APPLY_MACRO, macro, seq)

// expands to "(macro(elem))(delimiter)"
#define TC_PP_HELPER_DELIMIT_TRANSFORMED_SEQ(state, pair, elem) \
	(BOOST_PP_SEQ_ELEM(0, pair)(elem))(BOOST_PP_SEQ_ELEM(1, pair))

// expands to "macro(elem1) delimiter macro(elem2) delimiter ... delimiter macro(elemN)"
#define TC_PP_DELIMIT_TRANSFORMED_SEQ(macro, delimiter, seq) \
	TC_PP_UNPACK_TRANSFORMED_SEQ( \
		TC_EXPAND, \
		TC_PP_SEQ_POP_BACK(BOOST_PP_IF( \
			TC_PP_SEQ_SIZE(seq), \
			TC_PP_SEQ_FOR_EACH(TC_PP_HELPER_DELIMIT_TRANSFORMED_SEQ, (macro)(delimiter), seq), /*(macro(elem1))(delimiter)...(delimiter)(macro(elemN))(delimiter)*/ \
			BOOST_PP_SEQ_NIL \
		)) /*(macro(elem1))(delimiter)...(delimiter)(macro(elemN))*/ \
	)

// expands a parameter seq into its pack:
// ((type)(...)(name)) --> ...
// ((type)(name)) -->
#define TC_PP_PARAM_PACK(seq) \
	BOOST_PP_IF(BOOST_PP_EQUAL(TC_PP_SEQ_SIZE(seq),3), BOOST_PP_SEQ_ELEM, BOOST_PP_EAT)(1, seq)

// expands a parameter seq into its type:
// ((type)(...)(name)) --> type
// ((type)(name)) --> type
#define TC_PP_PARAM_TYPE(seq) \
	BOOST_PP_SEQ_ELEM(0, seq)

// expands a parameter seq into its type pack:
// ((type)(...)(name)) --> type ...
// ((type)(name)) --> type
#define TC_PP_PARAM_TYPE_PACK(seq) \
	TC_PP_PARAM_TYPE(seq) TC_PP_PARAM_PACK(seq)

// expands a parameter seq into its name:
// ((type)(...)(name)) --> name
// ((type)(name)) --> name
#define TC_PP_PARAM_NAME(seq) \
	BOOST_PP_SEQ_HEAD(BOOST_PP_SEQ_REVERSE(seq))

// expands a parameter seq into its name pack:
// ((type)(...)(name)) --> name ...
// ((type)(name)) --> name
#define TC_PP_PARAM_NAME_PACK(seq) \
	TC_PP_PARAM_NAME(seq) TC_PP_PARAM_PACK(seq)

// expands a parameter seq:
// ((type)(...)(name)) --> type ... name
// ((type)(name)) --> type name
#define TC_PP_PARAM(seq) \
	TC_PP_PARAM_TYPE_PACK(seq) TC_PP_PARAM_NAME(seq)

// expands a variable pair into its type:
// ((type)(name)) --> type
#define TC_PP_PAIR_VAR_TYPE(pairTypeName) \
	BOOST_PP_SEQ_ELEM(0, pairTypeName)

// expands a variable pair into its name:
// ((type)(name)) --> name
#define TC_PP_PAIR_VAR_NAME(pairTypeName) \
	BOOST_PP_SEQ_ELEM(1, pairTypeName)

// expands a variable pair:
// ((type)(name)) --> type name
#define TC_PP_PAIR_VAR(pairTypeName) \
	TC_PP_PAIR_VAR_TYPE(pairTypeName) TC_PP_PAIR_VAR_NAME(pairTypeName)

// expands a variable pair into its definition (with semicolon, useful for defining members from a list of pairs)
// ((type)(name)) --> type name;
#define TC_PP_PAIR_VAR_DEFINE(pairTypeName) \
	TC_PP_PAIR_VAR(pairTypeName);

// expands parameter seq to enum. e.g. (template|function definition) parameter list supporting optional parameter pack:
// "type1 (...)? name1, type2 (...)? name2, ... , typeN (...)? nameN"
#define TC_PP_PARAMS_ENUM(seqArgs) \
	TC_PP_ENUM_TRANSFORMED_SEQ(TC_PP_APPLY_MACRO, TC_PP_PARAM, seqArgs)

// expands parameter seq to argument enum (losing the types). e.g. (template|function call) argument list supporting optional parameter pack:
// "name1 (...)?, name2 (...)?, ... , nameN (...)?"
#define TC_PP_PARAMS_ARG_ENUM(seqArgs) \
	TC_PP_ENUM_TRANSFORMED_SEQ(TC_PP_APPLY_MACRO, TC_PP_PARAM_NAME_PACK, seqArgs)

// expands parameter seq to type enum (losing the names). e.g. (template template|function decl) parameter list supporting optional parameter pack:
// "type1 (...)?, type2 (...)?, ... , typeN (...)?
#define TC_PP_PARAMS_TYPE_ENUM(seqArgs) \
	TC_PP_ENUM_TRANSFORMED_SEQ(TC_PP_APPLY_MACRO, TC_PP_PARAM_TYPE_PACK, seqArgs)

#define TC_OP_PAIR_TO_SPACE_SEPARATED_PAIR(state, data, pair) \
	BOOST_PP_SEQ_ELEM(0, pair) BOOST_PP_SEQ_ELEM(1, pair)

#define TC_OP_PAIR_TO_1ST_ELEMENT(state, data, pair) \
	BOOST_PP_SEQ_ELEM(0, pair)

#define TC_OP_PAIR_TO_2ND_ELEMENT(state, data, pair) \
	BOOST_PP_SEQ_ELEM(1, pair)

// Macro for accessing nameless union members within MS structs, which we can't control (can't name the union).
// In Mac nameless union is not allowed, so the union has a name.
#ifdef __clang__
#define WIN32_UNION_MEMBER(_obj_ptr, _member) \
    (_obj_ptr)->DUMMYUNIONNAME._member
#else
#define WIN32_UNION_MEMBER(_obj_ptr, _member) \
    (_obj_ptr)->_member
#endif

// Use if you have to check if an expression compiles, e.g., to check if an operator is defined, a cast is valid etc
#define TC_HAS_EXPR_TEMPLATE_PARAMETER(Type) typename Type
#define TC_HAS_EXPR(name, seqArgs, ...) \
template< BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_TRANSFORM(TC_PP_APPLY_MACRO, TC_HAS_EXPR_TEMPLATE_PARAMETER, seqArgs)) > \
concept has_ ## name = requires { __VA_ARGS__; };
