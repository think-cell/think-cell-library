#pragma once

#include <boost/preprocessor/repetition/repeat.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/preprocessor/repetition/enum_binary_params.hpp>
#include <boost/preprocessor/arithmetic/inc.hpp>

#define MAX_PERFECT_FORWARD_PARAMS 16

#define PERFECT_FORWARD_CALL_(z,n,_) std::forward< A ## n >( a ## n )
#define PERFECT_FORWARD_(z, n, _) \
	PART1() BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), typename A) \
	PART2() BOOST_PP_ENUM_BINARY_PARAMS(BOOST_PP_INC(n), A, && a) \
	PART3() BOOST_PP_ENUM(BOOST_PP_INC(n), PERFECT_FORWARD_CALL_, _ ) \
	PART4()
#define PERFECT_FORWARD BOOST_PP_REPEAT( MAX_PERFECT_FORWARD_PARAMS, PERFECT_FORWARD_, _ )
