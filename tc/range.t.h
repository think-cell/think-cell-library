
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "range.h"
#include "container.h" // tc::vector

namespace tc {

	// this is for testing only, equivalent to slice(rng, begin(rng), end(rng)), but also works correctly on temporaries
	template< typename Rng >
	tc::make_sub_range_result_t< Rng > slice(Rng&& rng) noexcept {
		return tc::make_sub_range_result_t< Rng >( std::forward<Rng>(rng), tc::begin(rng), tc::end(rng) );
	}

	// Do we want/need something like this as a generic tool?
	template<typename Rng>
	auto const_slice(Rng const& rng) noexcept return_decltype(slice(rng)) 


	// create a generator range that gives the same values as the vector it takes (for testing)
	template< typename Value_type>
	struct generator_range_mock final {
		generator_range_mock(tc::vector<Value_type> const& v) noexcept : m_values(v) {}

		template< typename Func > break_or_continue operator()(Func func) & noexcept {
			break_or_continue bc=continue_;
			auto const itEnd=tc::end(m_values);
			for( auto it=tc::begin(m_values);
				it!=itEnd && continue_==(bc=continue_if_not_break( func, *it ));
				++it );
			return bc;
		}

		template< typename Func > break_or_continue operator()(Func func) const& noexcept {
			break_or_continue bc=continue_;
			auto const itEnd=tc::end(m_values);
			for( auto it=tc::begin(m_values);
				it!=itEnd && continue_==(bc=continue_if_not_break( func, *it ));
				++it );
			return bc;
		}
	private:
		tc::vector<Value_type> m_values;
	
	};

	template< typename Value_type > 
	generator_range_mock<Value_type> make_generator_range( tc::vector<Value_type> const& v ) noexcept {
		return generator_range_mock<Value_type>(v);
	}
}

//-----------------------------------------------------------------------------------------------------------------------------
// Unit test macros and output

#ifdef TC_PRIVATE

#include "Library/ErrorReporting/_Assert.h" // required by _ASSERTPRINT
#include "Library/ErrorReporting/UnitTest.h"

#define TEST_RANGE_EQUAL(EXPECT, IS) _ASSERT(tc::equal(EXPECT, IS))
#define TEST_EQUAL(EXPECT, IS) _ASSERTEQUAL(IS, EXPECT);
#define TEST_OUTPUT(...)

#else

#include <ostream>
#include <sstream>
#include <iostream>
#define UNITTEST_PRINT_NAME(testname) std::cerr << "Running Unit test '" << #testname << "' ..." << std::endl;

#define UNITTESTDEF(testname)                                                                                                 \
	void testname##UnitTest();                                                                                                \
	struct C##testname##UnitTest{                                                                                             \
		C##testname##UnitTest() {                                                                                             \
			UNITTEST_PRINT_NAME(testname)                                                                                     \
			testname##UnitTest();                                                                                             \
		}                                                                                                                     \
	};                                                                                                                        \
	C##testname##UnitTest g_unittest##testname;                                                                               \
	void testname##UnitTest()


namespace tc {
	//-------------------------------------------------------------------------------------------------------------------------
	// print_range - debug helper
	namespace detail {
		template<typename OStream>
		struct print_elem final {
			print_elem(OStream& os_, std::size_t max_elems) noexcept : os(os_), elems(max_elems) {} 

			template<typename Elem>
			break_or_continue operator()(Elem&& e) noexcept {
				os << e << ", ";
				--elems;
				if( elems > 0 ) {
					return continue_;
				} else {
					return break_;
				}
			}
			
			private:
				OStream& os;
				std::size_t elems;
		};
	}

	template<typename Rng>
	std::string dbg_print_rng(Rng&& rng, std::size_t max_elems = 50) noexcept {

		std::stringstream os;
		os << "[";
		for_each(rng, detail::print_elem<std::stringstream>(os, max_elems) );
		os << "]";
		return os.str();
	}
}

#	define TEST_RANGE_EQUAL(EXPECT, IS) {   bool e = tc::equal(EXPECT, IS);													\
											if (!e) {																		\
												std::cerr << "Fail: Ranges differ:\n"										\
															 "Expected: " << tc::dbg_print_rng(EXPECT) << "\n"				\
															 "Is      : " << tc::dbg_print_rng(IS) << std::endl;			\
												_ASSERT(tc::equal(EXPECT, IS));												\
											}}
#	define TEST_EQUAL(EXPECT, IS)       {   bool e = EXPECT == IS;															\
											if (!e) {																		\
												std::cerr << "Fail: Values differ:\n"										\
															 "Expected: " << EXPECT << "\n"									\
															 "Is      : " << IS << std::endl;								\
												_ASSERTEQUAL(IS, EXPECT);														\
											}}
#	define TEST_OUTPUT(...) std::cerr __VA_ARGS__

#endif

#define TEST_init_hack(CTYPE, ETYPE, NAME, ...) \
	ETYPE internal_array_##NAME[] = __VA_ARGS__; \
	auto NAME = CTYPE<ETYPE>(internal_array_##NAME, \
									internal_array_##NAME + sizeof(internal_array_##NAME)/sizeof(ETYPE));

#define UNUSED_TEST_VARIABLE(v) static_cast<void>(v)

#define TEST_RANGE_LENGTH(RNG, LENGTH) _ASSERTEQUAL(tc::size(RNG), LENGTH)
#define TEST_RANGE_NOT_EQUAL(EXPECT, IS) _ASSERT(!tc::equal(EXPECT, IS))
#define TEST_NOT_EQUAL(EXPECT, IS) _ASSERT(!(EXPECT == IS));

#define STATIC_ASSERT(...) static_assert((__VA_ARGS__), #__VA_ARGS__ " is not true.")
