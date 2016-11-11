//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once
#include "range.h"
#include "container.h" // tc::vector
#include "initializer_list.h"

namespace tc {

	// this is for testing only, equivalent to slice(rng, begin(rng), end(rng)), but also works correctly on temporaries
	template< typename Rng >
	typename make_sub_range_result< Rng >::type slice(Rng&& rng) noexcept {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), boost::begin(rng), boost::end(rng) );
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
			auto const itEnd=boost::end(m_values);
			for( auto it=boost::begin(m_values);
				it!=itEnd && continue_==(bc=continue_if_not_break( func, *it ));
				++it );
			return bc;
		}

		template< typename Func > break_or_continue operator()(Func func) const& noexcept {
			break_or_continue bc=continue_;
			auto const itEnd=boost::end(m_values);
			for( auto it=boost::begin(m_values);
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
#define TEST_EQUAL(EXPECT, IS) _ASSERT(EXPECT == IS);
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
			break_or_continue operator()(Elem&& e) {
				os << e << ", ";
				--elems;
				return (elems > 0) ? continue_ : break_;
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

#	define TEST_RANGE_EQUAL(EXPECT, IS) {   bool e = tc::equal(EXPECT, IS);													  \
											if (!e) {																		  \
												std::cerr << "Fail: Ranges differ:\n"										  \
															 "Expected: " << dbg_print_rng(EXPECT) << "\n"					  \
															 "Is      : " << dbg_print_rng(IS) << std::endl;				  \
												_ASSERT(tc::equal(EXPECT, IS));												  \
											}}
#	define TEST_EQUAL(EXPECT, IS)       {   bool e = EXPECT == IS;															  \
											if (!e) {																		  \
												std::cerr << "Fail: Values differ:\n"										  \
															 "Expected: " << EXPECT << "\n"									  \
															 "Is      : " << IS << std::endl;								  \
												_ASSERT(EXPECT == IS);														  \
											}}
#	define TEST_OUTPUT(...) std::cerr __VA_ARGS__

#endif

#define TEST_init_hack(CTYPE, ETYPE, NAME, ...)                                                                               \
	ETYPE internal_array_##NAME[] = __VA_ARGS__;                                                                              \
	auto NAME = CTYPE<ETYPE>(internal_array_##NAME,                                                                           \
									internal_array_##NAME + sizeof(internal_array_##NAME)/sizeof(ETYPE));

#define UNUSED_TEST_VARIABLE(v) (v)

#define TEST_RANGE_LENGTH(RNG, LENGTH) _ASSERT(tc::size(RNG) == LENGTH)
#define TEST_RANGE_NOT_EQUAL(EXPECT, IS) _ASSERT(!tc::equal(EXPECT, IS))
#define TEST_NOT_EQUAL(EXPECT, IS) _ASSERT(!(EXPECT == IS));

#define TEST_OUTPUT_RANGE(Rng) TEST_OUTPUT( << #Rng << " = " << dbg_print_rng(Rng) << std::endl)
#define STATIC_ASSERT(...) static_assert((__VA_ARGS__), #__VA_ARGS__ " is not true.")
