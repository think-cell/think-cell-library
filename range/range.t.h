#pragma once
#include "Range.h"

#include <vector>

namespace RANGE_PROPOSAL_NAMESPACE {

	// this is for testing only, equivalent to slice(rng, begin(rng), end(rng)), but also works correctly on temporaries
	template< typename Rng >
	typename make_sub_range_result< Rng >::type slice(Rng&& rng) {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), boost::begin(rng), boost::end(rng) );
	}

	// Do we want/need something like this as a generic tool?
	template<typename Rng>
	auto const_slice(Rng const& rng) return_decltype(slice(rng)) 


	// create a generator range that gives the same values as the vector it takes (for testing)
	template< typename Value_type>
	struct generator_range_mock {
		generator_range_mock(std::vector<Value_type> const & v) : m_values(v) {}

		template< typename Func > break_or_continue operator()(Func func) {
			break_or_continue bc=continue_;
			auto const itEnd=boost::end(m_values);
			for( auto it=boost::begin(m_values);
				it!=itEnd && continue_==(bc=continue_if_not_break( func, *it ));
				++it );
			return bc;
		}

		template< typename Func > break_or_continue operator()(Func func) const {
			break_or_continue bc=continue_;
			auto const itEnd=boost::end(m_values);
			for( auto it=boost::begin(m_values);
				it!=itEnd && continue_==(bc=continue_if_not_break( func, *it ));
				++it );
			return bc;
		}
	private:
		std::vector<Value_type> m_values;
	
	};

	template< typename Value_type > 
	generator_range_mock<Value_type> make_generator_range( std::vector<Value_type> const& v ) {
		return generator_range_mock<Value_type>(v);
	}
}

//-----------------------------------------------------------------------------------------------------------------------------
// Unit test macros and output
#ifdef RANGE_UNITTEST_OUTPUT
#	define UNITTEST_OUTPUT
#endif

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE

#	include "UnitTest.h"

#else

#	ifdef UNITTEST_OUTPUT
#		include <iostream>
#		define UNITTEST_PRINT_NAME(testname) std::cerr << "Running Unit test '" << #testname << "' ..." << std::endl;
#	else
#		define UNITTEST_PRINT_NAME(testname)
#	endif
#	define UNITTESTDEF(testname)                                                                                              \
	void testname##UnitTest();                                                                                                \
	struct C##testname##UnitTest{                                                                                             \
		C##testname##UnitTest() {                                                                                             \
			UNITTEST_PRINT_NAME(testname)                                                                                     \
			testname##UnitTest();                                                                                             \
		}                                                                                                                     \
	};                                                                                                                        \
	C##testname##UnitTest g_unittest##testname;                                                                               \
	void testname##UnitTest()

#endif



#define TEST_init_hack(CTYPE, ETYPE, NAME, ...)                                                                               \
	ETYPE internal_array_##NAME[] = __VA_ARGS__;                                                                              \
	auto NAME = CTYPE<ETYPE>(internal_array_##NAME,                                                                           \
									internal_array_##NAME + sizeof(internal_array_##NAME)/sizeof(ETYPE));


#define UNUSED_TEST_VARIABLE(v) (v)

#ifdef RANGE_UNITTEST_OUTPUT

#include <ostream>
#include <sstream>

namespace RANGE_PROPOSAL_NAMESPACE {
	//-------------------------------------------------------------------------------------------------------------------------
	// print_range - debug helper
	namespace detail {
		template<typename OStream>
		struct print_elem {
			print_elem(OStream& os_, std::size_t max_elems) : os(os_), elems(max_elems) {} 

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
	std::string dbg_print_rng(Rng&& rng, std::size_t max_elems = 50) {

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

#else
#	define TEST_RANGE_EQUAL(EXPECT, IS) _ASSERT(tc::equal(EXPECT, IS))
#	define TEST_EQUAL(EXPECT, IS) _ASSERT(EXPECT == IS);
#	define TEST_OUTPUT(...)
#endif

#define TEST_RANGE_LENGTH(RNG, LENGTH) _ASSERT(tc::size(RNG) == LENGTH)
#define TEST_RANGE_NOT_EQUAL(EXPECT, IS) _ASSERT(!tc::equal(EXPECT, IS))
#define TEST_NOT_EQUAL(EXPECT, IS) _ASSERT(!(EXPECT == IS));

#define TEST_OUTPUT_RANGE(Rng) TEST_OUTPUT( << #Rng << " = " << dbg_print_rng(Rng) << std::endl)
#define STATIC_ASSERT(...) static_assert((__VA_ARGS__), #__VA_ARGS__ " is not true.")

