
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "array.h" // tc::single
#include "container/container.h" // tc::vector

namespace tc {
	// create a generator range that gives the same values as the rng it takes (for testing)
	template<typename Rng> 
	constexpr auto make_generator_range(Rng&& rng) noexcept {
		return tc::generator_range_output<tc::mp_transform<std::add_rvalue_reference_t, tc::range_output_t<decltype(*tc::as_const(tc::as_lvalue(tc::make_reference_or_value(tc_move_if_owned(rng)))))>>>([rng=tc::make_reference_or_value(tc_move_if_owned(rng))](auto&& sink) noexcept {
			return tc::for_each(*rng, tc_move_if_owned(sink));
		});
	}
	
	//--------------------------------------------------------------------------------------------------------------------------
	// chunk_range
	// convert range to range of chunks

	namespace no_adl {
		template<typename RngChunk, typename Sink>
		struct chunk_range_sink {
			Sink& m_sink;

			auto chunk(RngChunk rng) const& return_decltype_MAYTHROW(m_sink(static_cast<RngChunk&&>(rng)))

			template<typename T>
			auto operator()(T&& t) const& return_decltype_MAYTHROW(chunk(tc::single(tc_move_if_owned(t))))
		};

		template<typename RngChunk, typename Rng>
		struct chunk_range_adaptor {
			tc::reference_or_value<Rng> m_baserng;
			friend auto range_output_t_impl(chunk_range_adaptor const&) -> boost::mp11::mp_list<RngChunk>; // declaration only

			template<typename Sink>
			auto operator()(Sink sink) const& noexcept {
				return tc::for_each(*m_baserng, chunk_range_sink<RngChunk, Sink&>{sink});
			}
		};
	}

	template<typename RngChunk, typename Rng>
	no_adl::chunk_range_adaptor<RngChunk, Rng> chunk_range(Rng&& rng) noexcept {
		return {tc::make_reference_or_value(tc_move_if_owned(rng))};
	}

}

//-----------------------------------------------------------------------------------------------------------------------------
// Unit test macros and output

#ifdef TC_PRIVATE

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
				if constexpr (requires (OStream& os, Elem& e) { os << e; }) {
					os << e;
				} else {
					os << "???";
				}
				os << ", ";
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
	auto dbg_print_rng(Rng&& rng, std::size_t const max_elems = 50) noexcept {
		std::stringstream os;
		os << "[";
		for_each(rng, std::ref(tc::as_lvalue(detail::print_elem<std::stringstream>(os, max_elems))) );
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


#if defined(__clang__) || defined(_MSC_VER)
	#define GCC_WORKAROUND_STATIC_ASSERT static_assert
#else // Fixed in GCC 13: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=92505
	#define GCC_WORKAROUND_STATIC_ASSERT _ASSERT
#endif
