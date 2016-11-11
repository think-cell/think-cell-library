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

#include "range.h"
#include "container.h" // tc::vector
#include "range.t.h"

#include "sparse_adaptor.h"

namespace {

//---- Basic ------------------------------------------------------------------------------------------------------------------
UNITTESTDEF( basic ) {
	using namespace tc;

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20});

	auto evenvr = tc::filter(v, [](int const& v) noexcept { return (v%2==0);});

	TEST_init_hack(tc::vector, int, vexp, {2, 4, 6, 8, 10, 12, 14, 16, 18, 20});
	TEST_RANGE_EQUAL(vexp, evenvr);
}

	using namespace tc;

	template<typename Func>
	struct WrapVoidFunc final {
		static_assert(
			std::is_reference<Func>::value,
			"type must be a reference type"
		);

		WrapVoidFunc(Func func, break_or_continue& breakorcontinue) noexcept :
			m_func(std::move(func)), m_breakorcontinue(breakorcontinue)
		{}

		// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
		template<typename Arg>
		std::enable_if_t<
			std::is_same<
				decltype(std::declval<std::remove_reference_t<Func> >()(std::declval<Arg>())),
				break_or_continue
			>::value
		>
		operator()(Arg&& arg) & noexcept {
			if (continue_ == m_breakorcontinue) {
				m_breakorcontinue = m_func(std::forward<Arg>(arg));
			}
		}

		// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
		template<typename Arg>
		typename std::enable_if<
			!std::is_same<
				decltype(std::declval<std::remove_reference_t<Func> >()(std::declval<Arg>())),
				break_or_continue
			>::value
		>::type
		operator()(Arg&& arg) & noexcept {
			if (continue_ == m_breakorcontinue) {
				m_func(std::forward<Arg>(arg));
			}
		}

		private:
			Func m_func;
			break_or_continue& m_breakorcontinue;
	};

	template<typename Rng>
	struct void_range_struct final : public std::remove_reference<Rng>::type {

		using base_ = std::remove_reference_t<Rng>;

		// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
		template< typename Func >
		std::enable_if_t<
			!std::is_same<
				decltype(std::declval<base_>()(std::declval<Func>())),
				break_or_continue
			>::value,
			break_or_continue
		>
		operator()(Func&& func) & noexcept {
			break_or_continue breakorcontinue = continue_;
			base_::operator()(WrapVoidFunc<Func&&>(std::forward<Func>(func), breakorcontinue));
			return breakorcontinue;
		}

		// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
		template< typename Func >
		std::enable_if_t<
			!std::is_same<
				decltype(std::declval<base_>()(std::declval<Func>())),
				break_or_continue
			>::value,
			break_or_continue
		>
		operator()(Func&& func) const& noexcept {
			break_or_continue breakorcontinue = continue_;
			base_::operator()(WrapVoidFunc<Func&&>(std::forward<Func>(func), breakorcontinue));
			return breakorcontinue;
		}

		// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
		template< typename Func >
		std::enable_if_t<
			std::is_same<
				decltype(std::declval<base_>()(std::declval<Func>())),
				break_or_continue
			>::value,
			break_or_continue
		>
		operator()(Func&& func) & noexcept {
			return base_::operator()(std::forward<Func>(func));
		}

		// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
		template< typename Func >
		std::enable_if_t<
			std::is_same<
				decltype(std::declval<base_>()(std::declval<Func>())),
				break_or_continue
			>::value,
			break_or_continue
		>
		operator()(Func&& func) const& noexcept {
			return base_::operator()(std::forward<Func>(func));
		}
	};

	template<typename Rng>
	auto void_range(Rng&& rng) return_decltype_rvalue_by_ref (
		derived_or_base_cast<void_range_struct<Rng&&>>(std::forward<Rng>(rng))
	)


//---- Generator Range --------------------------------------------------------------------------------------------------------
namespace {
	struct generator_range {
		template< typename Func >
		void operator()(Func func) const& noexcept {
			for(int i=0;i<50;++i) {
				func(i);
			}
		}
	};
}

UNITTESTDEF( generator_range ) {
   using namespace tc;
   
   TEST_init_hack(tc::vector, int, vexp, {0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48});

   TEST_RANGE_EQUAL(vexp, tc::filter( void_range(generator_range()), [](int i) noexcept { return i%2==0; } ));
   TEST_RANGE_EQUAL(tc::filter( void_range(generator_range()), [](int i) noexcept { return i%2==0; } ), vexp);
}

//---- Generator Range (with break) -------------------------------------------------------------------------------------------
namespace {
	struct generator_range_break final {
		template< typename Func >
		tc::break_or_continue operator()(Func func) {
			using namespace tc;
			for(int i=0;i<5000;++i) {
				if (func(i)==break_) { return break_; }
			}
			return continue_;
		}
	};
}

// TODO, we need something like a tc::starts_with() and TC_RANGE_STARTS_WITH for this to make sense.
//UNITTESTDEF( generator_range_break ) {
//   using namespace tc;
//
//   TEST_init_hack(tc::vector, int, vexp, {0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50});
//   TEST_RANGE_EQUAL(vexp, tc::filter( generator_range_break(), [](int i) noexcept { return i%2==0; } ));
//}

//---- N3752 filters examples  ------------------------------------------------------------------------------------------------
UNITTESTDEF( N3752 ) {
   using namespace tc;

   TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20});

   auto r =  tc::filter( tc::filter( tc::filter(
                                v,
                                [](int i) noexcept { return i%2!=0; } ),
                                [](int i) noexcept { return i%3!=0; } ),
                                [](int i) noexcept { return i%5!=0; } );

   TEST_init_hack(tc::vector, int, vexp, {1, 7, 11, 13, 17, 19});
   TEST_RANGE_EQUAL(vexp, r);

   auto ir = tc::make_iterator_range(std::begin(r), std::end(r));    // you shouldn't do this in real code! 
   TEST_RANGE_EQUAL(vexp, ir);

   auto bir = boost::make_iterator_range(boost::begin(r), boost::end(r));    // you shouldn't do this in real code! 
   TEST_RANGE_EQUAL(vexp, bir);
}

//---- Stacked filters --------------------------------------------------------------------------------------------------------
// TODO, we need something like a tc::starts_with() and TC_RANGE_STARTS_WITH for this to make sense.
//UNITTESTDEF( stacked_filters) {
//   using namespace tc;
//
//   TEST_init_hack(tc::vector, int, vexp, {1, 7, 11, 13, 17, 19});
//   TEST_RANGE_EQUAL(vexp, tc::filter( tc::filter( tc::filter(
//                               generator_range_break(),
//                               [](int i) noexcept { return i%2!=0; } ),
//                               [](int i) noexcept { return i%3!=0; } ),
//                               [](int i) noexcept { return i%5!=0; } ));
//}

UNITTESTDEF( zero_termination ) {
	// only char is treated as zero-terminated character array.
	// signed/unsigned char is treated as a regular array
	{
		char const ach[]={ 0x20, 0 };
		// _ASSERTEQUAL( tc::size(ach), 1 ); // does not compile
		char const* pch=ach;
		_ASSERTEQUAL( tc::size(pch), 1 );
	}
	{
		signed char const ach[]={ 0x20, 0 };
		_ASSERTEQUAL( tc::size(ach), 2 );
		// signed char const* pch=ach;
		// _ASSERTEQUAL( tc::size(pch), 2 ); // does not compile
	}
	{
		unsigned char const ach[]={ 0x20, 0 };
		_ASSERTEQUAL( tc::size(ach), 2 );
		// unsigned char const* pch=ach;
		// _ASSERTEQUAL( tc::size(pch), 2 ); // does not compile
	}
}

UNITTESTDEF( ensure_index_range_on_chars ) {
	static_assert( tc::is_range_with_iterators<char*>::value, "" );
	static_assert( tc::is_range_with_iterators<char const*>::value, "" );
	static_assert( tc::is_range_with_iterators<char* &>::value, "" );
	static_assert( tc::is_range_with_iterators<char* const&>::value, "" );

	struct check_5_chars final {
		void operator()( char ) {
			++m_cch;
		}
		~check_5_chars() {
			_ASSERTEQUAL(m_cch,5u);
		}
	private:
		std::size_t m_cch = 0;
	};

	{
		char ach[] = "Hello";
		char * str=ach;
		{
			check_5_chars chk;
			tc::for_each(str, std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::for_each(tc::as_const(str), std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::for_each(tc_move(str), std::ref(chk));
		}
	}
	{
		char str[]="Hello";
		{
			check_5_chars chk;
			tc::for_each(str, std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::for_each(tc::as_const(str), std::ref(chk));
		}
	}
	{
		char const* str="Hello";
		{
			check_5_chars chk;
			tc::for_each(str, std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::for_each(tc::as_const(str), std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::for_each(tc_move(str), std::ref(chk));
		}
	}
	{
		char const str[]="Hello";
		{
			check_5_chars chk;
			tc::for_each(str, std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::for_each(tc::as_const(str), std::ref(chk));
		}
	}
}

UNITTESTDEF( construct_array_from_range ) {
	auto rng=make_counting_range(0, 10);
	tc::array<int, 10> an=rng;
	tc::array<int, 10> anCopy=an;
	tc::array<std::vector<int>, 10> avecn(an);
	tc::array<std::vector<int>&, 10> avecnRef=avecn;
	tc::array<std::vector<int>, 10> avecnMoved=tc_move_always(avecn);
	tc::for_each(rng, [&](int n) {
		_ASSERTEQUAL(an[n], n);
		_ASSERTEQUAL(anCopy[n], n);
		_ASSERTEQUAL(tc::size(avecn[n]), 0);
		_ASSERTEQUAL(tc::size(avecnRef[n]), 0);
		_ASSERTEQUAL(tc::size(avecnMoved[n]), n);
	});
}

}