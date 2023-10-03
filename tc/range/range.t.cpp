
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../unittest.h"

#include "../array.h"
#include "../container/container.h" // tc::vector
#include "../algorithm/append.h"
#include "../algorithm/best_element.h"
#include "../algorithm/algorithm.h"

#include "adjacent_adaptor.h"
#include "cartesian_product_adaptor.h"
#include "concat_adaptor.h"
#include "filter_adaptor.h"
#include "join_adaptor.h"
#include "sparse_adaptor.h"


namespace {

static_assert( tc::is_range_of<TRAITFROMCONCEPT(tc::char_type), wchar_t const* const>::value );

//---- Basic ------------------------------------------------------------------------------------------------------------------
UNITTESTDEF( basic ) {
	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20});

	auto evenvr = tc::filter(v, [](int const v) noexcept { return v%2==0;});

	TEST_init_hack(tc::vector, int, vexp, {2, 4, 6, 8, 10, 12, 14, 16, 18, 20});
	TEST_RANGE_EQUAL(vexp, evenvr);
}

	template<typename Func>
	struct WrapVoidFunc final {
		static_assert(
			std::is_reference<Func>::value,
			"type must be a reference type"
		);

		WrapVoidFunc(Func func, tc::break_or_continue& breakorcontinue) noexcept :
			m_func(std::move(func)), m_breakorcontinue(breakorcontinue)
		{}

		template<typename Arg>
		void operator()(Arg&& arg) & noexcept {
			if (tc::continue_ == m_breakorcontinue) {
				if constexpr( std::is_same<decltype(std::declval<std::remove_reference_t<Func> >()(std::declval<Arg>())), tc::break_or_continue>::value ) {
					m_breakorcontinue = m_func(std::forward<Arg>(arg));
				} else {
					m_func(std::forward<Arg>(arg));
				}
			}
		}

		private:
			Func m_func;
			tc::break_or_continue& m_breakorcontinue;
	};

	template<typename Rng>
	struct void_range_struct final : std::remove_reference_t<Rng> {

		using base_ = std::remove_reference_t<Rng>;

		template< typename Func>
		tc::break_or_continue operator()(Func&& func) & noexcept {
			if constexpr( std::is_same<decltype(std::declval<base_>()(std::declval<Func>())), tc::break_or_continue>::value ) {
				return base_::operator()(std::forward<Func>(func));
			} else {
				tc::break_or_continue breakorcontinue = tc::continue_;
				base_::operator()(WrapVoidFunc<Func&&>(std::forward<Func>(func), breakorcontinue));
				return breakorcontinue;
			}
		}

		template< typename Func>
		tc::break_or_continue operator()(Func&& func) const& noexcept {
			if constexpr( std::is_same<decltype(std::declval<base_>()(std::declval<Func>())), tc::break_or_continue>::value ) {
				return base_::operator()(std::forward<Func>(func));
			} else {
				tc::break_or_continue breakorcontinue = tc::continue_;
				base_::operator()(WrapVoidFunc<Func&&>(std::forward<Func>(func), breakorcontinue));
				return breakorcontinue;
			}
		}
	};

	template<typename Rng>
	auto void_range(Rng&& rng) return_decltype_xvalue_by_ref_MAYTHROW(
		tc::derived_cast<void_range_struct<Rng&&>>(std::forward<Rng>(rng))
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
   TEST_init_hack(tc::vector, int, vexp, {0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48});

   TEST_RANGE_EQUAL(vexp, tc::filter( void_range(generator_range()), [](int const i) noexcept { return i%2==0; } ));
   TEST_RANGE_EQUAL(tc::filter( void_range(generator_range()), [](int const i) noexcept { return i%2==0; } ), vexp);
}

//---- Generator Range (with break) -------------------------------------------------------------------------------------------
namespace {
	struct generator_range_break final {
		template< typename Func >
		tc::break_or_continue operator()(Func func) {
			for(int i=0;i<5000;++i) {
				if (func(i)==tc::break_) { return tc::break_; }
			}
			return tc::continue_;
		}
	};
}

//---- N3752 filters examples  ------------------------------------------------------------------------------------------------
UNITTESTDEF( N3752 ) {
   TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20});

   auto r =  tc::filter( tc::filter( tc::filter(
                                v,
                                [](int const i) noexcept { return i%2!=0; } ),
                                [](int const i) noexcept { return i%3!=0; } ),
                                [](int const i) noexcept { return i%5!=0; } );

   TEST_init_hack(tc::vector, int, vexp, {1, 7, 11, 13, 17, 19});
   TEST_RANGE_EQUAL(vexp, r);

   auto ir = tc::make_iterator_range(std::begin(r), std::end(r));    // you shouldn't do this in real code! 
   TEST_RANGE_EQUAL(vexp, ir);

   auto bir = boost::make_iterator_range(tc::begin(r), tc::end(r));    // you shouldn't do this in real code! 
   TEST_RANGE_EQUAL(vexp, bir);
}

UNITTESTDEF( zero_termination ) {
	// only char is treated as zero-terminated character array.
	// signed/unsigned char is treated as a regular array
	{
		char const ach[]={ 0x20, 0 };
		_ASSERTEQUAL( tc::size(ach), 1 );
		char const* pch=ach;
		_ASSERTEQUAL( tc::size_linear(pch), 1 );
	}
	{
		signed char const ach[]={ 0x20, 0 };
		_ASSERTEQUAL( tc::size(ach), 2 );
		// signed char const* pch=ach;
		// _ASSERTEQUAL( tc::size(pch), 2 ); // correctly refuses to compile
	}
	{
		unsigned char const ach[]={ 0x20, 0 };
		_ASSERTEQUAL( tc::size(ach), 2 );
		// unsigned char const* pch=ach;
		// _ASSERTEQUAL( tc::size(pch), 2 ); // correctly refuses to compile
	}
}

UNITTESTDEF( ensure_index_range_on_chars ) {
	static_assert( tc::range_with_iterators<char*> );
	static_assert( tc::range_with_iterators<char const*> );
	static_assert( tc::range_with_iterators<char* &> );
	static_assert( tc::range_with_iterators<char* const&> );

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
	auto rng=tc::iota(0, 10);
	auto an=tc::explicit_cast<std::array<int, 10>>(rng);
	auto anCopy=an;
	tc::array<int&, 10> anRef(an);
	tc::for_each(rng, [&](int const n) {
		_ASSERTEQUAL(tc::at(an, n), n);
		_ASSERTEQUAL(tc::at(anCopy, n), n);
		_ASSERTEQUAL(tc::at(anRef, n), n);
	});
}

struct GeneratorInt {
	friend auto range_output_t_impl(GeneratorInt const&) -> tc::type::list<int>; // declaration only
	template<typename Func>
	tc::break_or_continue operator()(Func func) const& {
		tc_yield(func, 1);
		tc_yield(func, 6);
		tc_yield(func, 3);
		tc_yield(func, 4);
		return tc::continue_;
	}
};

struct GeneratorLong {
	friend auto range_output_t_impl(GeneratorLong const&) -> tc::type::list<long>; // declaration only
	template<typename Func>
	tc::break_or_continue operator()(Func func) const& {
		tc_yield(func, 1l);
		tc_yield(func, 6l);
		tc_yield(func, 3l);
		tc_yield(func, 4l);
		return tc::continue_;
	}

};

struct GeneratorGeneratorInt {
	friend auto range_output_t_impl(GeneratorGeneratorInt const&) -> tc::type::list<GeneratorInt>; // declaration only
	template<typename Func>
	tc::break_or_continue operator()(Func func) const& {
		tc_yield(func, GeneratorInt());
		tc_yield(func, GeneratorInt());
		return tc::continue_;
	}
};

struct GeneratorMutableInt {
	int m_an[3] = {1,2,3};

	friend auto range_output_t_impl(GeneratorMutableInt&) -> tc::type::list<int&>; // declaration only

	template<typename Func>
	tc::break_or_continue operator()(Func func) & {
		tc_yield(func, m_an[0]);
		tc_yield(func, m_an[1]);
		tc_yield(func, m_an[2]);
		return tc::continue_;
	}
};

} // end anonymous namespace

namespace {

struct dummy_pred {
	bool operator()(tc::unused) noexcept;
};

static_assert(
	!tc::has_range_value<decltype(
			tc::filter(
				std::declval<GeneratorMutableInt>(),
				dummy_pred()
			)
	)>::value
);

static_assert(
	!tc::has_range_value<std::add_const_t<decltype(
		tc::filter(
			std::declval<GeneratorMutableInt>(),
			dummy_pred()
		)
	)>>::value
);

static_assert(
	std::is_same<
		tc::range_value_t<
			decltype(
				tc::filter(
					std::declval<GeneratorMutableInt&>(),
					dummy_pred()
				)
			)
		>,
		int
	>::value
);

static_assert(
	std::is_same<
		tc::range_value_t<
			std::add_const_t<decltype(
				tc::filter(
					std::declval<GeneratorMutableInt&>(),
					dummy_pred()
				)
			)>
		>,
		int
	>::value
);

static_assert(
	!tc::has_range_value<
		decltype(
			tc::filter(
				std::declval<GeneratorMutableInt const&>(),
				dummy_pred()
			)
		)
	>::value
);

static_assert(
	!tc::has_range_value<
		std::add_const_t<decltype(
			tc::filter(
				std::declval<GeneratorMutableInt const&>(),
				dummy_pred()
			)
		)>
	>::value
);

static_assert(
	std::is_same<
		tc::range_value_t<
			std::add_const_t<decltype(
				tc::concat(
					std::declval<GeneratorMutableInt&>(),
					std::declval<GeneratorMutableInt&>()
				)
			)>
		>,
		int
	>::value
);

static_assert(
	std::is_same<
		tc::range_value_t<
			decltype(
				tc::concat(
					std::declval<GeneratorInt&>(),
					std::declval<GeneratorMutableInt&>()
				)
			)
		>,
		int
	>::value
);

UNITTESTDEF(filter_with_generator_range) {
	_ASSERTEQUAL(
		tc::max_element<tc::return_value>(
			GeneratorInt()
		),
		6
	);
	
	_ASSERTEQUAL(
		tc::max_element<tc::return_value>(
			tc::filter(
				GeneratorInt(),
				[](int const n) noexcept {return 1==n%2;}
			)
		),
		3
	);

	tc::for_each(
		GeneratorInt(),
		[](int&& n) noexcept {
			n += 1;
		}
	);

	_ASSERTEQUAL(
		tc::max_element<tc::return_value>(
			tc::transform(
				GeneratorInt(),
				[](int const n) noexcept {return -n;}
			)
		),
		-1
	);

	auto const tr1 = tc::transform(
		GeneratorInt(),
		[](int const n) noexcept {return -n;}
	);

	static_assert(
		std::is_same<
			tc::range_value_t<decltype(tr1)>,
			int
		>::value
	);

	auto const filtered = tc::filter(
		GeneratorInt(),
		[](int const n) noexcept {return n>0;}
	);
	static_assert(
		std::is_same<
			tc::range_value_t<decltype(filtered)>,
			int
		>::value
	);


	auto vecn = tc::make_vector(GeneratorInt());

	{
		auto vecn2 = tc::make_vector(tc::join(GeneratorGeneratorInt()));
		_ASSERTEQUAL(
			tc::size(vecn2),
			8
		);
	}

	auto vecgenint = tc::make_vector(GeneratorGeneratorInt());
	{
		auto vecn2 = tc::make_vector(tc::join(vecgenint));
		_ASSERTEQUAL(
			tc::size(vecn2),
			8
		);
	}

	static_assert(
		std::is_same<
			tc::range_value_t<
				decltype(tc::join(vecgenint))
			>,
			int
		>::value
	);
	auto vecnx = tc::make_vector(GeneratorInt(), GeneratorInt());
	_ASSERTEQUAL(tc::size(vecnx), 8);

	static_assert(
		std::is_same<
			tc::range_value_t<
				decltype(tc::concat(GeneratorInt(), GeneratorLong()))
			>,
			long
		>::value
	);

	static_assert(
		std::is_same<
			tc::range_value_t<
				decltype(tc::concat(GeneratorLong(), GeneratorLong()))
			>,
			long
		>::value
	);
}

UNITTESTDEF(make_constexpr_array_test) {
	constexpr auto const& an = tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1, 2, 3));
	static_assert(tc::at(an, 0) == 1);
	static_assert(tc::at(an, 1) == 2);
	static_assert(tc::at(an, 2) == 3);
	static_assert(tc::size(an) == 3);
	_ASSERT(tc::equal(an, tc::iota(1, 4)));
}

UNITTESTDEF(join_repeat) {
	_ASSERTEQUAL(tc::make_str(tc::join(tc::repeat_n(3, "ab"))), "ababab");
}

}

//---- Cartesian product ------------------------------------------------------------------------------------------------------

namespace {
	[[maybe_unused]] void cartesian_product_test(std::array<int, 2>& an) {
		tc::for_each(tc::cartesian_product(an, an), [](auto&& x){
			STATICASSERTSAME(decltype(x), (tc::tuple<int&, int&>&&));
		});
		tc::for_each(tc::cartesian_product(tc::as_const(an), tc::as_const(an)), [](auto&& x){
			STATICASSERTSAME(decltype(x), (tc::tuple<int const&, const int&>&&));
		});
		auto const rvaluerng = [](auto sink) { sink(1); };
		tc::for_each(tc::cartesian_product(rvaluerng, rvaluerng), [](auto&& x){
			STATICASSERTSAME(decltype(x), (tc::tuple<int const&&, int&&>&&));
		});

		static auto constexpr an1 = std::array<int, 5>{ 0, 1, 2, 3, 4 };
		static auto constexpr an2 = std::array<int, 3>{ 0, 1, 2 };
		static auto constexpr rng = tc::cartesian_product(an1, an2);

		GCC_WORKAROUND_STATIC_ASSERT( *++tc::begin(rng) == tc::make_tuple(0, 1) );
		GCC_WORKAROUND_STATIC_ASSERT( *--tc::end(rng) == tc::make_tuple(4, 2) );
		GCC_WORKAROUND_STATIC_ASSERT( tc::at(rng, 7) == tc::make_tuple(2, 1) );
		GCC_WORKAROUND_STATIC_ASSERT( int(tc::begin(rng) - tc::end(rng)) == -15 );
		GCC_WORKAROUND_STATIC_ASSERT( int(++tc::begin(rng) - --tc::end(rng)) == -13 );

		GCC_WORKAROUND_STATIC_ASSERT( tc::find_unique<tc::return_element_index>(rng, tc::make_tuple(2, 1)) == 7 );
		GCC_WORKAROUND_STATIC_ASSERT( tc::find_unique<tc::return_element>(rng, tc::make_tuple(2, 1)) - tc::begin(rng) == 7 );
	}
}

//---- Adjacent tuples ------------------------------------------------------------------------------------------------------

namespace {
	namespace adjacent_tuples_iterator_test {
		auto constexpr an = std::array<int, 5>{ 1, 2, 3, 4, 5 };
		namespace non_empty {
			auto constexpr rng = tc::adjacent<3>(an);

			static_assert( 3 == tc::size(rng) );
			static_assert( *tc::begin(rng) == tc::make_tuple(1, 2, 3) );
			auto constexpr it = []() { auto _ = tc::begin(rng); ++_; return _; }();
			static_assert( *it == tc::make_tuple(2,3,4) );
			auto constexpr it2 = []() { auto _ = tc::end(rng); --_; return _; }();
			static_assert( *it2 == tc::make_tuple(3,4,5) );
			static_assert( it + 1 == it2 );
			static_assert( *(tc::begin(rng) + 2) == tc::make_tuple(3, 4, 5) );
			auto constexpr it3 = []() { auto _ = tc::begin(rng); ++_; ++_; ++_; return _; }();
			static_assert( tc::begin(rng) + 3 == it3 );
			static_assert( tc::begin(rng) + 3 == tc::end(rng) );
			static_assert( tc::begin(rng) + 3 == tc::end_sentinel() );
			static_assert( tc::end(rng) == tc::end_sentinel() );
			static_assert( tc::end(rng) - tc::begin(rng) == 3 );
			static_assert( *tc::middle_point(tc::begin(rng), tc::end(rng)) == tc::make_tuple(2,3,4) );
		}
		namespace empty {
			auto constexpr rng = tc::adjacent<6>(an);
			static_assert( tc::empty(rng) );
			static_assert( tc::begin(rng) == tc::end(rng) );
			static_assert( tc::begin(rng) == tc::end_sentinel() );
			static_assert( tc::end(rng) == tc::end_sentinel() );
			static_assert( tc::begin(rng) + 0 == tc::begin(rng) );
			static_assert( tc::end(rng) - tc::begin(rng) == 0 );
		}
	}
}

STATICASSERTSAME((tc::range_output_t<tc::tuple<int, double, int, double const&>>), (tc::type::list<int, double, double const&>));
STATICASSERTSAME((tc::range_output_t<std::tuple<int, double, int, double const&>>), (tc::type::list<int, double, double const&>));
STATICASSERTSAME(tc::range_value_t<decltype(tc::concat_nonempty_with_separator("-", "1", "2"))>, char);
STATICASSERTSAME(tc::range_value_t<decltype(tc::concat_nonempty_with_separator("-", "1", tc::string<char>("2")))>, char);
STATICASSERTSAME(tc::range_value_t<decltype(tc::concat_nonempty_with_separator("-", "1", tc::empty_range()))>, char);

//---- Adjacent tuples ------------------------------------------------------------------------------------------------------

namespace {
	UNITTESTDEF(range_of_elements__dereference_index) {
		tc::discard(tc::make_vector(tc::transform(
			tc::make_range_of_iterators(tc::filter(tc::vector<void*>())),
			[&](auto&& it) noexcept {
				_ASSERT( std::is_same<decltype(it), decltype(tc::begin(tc::as_const(tc::as_lvalue(tc::filter(tc::vector<void*>()))))) &&>::value );
				return 7;
			}
		)));
	}
}
