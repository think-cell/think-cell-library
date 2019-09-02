
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.h"
#include "container.h" // tc::vector
#include "sub_range.h"
#include "union_adaptor.h"
#include "range.t.h"
#include "unique_range_adaptor.h"
#include "intersection_adaptor.h"
#include "interval.h"

#ifdef TC_PRIVATE
#include "Library/Utilities/Hash.h"
#endif

#include <boost/implicit_cast.hpp>

#include <type_traits>

namespace {
	STATICASSERTSAME(tc::make_sub_range_result_t<tc::ptr_range<char const>&>, tc::ptr_range<char const>);

	using SRVI = tc::make_sub_range_result_t<tc::vector<int>&>;
	using CSRVI = tc::make_sub_range_result_t<tc::vector<int> const&>;

	using SSRVI = tc::make_sub_range_result_t<tc::make_sub_range_result_t<tc::vector<int>&>>;
	using CSSRVI = tc::make_sub_range_result_t<tc::make_sub_range_result_t<tc::vector<int> const&>>;

	UNITTESTDEF( sub_range_array ) {

		int arr[4] = {1,2,3,4};
		auto arr_rng = tc::slice_by_interval(arr, tc::make_interval(1, 3));

		boost::implicit_cast<tc::sub_range<tc::iterator_base<int *>>>(arr_rng);
		boost::implicit_cast<tc::sub_range<tc::iterator_base<int const*>>>(arr_rng);
	}


	UNITTESTDEF( const_sub_range_char_ptr ) {
		tc::sub_range<tc::iterator_base<char const*>>{"test"};
	}

	//-------------------------------------------------------------------------------------------------------------------------

	using SRVI = tc::make_sub_range_result_t<tc::vector<int>&>;
	using CSRVI = tc::make_sub_range_result_t<tc::vector<int> const&>;

	using SSRVI = tc::make_sub_range_result_t<tc::make_sub_range_result_t<tc::vector<int>&>>;
	using CSSRVI = tc::make_sub_range_result_t<tc::make_sub_range_result_t<tc::vector<int> const&>>;

	[[maybe_unused]] void ref_test(SRVI & rng) noexcept {
		CSRVI const_rng(rng);
		SRVI non_const_rng(rng);
	}

	UNITTESTDEF( const_sub_range ) {
		tc::vector<int> v;
		auto srvi = tc::slice(v);
		auto csrvi = tc::slice(tc::as_const(v));

		(void) srvi;
		(void) csrvi;

		SRVI non_const_rng(srvi);

		CSRVI const_rng(srvi);
	}

	UNITTESTDEF( sub_sub_range_rvalue ) {

		tc::vector<int> v;

		auto srvi = tc::slice(v);
		auto csrvi = tc::slice(tc::as_const(v));

		auto ssrvi = tc::slice(tc::slice(v));
		auto cssrvi = tc::slice(tc::slice(tc::as_const(v)));

		STATICASSERTSAME(decltype(ssrvi), decltype(srvi), "Sub-sub-range does not flatten to sub-range (decltype)");
		STATICASSERTSAME(decltype(cssrvi), decltype(csrvi), "const sub-sub-range does not flatten to const sub-range (decltype)");
	}

	UNITTESTDEF( sub_sub_range_lvalue ) {

		tc::vector<int> v;

		auto srvi = tc::slice(v);
		auto csrvi = tc::slice(tc::as_const(v));

		auto ssrvi = tc::slice(srvi);
		auto cssrvi = tc::slice(csrvi);

		// sanity checks
		STATICASSERTSAME(decltype(srvi), SRVI, "make_sub_range_result gives wrong result");
		STATICASSERTSAME(decltype(csrvi), CSRVI, "make_sub_range_result gives wrong result");
		STATICASSERTSAME(decltype(ssrvi), SSRVI, "make_sub_range_result gives wrong result");
		STATICASSERTSAME(decltype(cssrvi), CSSRVI, "make_sub_range_result gives wrong result");

		// the actual test
		STATICASSERTSAME(decltype(ssrvi), decltype(srvi), "Sub-sub-range does not flatten to sub-range");
		STATICASSERTSAME(decltype(cssrvi), decltype(csrvi), "const sub-sub-range does not flatten to const sub-range");
	}

	UNITTESTDEF( sub_sub_range_index ) {

		TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});
		TEST_init_hack(tc::vector, int, exp36, {4,5,6});

		auto sr = tc::slice(v);
		auto csr = tc::slice(tc::as_const(v));

		// use range_difference to specify bounds
		auto ssr1 = tc::slice_by_interval(sr, tc::make_interval(3, 6));
		auto cssr1 = tc::slice_by_interval(csr, tc::make_interval(3, 6));

		STATICASSERTSAME(decltype(ssr1), decltype(sr), "Sub-sub-range does not flatten to sub-range");
		STATICASSERTSAME(decltype(cssr1), decltype(csr), "const sub-sub-range does not flatten to const sub-range");

	}

	UNITTESTDEF( sub_sub_range_crazy_manual ) {

		tc::vector<int> v;

		// don't try that at work! - Seriously: sub_range is build to work correctly in a variety of situations,
		// but normally it should not be necessary to manually utter the type. Use slice and make_sub_range_result instead.
		// if you do need to say the type (e.g. when deducting sth.)  ...
		//_ASSERT(false);
		tc::sub_range<tc::vector<int>&>{v}; // create a sub range from unrelated range
		static_cast<void>(tc::sub_range<tc::vector<int>&>(tc::sub_range<tc::vector<int>&>(v))); // copy sub range
	}

	UNITTESTDEF(union_range_tests) {
		TEST_RANGE_EQUAL(
			MAKE_CONSTEXPR_ARRAY(1,2,3,4),
			tc::union_range(
				MAKE_CONSTEXPR_ARRAY(1,4),
				MAKE_CONSTEXPR_ARRAY(2,3)
			)
		);

		TEST_RANGE_EQUAL(
			MAKE_CONSTEXPR_ARRAY(4,3,2,1,0),
			tc::union_range(
				MAKE_CONSTEXPR_ARRAY(4,2,0),
				MAKE_CONSTEXPR_ARRAY(3,2,1),
				[](int lhs, int rhs) noexcept {return tc::compare(rhs,lhs);}
			)
		);

		_ASSERTEQUAL(
			*tc::upper_bound<tc::return_border>(
				tc::union_range(
					MAKE_CONSTEXPR_ARRAY(1,2,4),
					MAKE_CONSTEXPR_ARRAY(2,3)
				),
				2
			),
			3
		);

		{
			auto rng = tc::union_range(
				MAKE_CONSTEXPR_ARRAY(1,2,3,3,4,4,5),
				MAKE_CONSTEXPR_ARRAY(1,1,1,3,4,4,4)
			);

			TEST_RANGE_EQUAL(
				rng,
				MAKE_CONSTEXPR_ARRAY(1,1,1,2,3,3,4,4,4,5)
			);

			{
				auto it = tc::lower_bound<tc::return_border>(rng,3);
				_ASSERTEQUAL(*it,3);
				_ASSERTEQUAL(*boost::prior(it), 2);
				_ASSERTEQUAL(*boost::next(it), 3);
	}
			{
				auto it = tc::upper_bound<tc::return_border>(rng,1);
				_ASSERTEQUAL(*it,2);
				_ASSERTEQUAL(*--it, 1);
				_ASSERTEQUAL(*--it, 1);
				_ASSERTEQUAL(*--it, 1);
			}
		}
		{
			auto rng = tc::union_range(
				MAKE_CONSTEXPR_ARRAY(1,1,1,1,1,1,1,1,1),
				MAKE_CONSTEXPR_ARRAY(1,1)
			);
			auto it = tc::lower_bound<tc::return_border>(rng,1);
			_ASSERTEQUAL(*it,1);
		}
	}

#ifdef _CHECKS
	struct GenRange final {
		using reference=int;
		using const_reference=int;
		template<typename Func>
		auto operator()(Func func) const& noexcept -> tc::break_or_continue {
			RETURN_IF_BREAK(func(1));
			RETURN_IF_BREAK(func(3));
			RETURN_IF_BREAK(func(5));
			return tc::continue_;
		}
	};

	struct GenRangeMutable final {
		using reference=double;
		using const_reference=double;
		tc::vector<double> m_vecf = tc::vector<double>({1,3,5});

		template<typename Func>
		auto operator()(Func func) & noexcept -> tc::break_or_continue {
			return tc::for_each(m_vecf, std::ref(func));
		}

		template<typename Func>
		auto operator()(Func func) const& noexcept -> tc::break_or_continue {
			return tc::for_each(m_vecf, std::ref(func));
		}
	};


#endif

	UNITTESTDEF(union_range_generator) {
		TEST_RANGE_EQUAL(
			MAKE_CONSTEXPR_ARRAY(1,2,3,5,6),
			tc::union_range(
				GenRange(),
				MAKE_CONSTEXPR_ARRAY(2,6)
			)
		);

		GenRangeMutable rngMut;
		tc::vector<double> vecf;
		tc::for_each(
			tc::union_range(
				rngMut,
				vecf
			),
			[](double& d) noexcept {d += 0.1;}
		);

		TEST_RANGE_EQUAL(
			MAKE_CONSTEXPR_ARRAY(1.1,3.1,5.1),
			rngMut
		);



	}

	UNITTESTDEF(sub_range_with_tranform) {

		tc::vector<int> vecn{1,2,3};
		auto rgntrnsfn = tc::transform(vecn, [](int n) noexcept {return n*n;});

		TEST_RANGE_EQUAL(
			tc::slice(rgntrnsfn, tc::begin(rgntrnsfn), tc::end(rgntrnsfn)),
			MAKE_CONSTEXPR_ARRAY(1,4,9)
		);

		TEST_RANGE_EQUAL(
			MAKE_CONSTEXPR_ARRAY( 1, 2, 3 ),
			tc::untransform(tc::slice(rgntrnsfn, tc::begin(rgntrnsfn), tc::end(rgntrnsfn)))
		);

		TEST_RANGE_EQUAL(
			MAKE_CONSTEXPR_ARRAY( 2 ),
			tc::untransform(tc::equal_range(tc::transform(vecn, [](int n) noexcept {return n*n; }), 4))
		);

		{
			auto rng = tc::transform(tc::filter(vecn, [](int n) noexcept {return n<3;}), [](int n) noexcept {return n*n; });

			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY( 2 ),
				tc::untransform(tc::equal_range(rng, 4))
			);
		}

		{
			// r-value transform with l-value range
			auto&& rng = tc::untransform(tc::transform(vecn, [](int n) noexcept {return n*n;}));
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			static_assert(!std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}

		{
			// r-value transform with l-value const range
			auto&& rng = tc::untransform(tc::transform(tc::as_const(vecn), [](int n) noexcept {return n*n;}));
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype((rng))>::value);
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}

		{
			// r-value transform with r-value range
			auto rng = tc::untransform(tc::transform(tc::vector<int>{1, 2, 3}, [](int n) noexcept {return n*n;}));
			STATICASSERTSAME(decltype(rng),tc::vector<int>);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}

		{
			// l-value transform of l-value-range
			auto trnsfrng = tc::transform(vecn, [](int n) noexcept {return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			static_assert(!std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}

		{
			// l-value transform of const l-value-range
			auto const trnsfrng = tc::transform(tc::as_const(vecn), [](int n) noexcept {return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}

		{
			// l-value transform of const l-value-range
			auto trnsfrng = tc::transform(tc::as_const(vecn), [](int n) noexcept {return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}

		{
			// l-value transform of r-value range
			auto trnsfrng = tc::transform(tc::vector<int>{1, 2, 3}, [](int n) noexcept {return n*n; });
			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(
				std::addressof(*tc::begin(trnsfrng).element_base()),
				std::addressof(*tc::begin(rng))
			);
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			static_assert(!std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}
		{
			// const l-value transform of r-value range
			auto const trnsfrng = tc::transform(tc::vector<int>{1, 2, 3}, [](int n) noexcept {return n*n; });
			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(
				std::addressof(*tc::begin(trnsfrng).element_base()),
				std::addressof(*tc::begin(rng))
			);
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}
	}

	UNITTESTDEF(Unique_Ranges) {
		tc::vector<int> vecn{1,2,3,3,5,5,7};

		{
			std::initializer_list<int> const rngExpected[] = { // extends life-time of underlying arrays
				{1},
				{2},
				{3,3},
				{5,5},
				{7}
			};

			{
				auto it = tc::begin(rngExpected);
				tc::for_each(
					tc::adjacent_unique_range(vecn),
					[&](auto const& subrng) noexcept {
						TEST_RANGE_EQUAL(subrng, *it);
						++it;
					}
				);
				_ASSERTEQUAL(tc::end(rngExpected), it);
			}

			{
				auto it = tc::begin(rngExpected);
				tc::for_each(
					tc::adjacent_unique_range(tc::as_const(vecn)),
					[&](auto const& subrng) noexcept {
						TEST_RANGE_EQUAL(subrng, *it);
						++it;
					}
				);
				_ASSERTEQUAL(tc::end(rngExpected), it);
			}

			{
				auto it = tc::begin(rngExpected);
				tc::for_each(
				tc::adjacent_unique_range(vecn),
					[&](auto const& subrng) noexcept {
						TEST_RANGE_EQUAL(subrng, *it);
						++it;
					}
				);
				_ASSERTEQUAL(tc::end(rngExpected), it);
			}

		}

		auto Pred = [](int lhs, int rhs) noexcept {
			return std::abs(lhs-rhs) <=1;
		};

		{
			std::initializer_list<int> const rngExpected[] = { // extends life-time of underlying arrays
				{1,2},
				{3,3},
				{5,5},
				{7}
			};
			auto it = tc::begin(rngExpected);
			tc::for_each(
				tc::front_unique_range(vecn, Pred),
				[&](auto const& subrng) noexcept {
					TEST_RANGE_EQUAL(subrng, *it);
					++it;
				}
			);
			_ASSERTEQUAL(tc::end(rngExpected), it);
		}

		{
			std::initializer_list<int> const rngExpected[] = { // extends life-time of underlying arrays
				{1,2,3,3},
				{5,5},
				{7}
			};
			auto it = tc::begin(rngExpected);
			tc::for_each(
				tc::adjacent_unique_range(vecn, Pred),
				[&](auto const& subrng) noexcept {
					TEST_RANGE_EQUAL(subrng, *it);
					++it;
				}
			);
			_ASSERTEQUAL(tc::end(rngExpected), it);
		}
	}

	UNITTESTDEF(difference_range) {
		TEST_RANGE_EQUAL(
			tc::difference(
				MAKE_CONSTEXPR_ARRAY(1,2,2,4,7,8,8,11,11),
				MAKE_CONSTEXPR_ARRAY(2,3,4,8,8)
			),
			MAKE_CONSTEXPR_ARRAY(1,2,7,11,11)
		);
	}

	UNITTESTDEF(difference_unordered_set) {
		tc::unordered_set<int> setn1;
		tc::cont_assign(setn1, MAKE_CONSTEXPR_ARRAY(1,2,3,4,5,6,7,8,9,10,11));

		tc::unordered_set<int> setn2;
		tc::cont_assign(setn2, MAKE_CONSTEXPR_ARRAY(10,9,8,7,6,5,4,3,2,1));

		TEST_RANGE_EQUAL(
			tc::set_difference(setn1, setn2),
			MAKE_CONSTEXPR_ARRAY(11)
		);

		tc::vector<int> vecn;
		tc::for_each(tc::set_intersect(setn1, setn2), [&](int n) noexcept {
			vecn.emplace_back(n);
		});

		TEST_RANGE_EQUAL(
			tc::sort(vecn),
			MAKE_CONSTEXPR_ARRAY(1,2,3,4,5,6,7,8,9,10)
		);
	}

	UNITTESTDEF(tc_unique) {
		TEST_RANGE_EQUAL(
			tc::adjacent_unique(MAKE_CONSTEXPR_ARRAY(1,2,2,4,7,8,8,11,11)),
			MAKE_CONSTEXPR_ARRAY(1,2,4,7,8,11)
		);

		tc::vector<int> vecn{1,1,2,2};
		auto rngunique = tc::adjacent_unique(vecn);
		auto it = tc::begin(rngunique);
		auto itvec = it.element_base();
		_ASSERT(1==*itvec);
		_ASSERT(1 == *(++itvec));
		_ASSERT(2 == *(++itvec));

		_ASSERT(1 == *it);
		_ASSERT(2 == *(++it));
		_ASSERT(1 == *(--it));
		_ASSERTEQUAL(tc::begin(rngunique), it);

	}

#ifdef _CHECKS
	struct MovingInt final {
		int m_n;

		MovingInt(int n) noexcept : m_n(n)
		{}

		MovingInt(MovingInt&& other) noexcept : m_n(other.m_n)
		{
			other.m_n = 0;
		}

		MovingInt(MovingInt const&) = delete;
		MovingInt& operator=(MovingInt& other) = delete;

		MovingInt& operator=(MovingInt&& other) & noexcept {
			m_n = other.m_n;
			if (&other.m_n != &m_n) {
				other.m_n = 0;
			}
			return *this;
		}

		operator int() const& noexcept {return m_n;}
	};
#endif

	UNITTESTDEF(tc_unique_inplace) {
		tc::vector<MovingInt> vecmn; // list initializtion not possible with move-only element type
		tc::cont_emplace_back(vecmn, 1);
		tc::cont_emplace_back(vecmn, 1);
		tc::cont_emplace_back(vecmn, 1);
		tc::cont_emplace_back(vecmn, 2);
		tc::cont_emplace_back(vecmn, 2);
		tc::cont_emplace_back(vecmn, 2);
		tc::cont_emplace_back(vecmn, 3);
		tc::cont_emplace_back(vecmn, 4);
		tc::cont_emplace_back(vecmn, 4);
		tc::adjacent_unique_inplace(vecmn);

		TEST_RANGE_EQUAL(
			vecmn,
			MAKE_CONSTEXPR_ARRAY(1,2,3,4)
		);
	}

#ifdef TC_PRIVATE
	UNITTESTDEF(hash_string_range) {
		std::string strNarrow = "test";
		_ASSERTEQUAL(tc::hash_range<std::size_t>(strNarrow), tc::hash_range<std::size_t>(tc::as_pointers(strNarrow)));
		_ASSERTEQUAL(tc::hash_range<std::size_t>(strNarrow), tc::hash_range<std::size_t>("test"));
		std::basic_string<tc::char16> strWide = UTF16("test");
		_ASSERTEQUAL(tc::hash_range<std::size_t>(strWide), tc::hash_range<std::size_t>(tc::as_pointers(strWide)));
		_ASSERTEQUAL(tc::hash_range<std::size_t>(strWide), tc::hash_range<std::size_t>(UTF16("test")));
		_ASSERTEQUAL(tc::hash_range<std::size_t>(tc::make_vector(strWide)), tc::hash_range<std::size_t>(tc::as_pointers(strWide)));
		_ASSERTEQUAL(tc::hash_range<std::size_t>(strNarrow), tc::hash<std::size_t>(strNarrow));
		_ASSERTEQUAL(tc::hash_range<std::size_t>(strWide), tc::hash<std::size_t>(strWide));
	}
#endif

	UNITTESTDEF( take_first_sink ) {
		auto const rngn = [](auto sink) noexcept {
			for(int i=0;;++i) { RETURN_IF_BREAK(tc::continue_if_not_break(sink, i)); }
		};

		_ASSERTEQUAL(tc::for_each(tc::take_first(rngn), [](int) noexcept {}), tc::continue_);
		_ASSERTEQUAL(tc::for_each(tc::take_first(rngn), [](int) noexcept { return tc::break_; }), tc::break_);
		_ASSERT(tc::equal(MAKE_CONSTEXPR_ARRAY(0,1,2,3), tc::take_first(rngn, 4)));

		auto rngch = tc::concat("123", [](auto&& sink) noexcept { return tc::for_each("456", std::forward<decltype(sink)>(sink)); });

		struct assert_no_single_char_sink /*final*/ {
			auto operator()(char) const& noexcept { _ASSERTFALSE; return tc::construct_default_or_terminate<tc::break_or_continue>(); }
			auto chunk(tc::ptr_range<char const> str) const& noexcept { return INTEGRAL_CONSTANT(tc::continue_)(); }
		};
		_ASSERTEQUAL(tc::for_each(tc::take_first(rngch, 4), assert_no_single_char_sink()), tc::continue_);
		_ASSERT(tc::equal("1234", tc::take_first(rngch, 4)));
	}
}

