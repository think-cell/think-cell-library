
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
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
	static_assert(std::is_same<tc::make_sub_range_result<tc::ptr_range<char const>&>::type, tc::ptr_range<char const>>::value);

	using SRVI = tc::make_sub_range_result<tc::vector<int>&>::type;
	using CSRVI = tc::make_sub_range_result<tc::vector<int> const&>::type;

	using SSRVI = tc::make_sub_range_result<tc::make_sub_range_result<tc::vector<int>&>::type>::type;
	using CSSRVI = tc::make_sub_range_result<tc::make_sub_range_result<tc::vector<int> const&>::type>::type;

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

	using SRVI = tc::make_sub_range_result<tc::vector<int>&>::type;
	using CSRVI = tc::make_sub_range_result<tc::vector<int> const&>::type;

	using SSRVI = tc::make_sub_range_result<tc::make_sub_range_result<tc::vector<int>&>::type>::type;
	using CSSRVI = tc::make_sub_range_result<tc::make_sub_range_result<tc::vector<int> const&>::type>::type;

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

		static_assert(std::is_same<decltype(ssrvi), decltype(srvi)>::value, "Sub-sub-range does not flatten to sub-range (decltype)");
		static_assert(std::is_same<decltype(cssrvi), decltype(csrvi)>::value, "const sub-sub-range does not flatten to const sub-range (decltype)");
	}

	UNITTESTDEF( sub_sub_range_lvalue ) {

		tc::vector<int> v;

		auto srvi = tc::slice(v);
		auto csrvi = tc::slice(tc::as_const(v));

		auto ssrvi = tc::slice(srvi);
		auto cssrvi = tc::slice(csrvi);

		// sanity checks
		static_assert(std::is_same<decltype(srvi), SRVI>::value, "make_sub_range_result gives wrong result");
		static_assert(std::is_same<decltype(csrvi), CSRVI>::value, "make_sub_range_result gives wrong result");
		static_assert(std::is_same<decltype(ssrvi), SSRVI>::value, "make_sub_range_result gives wrong result");
		static_assert(std::is_same<decltype(cssrvi), CSSRVI>::value, "make_sub_range_result gives wrong result");

		// the actual test
		static_assert(std::is_same<decltype(ssrvi), decltype(srvi)>::value, "Sub-sub-range does not flatten to sub-range");
		static_assert(std::is_same<decltype(cssrvi), decltype(csrvi)>::value, "const sub-sub-range does not flatten to const sub-range");
	}

	UNITTESTDEF( sub_sub_range_index ) {

		TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});
		TEST_init_hack(tc::vector, int, exp36, {4,5,6});

		auto sr = tc::slice(v);
		auto csr = tc::slice(tc::as_const(v));

		// use range_difference to specify bounds
		auto ssr1 = tc::slice_by_interval(sr, tc::make_interval(3, 6));
		auto cssr1 = tc::slice_by_interval(csr, tc::make_interval(3, 6));

		static_assert(std::is_same<decltype(ssr1), decltype(sr)>::value, "Sub-sub-range does not flatten to sub-range");
		static_assert(std::is_same<decltype(cssr1), decltype(csr)>::value, "const sub-sub-range does not flatten to const sub-range");

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
			3,
			*tc::upper_bound<tc::return_border>(
				tc::union_range(
					MAKE_CONSTEXPR_ARRAY(1,2,4),
					MAKE_CONSTEXPR_ARRAY(2,3)
				),
				2
			)
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
			tc::single( 2 ),
			tc::untransform(tc::equal_range(tc::transform(vecn, [](int n) noexcept {return n*n; }), 4))
		);

		{
			auto rng = tc::transform(tc::filter(vecn, [](int n) noexcept {return n<3;}), [](int n) noexcept {return n*n; });

			TEST_RANGE_EQUAL(
				tc::single( 2 ),
				tc::untransform(tc::equal_range(rng, 4))
			);
		}

		{
			// r-value transform with l-value range
			auto&& rng = tc::untransform(tc::transform(vecn, [](int n) noexcept {return n*n;}));
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*tc::begin(rng)) == std::addressof(*tc::begin(vecn)));
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
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*tc::begin(rng)) == std::addressof(*tc::begin(vecn)));
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
			static_assert(std::is_same<decltype(rng),tc::vector<int>>::value);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}

		{
			// l-value transform of l-value-range
			auto trnsfrng = tc::transform(vecn, [](int n) noexcept {return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*tc::begin(rng)) == std::addressof(*tc::begin(vecn)));
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
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*tc::begin(rng)) == std::addressof(*tc::begin(vecn)));
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
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*tc::begin(rng)) == std::addressof(*tc::begin(vecn)));
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
				_ASSERT(tc::end(rngExpected) == it);
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
				_ASSERT(tc::end(rngExpected) == it);
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
				_ASSERT(tc::end(rngExpected) == it);
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
			_ASSERT(tc::end(rngExpected) == it);
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
			_ASSERT(tc::end(rngExpected) == it);
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
			tc::single(11)
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
		_ASSERT(tc::begin(rngunique) == it);

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
		_ASSERTEQUAL(tc::hash_range(strNarrow), tc::hash_range(tc::as_pointers(strNarrow)));
		_ASSERTEQUAL(tc::hash_range(strNarrow), tc::hash_range("test"));
		std::basic_string<tc::char16> strWide = UTF16("test");
		_ASSERTEQUAL(tc::hash_range(strWide), tc::hash_range(tc::as_pointers(strWide)));
		_ASSERTEQUAL(tc::hash_range(strWide), tc::hash_range(UTF16("test")));
		_ASSERTEQUAL(tc::hash_range(tc::make_vector(strWide)), tc::hash_range(tc::as_pointers(strWide)));
		_ASSERTEQUAL(tc::hash_range(strNarrow), tc::hash(strNarrow));
		_ASSERTEQUAL(tc::hash_range(strWide), tc::hash(strWide));
	}
#endif
}

