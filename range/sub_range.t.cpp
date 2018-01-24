//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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
#include "sub_range.h"
#include "union_adaptor.h"
#include "range.t.h"

#include <type_traits>

#include "unique_range_adaptor.h"
#include "intersection_adaptor.h"

#include "interval.h"

namespace {
	using SRVI = tc::make_sub_range_result<tc::vector<int>&>::type;
	using CSRVI = tc::make_sub_range_result<tc::vector<int> const&>::type;

	using SSRVI = tc::make_sub_range_result<tc::make_sub_range_result<tc::vector<int>&>::type>::type;
	using CSSRVI = tc::make_sub_range_result<tc::make_sub_range_result<tc::vector<int> const&>::type>::type;

	UNITTESTDEF( sub_range_array ) {

		int arr[4] = {1,2,3,4};
		auto arr_rng = tc::slice_by_interval(arr, tc::make_interval(1, 3));

		tc::sub_range<tc::iterator_base<int *>> mutable_iter_rng = arr_rng;
		tc::sub_range<tc::iterator_base<int const*>> iter_rng = arr_rng;
	}


	UNITTESTDEF( const_sub_range_char_ptr ) {
		tc::sub_range<tc::iterator_base<char const*>> srccp = "test";
	}

	//-------------------------------------------------------------------------------------------------------------------------

	using SRVI = tc::make_sub_range_result<tc::vector<int>&>::type;
	using CSRVI = tc::make_sub_range_result<tc::vector<int> const&>::type;

	using SSRVI = tc::make_sub_range_result<tc::make_sub_range_result<tc::vector<int>&>::type>::type;
	using CSSRVI = tc::make_sub_range_result<tc::make_sub_range_result<tc::vector<int> const&>::type>::type;

	void ref_test(SRVI & rng) noexcept {
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

	tc::vector<int> tmp_vec() noexcept {
		return tc::vector<int>();
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
		auto srvi = tc::sub_range<tc::vector<int>&>(v); // create a sub range from unrelated range
		auto ssrvi = tc::sub_range<tc::vector<int>&>(tc::sub_range<tc::vector<int>&>(v)); // copy sub range
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
		template<typename Func>
		auto operator()(Func func) const& noexcept -> tc::break_or_continue {
			RETURN_IF_BREAK(func(1));
			RETURN_IF_BREAK(func(3));
			RETURN_IF_BREAK(func(5));
			return tc::continue_;
		}
	};

	struct GenRangeMutable final {
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
			tc::slice(rgntrnsfn, boost::begin(rgntrnsfn), boost::end(rgntrnsfn)),
			MAKE_CONSTEXPR_ARRAY(1,4,9)
		);

		TEST_RANGE_EQUAL(
			MAKE_CONSTEXPR_ARRAY( 1, 2, 3 ),
			tc::untransform(tc::slice(rgntrnsfn, boost::begin(rgntrnsfn), boost::end(rgntrnsfn)))
		);

		TEST_RANGE_EQUAL(
			tc::make_singleton_range( 2 ),
			tc::untransform(tc::equal_range(tc::transform(vecn, [](int n) noexcept {return n*n; }), 4))
		);

		{
			auto rng = tc::transform(tc::filter(vecn, [](int n) noexcept {return n<3;}), [](int n) noexcept {return n*n; });

			TEST_RANGE_EQUAL(
				tc::make_singleton_range( 2 ),
				tc::untransform(tc::equal_range(rng, 4))
			);
		}

		{
			// r-value transform with l-value range
			auto&& rng = tc::untransform(tc::transform(vecn, [](int n) noexcept {return n*n;}));
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
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
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype((rng))>::value);
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				MAKE_CONSTEXPR_ARRAY(1,2,3),
				rng
			);
		}

		{
			// r-value transform with r-value range
			auto&& rng = tc::untransform(tc::transform(tc::vector<int>{1, 2, 3}, [](int n) noexcept {return n*n;}));
			_ASSERT(3 == tc::size(vecn));
			static_assert(std::is_rvalue_reference<decltype(rng)>::value);
			static_assert(!std::is_const<std::remove_reference_t<decltype(rng)>>::value);
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
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
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
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
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
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
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
				std::addressof(*boost::begin(trnsfrng).element_base()),
				std::addressof(*boost::begin(rng))
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
				std::addressof(*boost::begin(trnsfrng).element_base()),
				std::addressof(*boost::begin(rng))
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
				auto it = boost::begin(rngExpected);
				tc::for_each(
					tc::adjacent_unique_range(vecn),
					[&](auto const& subrng) noexcept {
						TEST_RANGE_EQUAL(subrng, *it);
						++it;
					}
				);
				_ASSERT(boost::end(rngExpected) == it);
			}

			{
				auto it = boost::begin(rngExpected);
				tc::for_each(
					tc::adjacent_unique_range(tc::as_const(vecn)),
					[&](auto const& subrng) noexcept {
						TEST_RANGE_EQUAL(subrng, *it);
						++it;
					}
				);
				_ASSERT(boost::end(rngExpected) == it);
			}

			{
				auto it = boost::begin(rngExpected);
				tc::for_each(
				tc::adjacent_unique_range(vecn),
					[&](auto const& subrng) noexcept {
						TEST_RANGE_EQUAL(subrng, *it);
						++it;
					}
				);
				_ASSERT(boost::end(rngExpected) == it);
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
			auto it = boost::begin(rngExpected);
			tc::for_each(
				tc::front_unique_range(vecn, Pred),
				[&](auto const& subrng) noexcept {
					TEST_RANGE_EQUAL(subrng, *it);
					++it;
				}
			);
			_ASSERT(boost::end(rngExpected) == it);
		}

		{
			std::initializer_list<int> const rngExpected[] = { // extends life-time of underlying arrays
				{1,2,3,3},
				{5,5},
				{7}
			};
			auto it = boost::begin(rngExpected);
			tc::for_each(
				tc::adjacent_unique_range(vecn, Pred),
				[&](auto const& subrng) noexcept {
					TEST_RANGE_EQUAL(subrng, *it);
					++it;
				}
			);
			_ASSERT(boost::end(rngExpected) == it);
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

	UNITTESTDEF(tc_unique) {
		TEST_RANGE_EQUAL(
			tc::adjacent_unique(MAKE_CONSTEXPR_ARRAY(1,2,2,4,7,8,8,11,11)),
			MAKE_CONSTEXPR_ARRAY(1,2,4,7,8,11)
		);

		tc::vector<int> vecn{1,1,2,2};
		auto rngunique = tc::adjacent_unique(vecn);
		auto it = boost::begin(rngunique);
		auto itvec = it.element_base();
		_ASSERT(1==*itvec);
		_ASSERT(1 == *(++itvec));
		_ASSERT(2 == *(++itvec));

		_ASSERT(1 == *it);
		_ASSERT(2 == *(++it));
		_ASSERT(1 == *(--it));
		_ASSERT(boost::begin(rngunique) == it);

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
}

