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
#include "sub_range.h"
#include "union_adaptor.h"
#include "range.t.h"

#include <type_traits>
#include <iostream>

#include "unique_range_adaptor.h"
#include "intersection_adaptor.h"

namespace {
	using namespace tc;

	//-------------------------------------------------------------------------------------------------------------------------
	// make_sub_range_result stanity checks
	namespace sub_range_detail{
		template< typename Rng > struct make_sub_range_result_tests final {
			template <typename Lhs, typename Rhs> static void test_const_ref_variants() noexcept {
				//static_assert(std::is_same< typename make_sub_range_result<Lhs>::type, sub_range<Rhs> >::value, "make_sub_range_result(Rng) gives wrong result");
				//static_assert(std::is_same< typename make_sub_range_result<Lhs&>::type, sub_range<Rhs&> >::value, "make_sub_range_result(Rng&) gives wrong result");
				//static_assert(std::is_same< typename make_sub_range_result<Lhs const>::type, sub_range<Rhs const> >::value, "make_sub_range_result(Rng const) gives wrong result");
				//static_assert(std::is_same< typename make_sub_range_result<Lhs const&>::type, sub_range<Rhs const&> >::value, "make_sub_range_result(Rng const&) gives wrong result");
			}

			static void test_sub_range_unrolling() noexcept {
				test_const_ref_variants<Rng, Rng>();
				test_const_ref_variants<sub_range<Rng>, Rng>();
				test_const_ref_variants<sub_range<sub_range<Rng>>, Rng>();
				test_const_ref_variants<sub_range<sub_range<sub_range<sub_range<sub_range<Rng>>>>>, Rng>(); // "proof" of induction
			}

			static void test() noexcept {
				test_sub_range_unrolling();
			}
		};

		static void test_make_sub_range_result() noexcept {
			make_sub_range_result_tests<tc::vector<int>>::test();
			make_sub_range_result_tests<std::string>::test();
			//make_sub_range_result_tests<tc::bstr>::test();
		}
	}

	//-------------------------------------------------------------------------------------------------------------------------

	using SRVI = make_sub_range_result<tc::vector<int>&>::type;
	using CSRVI = make_sub_range_result<tc::vector<int> const&>::type;

	using SSRVI = make_sub_range_result<make_sub_range_result<tc::vector<int>&>::type>::type;
	using CSSRVI = make_sub_range_result<make_sub_range_result<tc::vector<int> const&>::type>::type;

	//void const_ref_test(SRVI const& rng) {
	//	//CSRVI const_rng(rng);  // TODO: this fails, but shouldn't! sub_range does detect this, but range_adapter can't cope ...
	//	SRVI non_const_rng(rng); // Todo: should not work!
	//}

	//void ref_test(SRVI & rng) {
	//	CSRVI const_rng(rng);
	//	SRVI non_const_rng(rng);
	//}

	UNITTESTDEF( sub_range_array ) {

		int arr[4] = {1,2,3,4};
		auto arr_rng = tc::slice_by_index(arr, 1,3);

		sub_range<iterator_base<int *>> mutable_iter_rng = arr_rng;
		sub_range<iterator_base<int const*>> iter_rng = arr_rng;
	}


	UNITTESTDEF( const_sub_range_char_ptr ) {
		sub_range<tc::iterator_base<char const*>> srccp = "test";
	}

	//-------------------------------------------------------------------------------------------------------------------------

	using SRVI = make_sub_range_result<tc::vector<int>&>::type;
	using CSRVI = make_sub_range_result<tc::vector<int> const&>::type;

	using SSRVI = make_sub_range_result<make_sub_range_result<tc::vector<int>&>::type>::type;
	using CSSRVI = make_sub_range_result<make_sub_range_result<tc::vector<int> const&>::type>::type;

	void const_ref_test(SRVI const& rng) noexcept {
		//CSRVI const_rng(rng);    // same as const_rng2 below. TODO: this fails, but shouldn't!
		//SRVI non_const_rng(rng); // same as non_const_rng4 below. fails to compile, as it should.
	}

	void ref_test(SRVI & rng) noexcept {
		CSRVI const_rng(rng);
		SRVI non_const_rng(rng);
	}

	UNITTESTDEF( const_sub_range ) {
		//static_assert(!std::is_convertible<CSRVI, SRVI>::value, "const sub-range is convertible to sub-range breaking const correctness (explicit)");

		tc::vector<int> v;
		auto srvi = slice(v);
		auto csrvi = slice(tc::as_const(v));

		(void) srvi;
		(void) csrvi;

		SRVI non_const_rng(srvi);

		CSRVI const_rng(srvi);
		//CSRVI const_rng2(tc::as_const(srvi));   // TODO: this fails, but shouldn't! sub_range does detect this, but range_adapter can't cope ...

		//auto sr = as_const(srvi);
		//auto sr = slice(tc::as_const(srvi));
		//auto sr = as_const(slice(v, boost::begin(v), boost::end(v))); // ??

		//static_assert(!std::is_convertible<decltype(csrvi), decltype(srvi)>::value, "const sub-range is convertible to sub-range breaking const correctness (decltype)"); // this assert fires! TODO: fix, should also improve error msg below
		//SRVI non_const_rng2(csrvi);             // fails to compile, as it should, msg is correct but still pretty horrible
		//SRVI non_const_rng3(tc::as_const(v));     // fails to compile, as it should, msg is correct but still pretty horrible
		//SRVI non_const_rng4 = as_const(srvi); // fails to compile, as it should

		//_ASSERT(false);

		//ref_test(srvi);
		//const_ref_test(srvi);
	}

	UNITTESTDEF( sub_sub_range_rvalue ) {

		tc::vector<int> v;

		auto srvi = slice(v);
		auto csrvi = slice(tc::as_const(v));

		auto ssrvi = slice(slice(v));
		auto cssrvi = slice(slice(tc::as_const(v)));

		static_assert(std::is_same<decltype(ssrvi), decltype(srvi)>::value, "Sub-sub-range does not flatten to sub-range (decltype)");
		static_assert(std::is_same<decltype(cssrvi), decltype(csrvi)>::value, "const sub-sub-range does not flatten to const sub-range (decltype)");
	}

	UNITTESTDEF( sub_sub_range_lvalue ) {

		tc::vector<int> v;

		auto srvi = slice(v);
		auto csrvi = slice(tc::as_const(v));

		auto ssrvi = slice(srvi);
		auto cssrvi = slice(csrvi);

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

		auto sr = slice(v);
		auto csr = slice(tc::as_const(v));

		// use range_difference to specify bounds
		auto ssr1 = tc::slice_by_index(sr, 3, 6);
		auto cssr1 = tc::slice_by_index(csr, 3, 6);

		//_ASSERTEQUAL(ssr1, exp36);
		//_ASSERTEQUAL(cssr1, exp36);
		static_assert(std::is_same<decltype(ssr1), decltype(sr)>::value, "Sub-sub-range does not flatten to sub-range");
		static_assert(std::is_same<decltype(cssr1), decltype(csr)>::value, "const sub-sub-range does not flatten to const sub-range");

	}

	UNITTESTDEF( sub_sub_range_crazy_manual ) {

		tc::vector<int> v;

		// don't try that at work! - Seriously: sub_range is build to work correctly in a variety of situations,
		// but normaly it should not be necessary to manualy utter the type. Use slice and make_sub_range_result instead.
		// if you do need to say the type (e.g. when deducting sth.)  ...
		//_ASSERT(false);
		auto srvi = sub_range<tc::vector<int>&>(v);                                                      // create a sub range from unrelated range
		auto ssrvi = sub_range<tc::vector<int>&>(sub_range<tc::vector<int>&>(v));                       // copy sub range
		//auto s2rvi = sub_range<tc::vector<int>&>(sub_range<sub_range<tc::vector<int>&>>(v));            // flatten sub range

		//auto s3rvi = sub_range<tc::vector<int>&>(sub_range<sub_range<sub_range<tc::vector<int>&>>>(v)); // recursive flatten
		//auto s4rvi = sub_range<tc::vector<int>&>(sub_range<sub_range<sub_range<sub_range<tc::vector<int>&>>>>(v));

		// ... make sure to not build sub sub ranges (see below) - those should not occur in the wild.
		//auto ssr = sub_range<sub_range<tc::vector<int>&>>(sub_range<tc::vector<int>&>(v));
		//auto ssr2 = sub_range<sub_range<tc::vector<int>&>>(v);
	}

	UNITTESTDEF(union_range_tests) {
		TEST_RANGE_EQUAL(
			tc::make_initializer_list({1,2,3,4}),
			tc::union_range(
				tc::make_initializer_list({1,4}),
				tc::make_initializer_list({2,3})
			)
		);

		TEST_RANGE_EQUAL(
			tc::make_initializer_list({4,3,2,1,0}),
			tc::union_range(
				tc::make_initializer_list({4,2,0}),
				tc::make_initializer_list({3,2,1}),
				[](int lhs, int rhs) {return tc::compare(rhs,lhs);}
			)
		);

		_ASSERTEQUAL(
			3,
			*tc::upper_bound<tc::return_bound>(
				tc::union_range(
					tc::make_initializer_list({1,2,4}),
					tc::make_initializer_list({2,3})
				),
				2
			)
		);

		{
			auto rng = tc::union_range(
				tc::make_initializer_list({1,2,3,3,4,4,5}),
				tc::make_initializer_list({1,1,1,3,4,4,4})
			);

			TEST_RANGE_EQUAL(
				rng,
				tc::make_initializer_list({1,1,1,2,3,3,4,4,4,5})
			);

			{
				auto it = tc::lower_bound<tc::return_bound>(rng,3);
				_ASSERTEQUAL(*it,3);
				_ASSERTEQUAL(*boost::prior(it), 2);
				_ASSERTEQUAL(*boost::next(it), 3);
	}
			{
				auto it = tc::upper_bound<tc::return_bound>(rng,1);
				_ASSERTEQUAL(*it,2);
				_ASSERTEQUAL(*--it, 1);
				_ASSERTEQUAL(*--it, 1);
				_ASSERTEQUAL(*--it, 1);
			}
		}
		{
			auto rng = tc::union_range(
				tc::make_initializer_list({1,1,1,1,1,1,1,1,1}),
				tc::make_initializer_list({1,1})
			);
			auto it = tc::lower_bound<tc::return_bound>(rng,1);
			_ASSERTEQUAL(*it,1);
		}
	}

#ifdef _CHECKS
	struct GenRange final {
		template<typename Func>
		auto operator()(Func func) const noexcept -> break_or_continue {
			RETURN_IF_BREAK(func(1));
			RETURN_IF_BREAK(func(3));
			RETURN_IF_BREAK(func(5));
			return tc::continue_;
		}
	};

	struct GenRangeMutable final {
		tc::vector<double> m_vecf = tc::vector<double>({1,3,5});

		template<typename Func>
		auto operator()(Func func) noexcept -> break_or_continue {
			return tc::for_each(m_vecf, std::ref(func));
		}

		template<typename Func>
		auto operator()(Func func) const noexcept -> break_or_continue {
			return tc::for_each(m_vecf, std::ref(func));
		}
	};


#endif

	UNITTESTDEF(union_range_generator) {
		TEST_RANGE_EQUAL(
			tc::make_initializer_list({1,2,3,5,6}),
			tc::union_range(
				GenRange(),
				tc::make_initializer_list({2,6})
			)
		);

		GenRangeMutable rngMut;
		tc::vector<double> vecf;
		tc::for_each(
			tc::union_range(
				rngMut,
				vecf
			),
			[](double& d) {d += 0.1;}
		);

		TEST_RANGE_EQUAL(
			tc::make_initializer_list({1.1,3.1,5.1}),
			rngMut
		);



	}

	UNITTESTDEF(sub_range_with_tranform) {

		tc::vector<int> vecn{1,2,3};
		auto rgntrnsfn = tc::transform(vecn, [](int n) {return n*n;});

		TEST_RANGE_EQUAL(
			tc::slice(rgntrnsfn, boost::begin(rgntrnsfn), boost::end(rgntrnsfn)),
			tc::make_initializer_list({1,4,9})
		);

		TEST_RANGE_EQUAL(
			tc::make_initializer_list({ 1, 2, 3 }),
			tc::untransform(tc::slice(rgntrnsfn, boost::begin(rgntrnsfn), boost::end(rgntrnsfn)))
		);

		TEST_RANGE_EQUAL(
			tc::make_initializer_list({ 2 }),
			tc::untransform(tc::equal_range(tc::transform(vecn, [](int n) {return n*n; }), 4))
		);

		{
			auto rng = tc::transform(tc::filter(vecn, [](int n) {return n<3;}), [](int n) {return n*n; });

			TEST_RANGE_EQUAL(
				tc::make_initializer_list({ 2 }),
				tc::untransform(tc::equal_range(rng, 4))
			);
		}

		{
			// r-value transform with l-value range
			auto&& rng = tc::untransform(tc::transform(vecn, [](int n){return n*n;}));
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value, "");
			static_assert(!std::is_const<std::remove_reference_t<decltype(rng)>>::value, "");
			TEST_RANGE_EQUAL(
				tc::make_initializer_list({1,2,3}),
				rng
			);
		}

		{
			// r-value transform with l-value const range
			auto&& rng = tc::untransform(tc::transform(tc::as_const(vecn), [](int n){return n*n;}));
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype((rng))>::value, "");
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value, "");
			TEST_RANGE_EQUAL(
				tc::make_initializer_list({1,2,3}),
				rng
			);
		}

		{
			// r-value transform with r-value range
			auto&& rng = tc::untransform(tc::transform(tc::vector<int>{1, 2, 3}, [](int n){return n*n;}));
			_ASSERT(3 == tc::size(vecn));
			static_assert(std::is_rvalue_reference<decltype(rng)>::value, "");
			static_assert(!std::is_const<std::remove_reference_t<decltype(rng)>>::value, "");
			TEST_RANGE_EQUAL(
				tc::make_initializer_list({1,2,3}),
				rng
			);
		}

		{
			// l-value transform of l-value-range
			auto trnsfrng = tc::transform(vecn, [](int n){return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value, "");
			static_assert(!std::is_const<std::remove_reference_t<decltype(rng)>>::value, "");
			TEST_RANGE_EQUAL(
				tc::make_initializer_list({1,2,3}),
				rng
			);
		}

		{
			// l-value transform of const l-value-range
			auto const trnsfrng = tc::transform(tc::as_const(vecn), [](int n){return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value, "");
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value, "");
			TEST_RANGE_EQUAL(
				tc::make_initializer_list({1,2,3}),
				rng
			);
		}

		{
			// l-value transform of const l-value-range
			auto trnsfrng = tc::transform(tc::as_const(vecn), [](int n){return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERT(3 == tc::size(vecn));
			_ASSERT(std::addressof(*boost::begin(rng)) == std::addressof(*boost::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value, "");
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value, "");
			TEST_RANGE_EQUAL(
				tc::make_initializer_list({1,2,3}),
				rng
			);
		}

		{
			// l-value transform of r-value range
			auto trnsfrng = tc::transform(tc::vector<int>{1, 2, 3}, [](int n){return n*n; });
			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(
				std::addressof(*boost::begin(trnsfrng).element_base()),
				std::addressof(*boost::begin(rng))
			);
			static_assert(std::is_lvalue_reference<decltype(rng)>::value, "");
			static_assert(!std::is_const<std::remove_reference_t<decltype(rng)>>::value, "");
			TEST_RANGE_EQUAL(
				tc::make_initializer_list({1,2,3}),
				rng
			);
		}
		{
			// const l-value transform of r-value range
			auto const trnsfrng = tc::transform(tc::vector<int>{1, 2, 3}, [](int n){return n*n; });
			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(
				std::addressof(*boost::begin(trnsfrng).element_base()),
				std::addressof(*boost::begin(rng))
			);
			static_assert(std::is_lvalue_reference<decltype(rng)>::value, "");
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value, "");
			TEST_RANGE_EQUAL(
				tc::make_initializer_list({1,2,3}),
				rng
			);
		}
	}

	UNITTESTDEF(Unique_Ranges) {
		tc::vector<int> vecn{1,2,3,3,5,5,7};

		{
			auto rngExpected = tc::make_initializer_list({
				tc::make_initializer_list({1}),
				tc::make_initializer_list({2}),
				tc::make_initializer_list({3,3}),
				tc::make_initializer_list({5,5}),
				tc::make_initializer_list({7})
			});

			{
				auto it = boost::begin(rngExpected);
				tc::for_each(
					tc::adjacent_unique_range(vecn),
					[&](tc::make_sub_range_result<tc::vector<int>&>::type subrng) {
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
					[&](tc::make_sub_range_result<tc::vector<int> const&>::type subrng) {
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
					[&](tc::make_sub_range_result<tc::vector<int>&>::type subrng) {
						TEST_RANGE_EQUAL(subrng, *it);
						++it;
					}
				);
				_ASSERT(boost::end(rngExpected) == it);
			}

		}

		auto Pred = [](int lhs, int rhs) {
			return std::abs(lhs-rhs) <=1;
		};

		{
			auto rngExpected = tc::make_initializer_list({
				tc::make_initializer_list({1,2}),
				tc::make_initializer_list({3,3}),
				tc::make_initializer_list({5,5}),
				tc::make_initializer_list({7})
			});
			auto it = boost::begin(rngExpected);
			tc::for_each(
				tc::front_unique_range(vecn, Pred),
				[&](tc::make_sub_range_result<tc::vector<int>&>::type subrng) {
					TEST_RANGE_EQUAL(subrng, *it);
					++it;
				}
			);
			_ASSERT(boost::end(rngExpected) == it);
		}

		{
			auto rngExpected = tc::make_initializer_list({
				tc::make_initializer_list({1,2,3,3}),
				tc::make_initializer_list({5,5}),
				tc::make_initializer_list({7})
			});
			auto it = boost::begin(rngExpected);
			tc::for_each(
				tc::adjacent_unique_range(vecn, Pred),
				[&](tc::make_sub_range_result<tc::vector<int>&>::type subrng) {
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
				tc::make_initializer_list({1,2,2,4,7,8,8,11,11}),
				tc::make_initializer_list({2,3,4,8,8})
			),
			tc::make_initializer_list({1,2,7,11,11})
		);
	}

	UNITTESTDEF(tc_unique) {
		TEST_RANGE_EQUAL(
			tc::adjacent_unique(tc::make_initializer_list({1,2,2,4,7,8,8,11,11})),
			tc::make_initializer_list({1,2,4,7,8,11})
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

		operator int() const noexcept {return m_n;}
	};
#endif

	UNITTESTDEF(tc_unique_inplace) {
		tc::vector<MovingInt> vecmn; // list initializtion not possible with move-only element type
		vecmn.emplace_back(1);
		vecmn.emplace_back(1);
		vecmn.emplace_back(1);
		vecmn.emplace_back(2);
		vecmn.emplace_back(2);
		vecmn.emplace_back(2);
		vecmn.emplace_back(3);
		vecmn.emplace_back(4);
		vecmn.emplace_back(4);
		tc::adjacent_unique_inplace(vecmn);

		TEST_RANGE_EQUAL(
			vecmn,
			tc::make_initializer_list({1,2,3,4})
		);
	}
}

