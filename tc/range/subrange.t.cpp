
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "../container/container.h" // tc::vector
#include "../interval.h"
#include "subrange.h"
#include "range_return.h"
#include "union_adaptor.h"
#include "unique_range_adaptor.h"
#include "intersection_adaptor.h"

#include <type_traits>
#include <string_view> // for test only

namespace {
	// non-borrowed ranges are subranges
	STATICASSERTSAME(tc::make_subrange_result_t<std::string>, tc::subrange<std::string>);
	// borrowed range are iterator_ranges
	STATICASSERTSAME(tc::make_subrange_result_t<std::string&>, tc::iterator_range_t<std::string&>);
	STATICASSERTSAME(tc::make_subrange_result_t<std::string_view>, tc::iterator_range_t<std::string_view>);
	STATICASSERTSAME(tc::make_subrange_result_t<std::string_view&>, tc::iterator_range_t<std::string_view>);
	// unwrapping
	STATICASSERTSAME(tc::make_subrange_result_t<tc::subrange<std::string>>, tc::subrange<std::string>);
	STATICASSERTSAME(tc::make_subrange_result_t<tc::subrange<std::string>&>, tc::iterator_range_t<std::string&>);
	STATICASSERTSAME(tc::make_subrange_result_t<tc::span<char const>>, tc::span<char const>);
	STATICASSERTSAME(tc::make_subrange_result_t<tc::span<char const>&>, tc::span<char const>);


	UNITTESTDEF( subrange_array ) {

		int arr[4] = {1,2,3,4};
		auto arr_rng = tc::slice_by_interval(arr, tc::make_interval(1, 3));

		void(tc::implicit_cast<tc::subrange<tc::universal_range<int *>>>(arr_rng));
		void(tc::implicit_cast<tc::subrange<tc::universal_range<int const*>>>(arr_rng));
	}


	UNITTESTDEF( const_subrange_char_ptr ) {
		tc::subrange<tc::universal_range<char const*>>{"test"};
	}

	//-------------------------------------------------------------------------------------------------------------------------

	using SRVI = tc::make_subrange_result_t<tc::vector<int>&>;
	using CSRVI = tc::make_subrange_result_t<tc::vector<int> const&>;

	using SSRVI = tc::make_subrange_result_t<tc::make_subrange_result_t<tc::vector<int>&>>;
	using CSSRVI = tc::make_subrange_result_t<tc::make_subrange_result_t<tc::vector<int> const&>>;

	[[maybe_unused]] void ref_test(SRVI & rng) noexcept {
		CSRVI const_rng(rng);
		SRVI non_const_rng(rng);
		UNUSED_TEST_VARIABLE(const_rng);
		UNUSED_TEST_VARIABLE(non_const_rng);
	}

	UNITTESTDEF( const_subrange ) {
		tc::vector<int> v;
		auto srvi = tc::all(v);
		auto csrvi = tc::all(tc::as_const(v));

		(void) srvi;
		(void) csrvi;

		SRVI non_const_rng(srvi);
		UNUSED_TEST_VARIABLE(non_const_rng);
		CSRVI const_rng(srvi);
		UNUSED_TEST_VARIABLE(const_rng);
	}

	UNITTESTDEF( sub_subrange_rvalue ) {

		tc::vector<int> v;

		auto srvi = tc::all(v);
		auto csrvi = tc::all(tc::as_const(v));

		auto ssrvi = tc::all(tc::all(v));
		auto cssrvi = tc::all(tc::all(tc::as_const(v)));

		STATICASSERTSAME(decltype(ssrvi), decltype(srvi), "Sub-sub-range does not flatten to sub-range (decltype)");
		STATICASSERTSAME(decltype(cssrvi), decltype(csrvi), "const sub-sub-range does not flatten to const sub-range (decltype)");
	}

	UNITTESTDEF( sub_subrange_lvalue ) {

		tc::vector<int> v;

		auto srvi = tc::all(v);
		auto csrvi = tc::all(tc::as_const(v));

		auto ssrvi = tc::all(srvi);
		auto cssrvi = tc::all(csrvi);

		// sanity checks
		STATICASSERTSAME(decltype(srvi), SRVI, "make_subrange_result gives wrong result");
		STATICASSERTSAME(decltype(csrvi), CSRVI, "make_subrange_result gives wrong result");
		STATICASSERTSAME(decltype(ssrvi), SSRVI, "make_subrange_result gives wrong result");
		STATICASSERTSAME(decltype(cssrvi), CSSRVI, "make_subrange_result gives wrong result");

		// the actual test
		STATICASSERTSAME(decltype(ssrvi), decltype(srvi), "Sub-sub-range does not flatten to sub-range");
		STATICASSERTSAME(decltype(cssrvi), decltype(csrvi), "const sub-sub-range does not flatten to const sub-range");
	}

	UNITTESTDEF( sub_subrange_index ) {

		TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});
		TEST_init_hack(tc::vector, int, exp36, {4,5,6});

		auto sr = tc::all(v);
		auto csr = tc::all(tc::as_const(v));

		// use range_difference to specify bounds
		auto ssr1 = tc::slice_by_interval(sr, tc::make_interval(3, 6));
		auto cssr1 = tc::slice_by_interval(csr, tc::make_interval(3, 6));

		STATICASSERTSAME(decltype(ssr1), decltype(sr), "Sub-sub-range does not flatten to sub-range");
		STATICASSERTSAME(decltype(cssr1), decltype(csr), "const sub-sub-range does not flatten to const sub-range");

	}

	UNITTESTDEF(union_range_tests) {
		TEST_RANGE_EQUAL(
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3,4)),
			tc::union_range(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,4)),
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 2,3))
			)
		);

		TEST_RANGE_EQUAL(
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 4,3,2,1,0)),
			tc::union_range(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 4,2,0)),
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 3,2,1)),
				[](int const lhs, int const rhs) noexcept {return tc::compare(rhs,lhs);}
			)
		);

		_ASSERTEQUAL(
			*tc::upper_bound<tc::return_border>(
				tc::union_range(
					tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,4)),
					tc_as_constexpr(tc::make_array(tc::aggregate_tag, 2,3))
				),
				2
			),
			3
		);

		{
			auto rng = tc::union_range(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3,3,4,4,5)),
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,1,1,3,4,4,4))
			);

			TEST_RANGE_EQUAL(
				rng,
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,1,1,2,3,3,4,4,4,5))
			);

			{
				auto it = tc::lower_bound<tc::return_border>(rng,3);
				_ASSERTEQUAL(*it,3);
				_ASSERTEQUAL(*tc_modified(it, --_), 2);
				_ASSERTEQUAL(*tc_modified(it, ++_), 3);
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
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,1,1,1,1,1,1,1,1)),
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,1))
			);
			auto it = tc::lower_bound<tc::return_border>(rng,1);
			_ASSERTEQUAL(*it,1);
		}
	}

	UNITTESTDEF(union_range_generator) {
		TEST_RANGE_EQUAL(
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3,5,6)),
			tc::union_range(
				[](auto sink) noexcept -> tc::break_or_continue {
					tc_yield(sink, 1);
					tc_yield(sink, 3);
					tc_yield(sink, 5);
					return tc::continue_;
				},
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 2,6))
			)
		);

		tc::vector<double> vecf = {1,3,5};
		tc::for_each(
			tc::union_range(
				tc::make_generator_range(vecf),
				vecf
			),
			[](double& d) noexcept {d += 0.1;}
		);

		TEST_RANGE_EQUAL(
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1.1,3.1,5.1)),
			vecf
		);
	}

	UNITTESTDEF(subrange_with_tranform) {

		tc::vector<int> vecn{1,2,3};
		auto rgntrnsfn = tc::transform(vecn, [](int const n) noexcept {return n*n;});

		TEST_RANGE_EQUAL(
			tc::slice(rgntrnsfn, tc::begin(rgntrnsfn), tc::end(rgntrnsfn)),
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,4,9))
		);

		TEST_RANGE_EQUAL(
			tc_as_constexpr(tc::make_array(tc::aggregate_tag,  1, 2, 3 )),
			tc::untransform(tc::slice(rgntrnsfn, tc::begin(rgntrnsfn), tc::end(rgntrnsfn)))
		);

		TEST_RANGE_EQUAL(
			tc::single(2),
			tc::untransform(tc::equal_range(tc::transform(vecn, [](int const n) noexcept {return n*n; }), 4))
		);

		{
			auto rng = tc::transform(tc::filter(vecn, [](int const n) noexcept {return n<3;}), [](int const n) noexcept {return n*n; });

			TEST_RANGE_EQUAL(
				tc::single(2),
				tc::untransform(tc::equal_range(rng, 4))
			);
		}

		{
			// r-value transform with l-value range
			auto&& rng = tc::untransform(tc::transform(vecn, [](int const n) noexcept {return n*n;}));
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			// rewrite the following line to ::value once VC++ compiler bug is resolved https://developercommunity.visualstudio.com/content/problem/1088659/class-local-to-function-template-erroneously-cant.html
			static_assert(!std::is_const_v<std::remove_reference_t<decltype(rng)>>);
			TEST_RANGE_EQUAL(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3)),
				rng
			);
		}

		{
			// r-value transform with l-value const range
			auto&& rng = tc::untransform(tc::transform(tc::as_const(vecn), [](int const n) noexcept {return n*n;}));
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype((rng))>::value);
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3)),
				rng
			);
		}

		{
			// r-value transform with r-value range
			auto rng = tc::untransform(tc::transform(tc::vector<int>{1, 2, 3}, [](int const n) noexcept {return n*n;}));
			STATICASSERTSAME(decltype(rng),tc::vector<int>);
			TEST_RANGE_EQUAL(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3)),
				rng
			);
		}

		{
			// l-value transform of l-value-range
			auto trnsfrng = tc::transform(vecn, [](int const n) noexcept {return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			// rewrite the following line to ::value once VC++ compiler bug is resolved https://developercommunity.visualstudio.com/content/problem/1088659/class-local-to-function-template-erroneously-cant.html
			static_assert(!std::is_const_v<std::remove_reference_t<decltype(rng)>>);
			TEST_RANGE_EQUAL(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3)),
				rng
			);
		}

		{
			// l-value transform of const l-value-range
			auto const trnsfrng = tc::transform(tc::as_const(vecn), [](int const n) noexcept {return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3)),
				rng
			);
		}

		{
			// l-value transform of const l-value-range
			auto trnsfrng = tc::transform(tc::as_const(vecn), [](int const n) noexcept {return n*n;});

			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(tc::size(vecn), 3);
			_ASSERTEQUAL(std::addressof(*tc::begin(rng)), std::addressof(*tc::begin(vecn)));
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3)),
				rng
			);
		}

		{
			// l-value transform of r-value range
			auto trnsfrng = tc::transform(tc::vector<int>{1, 2, 3}, [](int const n) noexcept {return n*n; });
			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(
				std::addressof(*tc::begin(trnsfrng).element_base()),
				std::addressof(*tc::begin(rng))
			);
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			// rewrite the following line to ::value once VC++ compiler bug is resolved https://developercommunity.visualstudio.com/content/problem/1088659/class-local-to-function-template-erroneously-cant.html
			static_assert(!std::is_const_v<std::remove_reference_t<decltype(rng)>>);
			TEST_RANGE_EQUAL(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3)),
				rng
			);
		}
		{
			// const l-value transform of r-value range
			auto const trnsfrng = tc::transform(tc::vector<int>{1, 2, 3}, [](int const n) noexcept {return n*n; });
			auto&& rng = tc::untransform(trnsfrng);
			_ASSERTEQUAL(
				std::addressof(*tc::begin(trnsfrng).element_base()),
				std::addressof(*tc::begin(rng))
			);
			static_assert(std::is_lvalue_reference<decltype(rng)>::value);
			static_assert(std::is_const<std::remove_reference_t<decltype(rng)>>::value);
			TEST_RANGE_EQUAL(
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3)),
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

		static auto constexpr Pred = [](int const lhs, int const rhs) noexcept {
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
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,2,4,7,8,8,11,11)),
				tc_as_constexpr(tc::make_array(tc::aggregate_tag, 2,3,4,8,8))
			),
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,7,11,11))
		);
	}

	UNITTESTDEF(difference_unordered_set) {
		tc::unordered_set<int> setn1;
		tc::cont_assign(setn1, tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3,4,5,6,7,8,9,10,11)));

		tc::unordered_set<int> setn2;
		tc::cont_assign(setn2, tc_as_constexpr(tc::make_array(tc::aggregate_tag, 10,9,8,7,6,5,4,3,2,1)));

		TEST_RANGE_EQUAL(
			tc::set_difference(setn1, setn2),
			tc::single(11)
		);

		tc::vector<int> vecn;
		tc::for_each(tc::set_intersect(setn1, setn2), [&](int const n) noexcept {
			vecn.emplace_back(n);
		});

		TEST_RANGE_EQUAL(
			tc::sort(vecn),
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3,4,5,6,7,8,9,10))
		);
	}

	UNITTESTDEF(tc_unique) {
		TEST_RANGE_EQUAL(
			tc::adjacent_unique(tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,2,4,7,8,8,11,11))),
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,4,7,8,11))
		);

		TEST_RANGE_EQUAL(
			tc::reverse(tc::adjacent_unique(tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,2,4,7,8,8,11,11)))),
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 11,8,7,4,2,1))
		);

		TEST_RANGE_EQUAL(
			tc::transform(
				tc::adjacent_unique(
					tc::make_range_of_iterators(tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,2,3,3,3,4,7,8,8,11,11))),
					tc::projected(tc::fn_equal_to(), tc::fn_indirection())
				),
				tc::fn_indirection()
			),
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3,4,7,8,11))
		);

		TEST_RANGE_EQUAL(
			tc::transform(
				tc::reverse(tc::adjacent_unique(
					tc::make_range_of_iterators(tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,2,3,3,3,4,7,8,8,11,11))),
					tc::projected(tc::fn_equal_to(), tc::fn_indirection())
				)),
				tc::fn_indirection()
			),
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 11,8,7,4,3,2,1))
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
			tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3,4))
		);
	}

	UNITTESTDEF( take_first_sink ) {
		static auto constexpr rngn = [](auto sink) noexcept {
			for(int i=0;;++i) { tc_yield(sink, i); }
		};

		_ASSERTEQUAL(tc::for_each(tc::begin_next<tc::return_take>(rngn), [](int) noexcept {}), tc::continue_);
		_ASSERTEQUAL(tc::for_each(tc::begin_next<tc::return_take>(rngn), [](int) noexcept { return tc::break_; }), tc::break_);
		_ASSERT(tc::equal(tc_as_constexpr(tc::make_array(tc::aggregate_tag, 0,1,2,3)), tc::begin_next<tc::return_take>(rngn, 4)));

		auto rngch = tc::concat("123", [](auto&& sink) noexcept { return tc::for_each("456", tc_move_if_owned(sink)); });

		struct assert_no_single_char_sink /*final*/ {
			auto operator()(char) const& noexcept { _ASSERTFALSE; return tc::construct_default_or_terminate<tc::break_or_continue>(); }
			auto chunk(tc::span<char const> str) const& noexcept { return tc::constant<tc::continue_>(); }
		};
		_ASSERTEQUAL(tc::for_each(tc::begin_next<tc::return_take>(rngch, 4), assert_no_single_char_sink()), tc::continue_);
		_ASSERT(tc::equal("1234", tc::begin_next<tc::return_take>(rngch, 4)));
	}

	UNITTESTDEF( drop_first_sink ) {
		static auto constexpr rngn = [](auto sink) noexcept {
			for(int i=0; i < 7;++i) { tc_yield(sink, i); }
			return tc::continue_;
		};

		_ASSERTEQUAL(tc::for_each(tc::begin_next<tc::return_drop>(rngn), [](int) noexcept {}), tc::continue_);
		_ASSERTEQUAL(tc::for_each(tc::begin_next<tc::return_drop>(rngn), [](int) noexcept { return tc::break_; }), tc::break_);
		_ASSERT(tc::equal(tc_as_constexpr(tc::make_array(tc::aggregate_tag, 4,5,6)), tc::begin_next<tc::return_drop>(rngn, 4)));

		auto rngch = tc::begin_next<tc::return_drop>(tc::concat("123", [](auto&& sink) noexcept { return tc::for_each("456", tc_move_if_owned(sink)); }), 2);

		struct assert_no_single_char_sink /*final*/ {
			auto operator()(char) const& noexcept { _ASSERTFALSE; return tc::construct_default_or_terminate<tc::break_or_continue>(); }
			auto chunk(tc::span<char const> str) const& noexcept { return tc::constant<tc::continue_>(); }
		};
		_ASSERTEQUAL(tc::for_each(rngch, assert_no_single_char_sink()), tc::continue_);
		_ASSERT(tc::equal("3456", rngch));
	}

	UNITTESTDEF( span_from_subrange ) {
		tc::string<char> str("1234");
		_ASSERTEQUAL( tc::begin(tc::as_span(tc::begin_next<tc::return_drop>(str, 2))), tc::begin(tc::begin_next<tc::return_drop>(tc::as_span(str), 2)) );
		_ASSERTEQUAL( tc::end(tc::as_span(tc::begin_next<tc::return_take>(str, 2))), tc::end(tc::begin_next<tc::return_take>(tc::as_span(str), 2)) );
	}
#ifndef __clang__
	namespace {
		struct STestPtrs {
			constexpr STestPtrs(char const* itBegin,char const* itEnd): m_itBegin(itBegin),m_itEnd(itEnd) {}

			char const* m_itBegin;
			char const* m_itEnd;
		};
	}

	UNITTESTDEF( constexpr_ptr_to_string_literal_runtime_bug ) {
		// https://developercommunity.visualstudio.com/content/problem/900648/codegen-constexpr-pointer-to-trailing-zero-on-stri.html
		// Visual Studio 2017/2019 will fire at run time if we disable string pooling by setting /GF-.
		constexpr char const* str = "abcde";
		constexpr STestPtrs ptrs(tc::ptr_begin(str), tc::ptr_end(str));
		static_assert(tc::begin(str)==ptrs.m_itBegin);
		static_assert(tc::end(str)==ptrs.m_itEnd);
		_ASSERTEQUAL(tc::begin(str), ptrs.m_itBegin); // run time compare
		_ASSERTEQUAL(tc::end(str), ptrs.m_itEnd);  // run time compare
	}
#endif

	UNITTESTDEF(and_then) {
		static auto constexpr fn=[](auto const n) noexcept {return n+1;};
		std::optional<int> on(12);

		auto n1=tc::and_then(on, fn);
		static_assert(std::is_same<decltype(n1), int>::value);
		_ASSERTEQUAL(n1, 13);

		auto on1=tc::and_then(on, tc::chained(tc::fn_make_optional{}, fn));
		static_assert(std::is_same<decltype(on1), std::optional<int>>::value);
		_ASSERTEQUAL(*VERIFY(on1), 13);

		on=std::nullopt;
		auto n2=tc::and_then(on, fn);
		static_assert(std::is_same<decltype(n2), int>::value);
		_ASSERTEQUAL(n2,0);

		auto on2=tc::and_then(on, tc::chained(tc::fn_make_optional{}, fn));
		static_assert(std::is_same<decltype(on1), std::optional<int>>::value);
		_ASSERT(!on2);

		struct X{ int const n; };
		struct Y{ X const* px; };
		struct Z{ Y const* py; };

		X const x{ 23 };
		Y const y{ &x };
		Z const z{ &y };

		auto n3=tc::and_then(&z, tc_member(.py), tc_member(.px), tc_member(.n));
		static_assert(std::is_same<decltype(n3), int>::value);
		_ASSERTEQUAL(n3, 23);

		Z const* pz=nullptr;
		auto n4=tc::and_then(pz, tc_member(.py), tc_member(.px), tc_member(.n));
		static_assert(std::is_same<decltype(n4), int>::value);
		_ASSERTEQUAL(n4, 0);
	}

}

STATICASSERTEQUAL( sizeof(tc::span<int>), 2 * sizeof(int*) );
static_assert( std::is_nothrow_default_constructible<tc::span<int>>::value );
static_assert( std::is_trivially_destructible<tc::span<int>>::value );
static_assert( std::is_trivially_copy_constructible<tc::span<int>>::value );
static_assert( std::is_trivially_move_constructible<tc::span<int>>::value );
static_assert( std::is_trivially_copy_assignable<tc::span<int>>::value );
static_assert( std::is_trivially_move_assignable<tc::span<int>>::value );
static_assert( std::is_trivially_copyable<tc::span<int>>::value );

namespace {
	UNITTESTDEF( subrange_index_translation ) {
		TEST_RANGE_EQUAL("est", []() noexcept {
			tc::string<char> str = "Test";
			return tc::begin_next<tc::return_drop>(tc_move(str));
		}());
	}
}
