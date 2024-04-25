
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/functors.h"
#include "../base/tag_type.h"
#include "../range/adjacent_adaptor.h"
#include "../range/make_range.h"
#include "../range/meta.h"
#include "../range/join_framed_adaptor.h"
#include "../range/subrange.h"
#include "../range/unique_range_adaptor.h"
#include "../range/iota_range.h"
#include "../range/concat_adaptor.h"
#include "../range/filter_adaptor.h"

#include "../container/container_traits.h"
#include "../container/container.h" // tc::vector
#include "../container/cont_assign.h"
#include "../container/insert.h"
#include "../container/cont_reserve.h"

#include "../storage_for.h"
#include "../string/spirit.h"

#include "append.h"
#include "size.h"
#include "element.h"
#include "equal.h"
#include "quantifier.h"
#include "empty.h"
#include "minmax.h"
#include "compare.h"
#include "for_each_xxx.h"
#include "accumulate.h"
#include "size_bounded.h"
#include "find.h"
#include "filter_inplace.h"
#include "best_element.h"
#include "partition_iterator.h"
#include "partition_range.h"
#include "size_linear.h"


#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/utility.hpp>
#include <boost/range/algorithm/copy.hpp>

#include <boost/multi_index_container_fwd.hpp>
#include <boost/multi_index/hashed_index_fwd.hpp>
#include <boost/multi_index/ordered_index_fwd.hpp>
#include <boost/intrusive/set.hpp>

#include <type_traits>
#include <set>
#include <map>
#include <utility>
#include <algorithm>

namespace tc {
	template< typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] constexpr bool is_strictly_sorted(Rng const& rng, Less less = Less()) noexcept {
		return tc::all_of(tc::adjacent<2>(rng), [&](auto const& first, auto const& second) noexcept {
			return less(first, second);
		});
	}
	template< typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] constexpr bool is_sorted(Rng const& rng, Less less = Less()) MAYTHROW {
		return !tc::any_of(tc::adjacent<2>(rng), [&](auto const& first, auto const& second) MAYTHROW {
			return less(second, first); // MAYTHROW
		});
	}

	/////////////////////////////////
	// associative containers

	namespace explicit_convert_adl {
		template<typename TTarget, typename Rng>
			requires has_mem_fn_lower_bound<TTarget> || has_mem_fn_hash_function<TTarget>
		TTarget explicit_convert_impl(adl_tag_t, std::type_identity<TTarget>, Rng&& rng) noexcept {
			TTarget cont;
			// force each element to be inserted
			tc::cont_must_insert_range(cont, tc_move_if_owned(rng));
			return cont;
		}
	}

	/////////////////////////////////////////////////////
	// sort
#ifdef __clang__
	namespace constexpr_sort_inplace_detail {

		// This duplicates the functionality of tc::sort, which cannot be used in a constant expression,
		// because it uses std::sort which is not constexpr (until C++20).
		template<typename Iterator, typename Pred>
		constexpr void constexpr_sort_inplace(Iterator itBegin, Iterator itEnd, Pred pred) noexcept {
#if defined(_CHECKS) && defined(TC_PRIVATE)
			int nIterationCount = 0;
#endif
			while (1 < itEnd - itBegin) {
				_ASSERTENOTIFY( ++nIterationCount <= 32 ); // Do we actually run into O(n^2) complexity?
				auto itPartitionBegin = itBegin;
				auto itPartitionEnd = tc_modified(itEnd, --_);
				// Any iterator in the interval [itBegin, itEnd - 2] works.
				// Middle is best for sorted and reverse sorted ranges.
				auto itPivotElement = tc::middle_point(itBegin, itPartitionEnd);

				for (;;) {
					while (pred(*itPartitionBegin, *itPivotElement)) {
						++itPartitionBegin;
					}
					while (pred(*itPivotElement, *itPartitionEnd)) {
						--itPartitionEnd;
					}

					if (itPartitionEnd <= itPartitionBegin) {
						break;
					}

					if (itPartitionBegin == itPivotElement) {
						itPivotElement = itPartitionEnd;
					} else if (itPartitionEnd == itPivotElement) {
						itPivotElement = itPartitionBegin;
					}
					
					tc::swap(*itPartitionBegin, *itPartitionEnd);

					++itPartitionBegin;
					--itPartitionEnd;
				}

				auto const itSplitPoint = itPartitionEnd + 1;
	#if defined(_CHECKS) && defined(_DEBUG)
				_ASSERTE(itBegin < itSplitPoint);
				_ASSERTE(itSplitPoint < itEnd);

				for (auto j = itBegin; j < itSplitPoint; ++j) {
					_ASSERTE(!pred(*itPivotElement, *j));
				}
				for (auto j = itSplitPoint; j < itEnd; ++j) {
					_ASSERTE(!pred(*j, *itPivotElement));
				}
	#endif

				// Recur into smaller subrange and sort larger subrange in next iteration to get O(log n) space complexity.
				if( itSplitPoint - itBegin < itEnd - itSplitPoint ) {
					constexpr_sort_inplace_detail::constexpr_sort_inplace(itBegin, itSplitPoint, pred);
					itBegin = itSplitPoint;
				} else {
					constexpr_sort_inplace_detail::constexpr_sort_inplace(itSplitPoint, itEnd, pred);
					itEnd = itSplitPoint;
				}
			}
		}
	}
#endif
	template<typename Rng, typename Less = tc::fn_less>
	constexpr void sort_inplace(Rng&& rng, Less&& less = Less()) noexcept(noexcept(less(tc::front(rng), tc::front(rng)))) {
		if constexpr( has_mem_fn_sort< Rng >) {
			static_assert( std::is_lvalue_reference<Rng>::value );
			rng.sort( tc_move_if_owned(less) );
		} else {
#ifdef __clang__ // xcode12 does not support constexpr std::sort
			if(std::is_constant_evaluated()) {
				constexpr_sort_inplace_detail::constexpr_sort_inplace(tc::begin(rng), tc::end(rng), less);
				_ASSERTE( tc::is_sorted(rng, less) );
			} else {
#endif
				std::sort( tc::begin(rng), tc::end(rng), tc_move_if_owned(less) );
#ifdef __clang__
			}
#endif
		}
	}

	template<typename Rng, typename Less = tc::fn_less>
	void stable_sort_inplace(Rng&& rng, Less&& less = Less()) noexcept(noexcept(less(tc::front(rng), tc::front(rng)))) {
		std::stable_sort(tc::begin(rng), tc::end(rng), tc_move_if_owned(less));
	}

	template< typename Less=tc::fn_less >
	constexpr void strictly_sort_inplace(auto&& rng, Less less=Less()) noexcept(noexcept(less(tc::front(rng), tc::front(rng)))) {
		tc::sort_inplace(rng, less);
		_ASSERT( tc::is_strictly_sorted(rng, less) );
	}

	namespace no_adl {
		template<typename Rng, bool bStable>
		struct [[nodiscard]] sorted_index_adaptor final
			: tc::range_adaptor_base_range<Rng>
			, tc::index_range_adaptor<
				sorted_index_adaptor<Rng, bStable>,
				tc::vector<tc::index_t<std::remove_reference_t<Rng>>>,
				index_range_adaptor_flags::inherit_begin_end | index_range_adaptor_flags::inherit_traversal
			>
			, private tc::nonmovable // disable copy ctor and default move ctor
		{
		private:
			using this_type = sorted_index_adaptor;
			using base_range_base = tc::range_adaptor_base_range<Rng>;
			using index_base = typename this_type::index_range_adaptor;

		public:
			using base_range_base::base_range;
			using typename index_base::tc_index;
			static constexpr bool c_bHasStashingIndex=false;

			template<typename LessOrComp>
			explicit sorted_index_adaptor(Rng&& rng, LessOrComp lessorcomp) noexcept
				: base_range_base(tc::aggregate_tag, tc_move_if_owned(rng))
				, index_base(tc::aggregate_tag, tc::explicit_cast<tc::vector<tc::index_t<std::remove_reference_t<Rng>>>>(
					tc::transform(tc::make_range_of_iterators(base_range()), tc_fn(tc::iterator2index<Rng>))
				))
			{
				tc::sort_inplace(
					this->index_base::base_range(),
					[&](auto const& idxLhs, auto const& idxRhs ) noexcept -> bool {
						tc_auto_cref(lhs, tc::dereference_index(base_range(), idxLhs));
						tc_auto_cref(rhs, tc::dereference_index(base_range(), idxRhs));
						if constexpr (bStable) {
							static_assert(tc::random_access_range<Rng>);
							tc_auto_cref(order, lessorcomp(lhs, rhs));
							static_assert(tc::is_comparison_category<std::remove_cvref_t<decltype(order)>>);
							if(tc::is_eq(order)) {
								return 0<tc::distance_to_index(base_range(), idxLhs, idxRhs);
							} else if(std::is_lt(order)) {
								return true;
							} else {
								_ASSERTDEBUG(std::is_gt(order));
								return false;
							}
						} else {
							return lessorcomp(lhs, rhs);
						}
					}
				);
			}

			sorted_index_adaptor(sorted_index_adaptor&& src) noexcept requires tc::stable_index_on_move<Rng> // tc::reference_or_value is movable for const Rng as well
				: base_range_base(tc::base_cast<base_range_base>(tc_move(src)))
				, index_base(tc::base_cast<index_base>(tc_move(src)))
			{}
		private:
			STATIC_FINAL(dereference_index)(tc_index const& idx) const& return_decltype_MAYTHROW(
				tc::dereference_index(base_range(), *idx)
			)

			STATIC_FINAL(dereference_index)(tc_index const& idx) & return_decltype_MAYTHROW(
				tc::dereference_index(base_range(), *idx)
			)

		public:
			static decltype(auto) element_base_index(tc_index const& idx) noexcept {
				return *idx;
			}
			static decltype(auto) element_base_index(tc_index&& idx) noexcept {
				return *tc_move(idx);
			}

			constexpr decltype(auto) dereference_untransform(tc_index const& idx) const& noexcept {
				return base_range().dereference_untransform(*idx);
			}
		};
	}
	using no_adl::sorted_index_adaptor;

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sorted_iterator_range(Rng& rng, Less&& less = Less()) noexcept {
		auto vecitSorted=tc::make_vector( tc::make_range_of_iterators(rng) );
		tc::sort_inplace(vecitSorted, tc::projected( tc_move_if_owned(less), tc::fn_indirection() ) );
		return vecitSorted;
	}

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sort(Rng&& rng, Less&& less = Less()) noexcept {
		return tc::sorted_index_adaptor<Rng, false/*bStable*/>(tc_move_if_owned(rng), tc_move_if_owned(less));
	}

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] constexpr auto constexpr_sort(Rng&& rng, Less&& less = Less()) noexcept {
		auto a = tc::make_array(tc_move_if_owned(rng));
		tc::sort_inplace(a, tc_move_if_owned(less));
		return a;
	}

	template<typename Rng, typename Comp = tc::fn_compare>
	[[nodiscard]] auto stable_sort(Rng&& rng, Comp&& comp = Comp()) noexcept {
		return tc::sorted_index_adaptor<Rng, true/*bStable*/>(tc_move_if_owned(rng), tc_move_if_owned(comp));
	}
	
	namespace no_adl {
		template< typename Rng >
		struct [[nodiscard]] untransform_adaptor
			:  tc::index_range_adaptor<
				untransform_adaptor<Rng>,
				Rng, tc::index_range_adaptor_flags::inherit_begin_end | tc::index_range_adaptor_flags::inherit_traversal
			>
		{
		private:
			using this_type = untransform_adaptor;
			using base_ = typename untransform_adaptor::index_range_adaptor;

		public:
			using typename base_::tc_index;
			using base_::base_;

			STATIC_FINAL_MOD(template<typename Index> constexpr, dereference_index)(Index&& idx) & return_decltype_MAYTHROW(
				this->base_range().dereference_untransform(tc_move_if_owned(idx))
			)

			STATIC_FINAL_MOD(template<typename Index> constexpr, dereference_index)(Index&& idx) const& return_decltype_MAYTHROW(
				this->base_range().dereference_untransform(tc_move_if_owned(idx))
			)
		};
	}

	template<typename Rng>
	[[nodiscard]] auto untransform(Rng&& rng) noexcept {
		return no_adl::untransform_adaptor<Rng>(tc::aggregate_tag, tc_move_if_owned(rng));
	}

	///////////////////////////////////////
	// partition ranges into subranges

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto ordered_unique_range(Rng&& rng, Less&& less = Less()) noexcept {
		_ASSERTDEBUG( tc::is_sorted( rng, less ) );
		return tc::adjacent_unique_range( tc_move_if_owned(rng), std::not_fn( tc_move_if_owned(less) ) );
	}

	template<typename Rng, typename FuncRngStart, typename FuncRngElement, typename Less = tc::fn_less>
	auto generator_ordered_unique_range(Rng&& rng, FuncRngStart funcStart, FuncRngElement funcElem, Less&& less = Less()) noexcept {
		std::optional<tc::range_value_t<Rng>> oelem;
		tc::for_each(tc_move_if_owned(rng), [&](auto&& element) noexcept {
			if (!oelem || less(*oelem, element)) {
				tc_return_if_break(tc::continue_if_not_break(funcStart))
				tc::optional_emplace(oelem, element);
			}
			return tc::continue_if_not_break(funcElem,tc_move_if_owned(element));
		});
	}

	template<typename Rng, typename Comp = tc::fn_compare>
	[[nodiscard]] auto stable_sort_unique_range(Rng&& rng, Comp const& comp = Comp()) noexcept {
		return tc::ordered_unique_range( tc::stable_sort( tc_move_if_owned(rng), comp ), tc::lessfrom3way(comp) );
	}

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sort_unique_range(Rng&& rng, Less const& less = Less()) noexcept {
		return tc::ordered_unique_range( tc::sort( tc_move_if_owned(rng), less ), less );
	}

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sort_inplace_unique_range(Rng&& rng, Less&& less = Less()) noexcept {
		static_assert( !std::is_reference<Rng>::value );
		tc::sort_inplace( rng, std::ref(less) );
		return tc::ordered_unique_range( tc_move_if_owned(rng), tc_move_if_owned(less) );
	}

	template< typename Rng, typename Less, typename Accu >
	void accumulate_each_unique_range(Rng&& cont, Less less, Accu accu) noexcept {
		range_filter< tc::decay_t<Rng> > rngfilter( cont );
		tc::for_each(
			tc::ordered_unique_range(
				cont,
				tc_move(less)
			),
			[&accu,&rngfilter]( auto const& rngEqualSubRange ) noexcept {
				for(
					auto it=tc::begin_next<tc::return_border>(rngEqualSubRange);
					it!=tc::end(rngEqualSubRange);
					++it
				) {
					tc_invoke(accu,*tc::begin(rngEqualSubRange), *it);
				}
				rngfilter.keep( tc::begin(rngEqualSubRange) );
			}
		);
	}

	template< typename Rng, typename Less, typename Accu >
	void sort_accumulate_each_unique_range(Rng&& cont, Less less, Accu accu) noexcept {
		tc::sort_inplace( cont, less );
		tc::accumulate_each_unique_range(cont, tc_move(less), tc_move(accu));
	}

	template< typename Cont, typename Equals = decltype(tc::equal_to)>
	void front_unique_inplace(Cont& cont, Equals&& pred={}) noexcept(noexcept(pred(tc::front(cont), tc::front(cont)))) {
		tc::range_filter< tc::decay_t<Cont> > rngfilter(cont);
		tc::for_each(
			tc::transform(
				tc::front_unique_range(cont, tc_move_if_owned(pred)),
				tc_fn(tc::begin)
			),
			[&](auto it) noexcept {
				rngfilter.keep(tc_move(it));
			}
		);
	}

	/*
		In contrast to std::unique, tc::adjacent_unique / tc::adjacent_unique_inplace always compares adjacent elements. This allows implementing
		bidirectional tc::adjacent_unique, with tc::adjacent_unique_inplace yielding the same result.
	*/
	template< typename Cont, typename Equals=decltype(tc::equal_to)>
	constexpr void adjacent_unique_inplace(Cont & cont, Equals&& pred={}) noexcept(noexcept(pred(tc::front(cont), tc::front(cont)))) {
		tc::range_filter< tc::decay_t<Cont> > rngfilter(cont);
		// When this function is evaluated at compile time, the range returned by tc::make_range_of_iterators cannot use an rvalue as a base range.
		// This is because it stores the base range in a mutable field inside tc::reference_or_value.
		tc::for_each(
			tc::may_remove_current(tc::make_range_of_iterators(tc::as_lvalue(tc::adjacent_unique(cont, tc_move_if_owned(pred/*MAYTHROW*/))))),
			[&](auto it) noexcept {
				rngfilter.keep(it.element_base());
			}
		);
	}

	template<typename Cont, typename Less=tc::fn_less>
	constexpr void ordered_unique_inplace( Cont& cont, Less less=Less() ) noexcept(noexcept(less(tc::front(cont), tc::front(cont)))) {
		_ASSERTDEBUG( tc::is_sorted( cont, less ) );
		tc::adjacent_unique_inplace( cont, std::not_fn( tc_move(less) ) );
	}

	template< typename Cont, typename Less=tc::fn_less >
	constexpr void sort_unique_inplace(Cont& cont, Less less=Less()) noexcept(noexcept(less(tc::front(cont), tc::front(cont)))) {
		tc::sort_inplace( cont, less );
		tc::ordered_unique_inplace( cont, tc_move(less) );
	}

	template< typename Cont, typename Less=tc::fn_less >
	void stable_sort_unique_inplace(Cont& cont, Less less=Less()) noexcept(noexcept(less(tc::front(cont), tc::front(cont)))) {
		tc::stable_sort_inplace( cont, less );
		tc::ordered_unique_inplace( cont, tc_move(less) );
	}

	template<typename Rng, typename Less>
	auto ordered_unique_begin_and_count(Rng&& rng, Less&& less) noexcept {
		return tc::transform(tc::ordered_unique_range( tc_move_if_owned(rng), tc_move_if_owned(less)), [&](auto const& rngSub) noexcept {
			return std::make_pair(
				tc::begin(rngSub),
				tc::implicit_cast<typename boost::range_size< std::remove_reference_t<Rng> >::type >(tc::size_linear(rngSub))
			);
		});
	}

	template<typename RangeReturn, typename Rng>
	[[nodiscard]] auto plurality_element(Rng&& rng) noexcept {
		auto vecitSorted = tc::sorted_iterator_range(rng, tc::fn_less()); // do not inline, oit->first points into vecitSorted
		if(auto oit = tc::max_element<tc::return_value_or_none>(
			tc::ordered_unique_begin_and_count(
				vecitSorted,
				tc::projected(tc::fn_less(), fn_indirection())
			),
			tc_member(.second)
		)) {
			return RangeReturn::pack_element(tc_move_always(*oit->first), tc_move_if_owned(rng));
		} else {
			return RangeReturn::pack_no_element(tc_move_if_owned(rng));
		}
	}

	template< typename RangeReturn, typename Rng, typename Pred >
	[[nodiscard]] decltype(auto) trim_left_if(Rng&& rng, Pred&& pred) MAYTHROW {
		static_assert( RangeReturn::allowed_if_always_has_border );
		return tc_rewrap_temporary(Rng&&, RangeReturn::pack_border(
			tc::find_first_if<tc::return_border_before_or_end>( tc_unwrap_temporary(rng), std::not_fn(tc_move_if_owned(pred)) ),
			tc_unwrap_temporary(tc_move_if_owned(rng))
		));
	}

	template< typename RangeReturn, typename Rng, typename Pred >
	[[nodiscard]] decltype(auto) trim_right_if(Rng&& rng, Pred&& pred) MAYTHROW {
		static_assert( RangeReturn::allowed_if_always_has_border );
		return tc_rewrap_temporary(Rng&&, RangeReturn::pack_border(
			tc::find_last_if<tc::return_border_after_or_begin>( tc_unwrap_temporary(rng), std::not_fn(tc_move_if_owned(pred)) ),
			tc_unwrap_temporary(tc_move_if_owned(rng))
		));
	}

	template< typename Rng, typename Pred >
	[[nodiscard]] decltype(auto) trim_if(Rng&& rng, Pred&& pred) MAYTHROW {
		return tc_invoke(tc::chained(
			 // clang crashes if we use return_decltype_allow_xvalue
			[&](auto&& rng)	-> decltype(auto) { return tc::trim_left_if<tc::return_drop>(tc_move_if_owned(rng), tc_move_if_owned(pred)); },
			[&](auto&& rng)	-> decltype(auto) { return tc::trim_right_if<tc::return_take>(tc_move_if_owned(rng), pred); }
		), tc_move_if_owned(rng));
	}

	template< typename RangeReturn, typename Rng, typename RngTrim >
	[[nodiscard]] decltype(auto) trim_left(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_left_if<RangeReturn>( tc_move_if_owned(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< typename RangeReturn, typename Rng, typename RngTrim >
	[[nodiscard]] decltype(auto) trim_right(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_right_if<RangeReturn>( tc_move_if_owned(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< typename Rng, typename RngTrim >
	[[nodiscard]] decltype(auto) trim(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_if( tc_move_if_owned(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< typename Rng, typename T >
	[[nodiscard]] bool contains_single(Rng const& rng, T const& t) noexcept {
		return 1==size_bounded(rng,2) && tc::front( rng )==t;
	}

	namespace cont_find_detail {
		template< typename RangeReturn, typename Cont>
		decltype(auto) cont_find_impl(Cont& cont, tc::iterator_t< Cont > it) noexcept {
			if( it==tc::end(cont) ) {
				return RangeReturn::pack_no_element(
					cont
				);
			} else {
				return RangeReturn::pack_element(
					it,
					cont
				);
			}
		}
	}

	template< typename RangeReturn, typename Cont, typename Arg >
	[[nodiscard]] decltype(auto) cont_find(Cont& cont, Arg&& arg) noexcept {
		return cont_find_detail::cont_find_impl<RangeReturn>(cont, cont.find(tc_move_if_owned(arg)));
	}

#ifdef _DEBUG
	using static_vector_size_t = std::uint32_t; // fixed width integer for shared heap
	namespace static_vector_adl {
		template< typename T, tc::static_vector_size_t N> struct static_vector;
	}
	using static_vector_adl::static_vector;
#endif

	template<typename Rng>
	void assert_no_null_terminator(Rng const& rng) noexcept {
		if constexpr( tc::char_type< tc::range_value_t<Rng const&> > ) {
			_ASSERT( !tc::find_first<tc::return_bool>(rng, tc::explicit_cast<tc::range_value_t<Rng const&>>('\0') ));
		}
	}

	template<typename Rng>
	void remove_null_terminator(Rng& rng) noexcept {
		static_assert( tc::char_type< tc::range_value_t<decltype((rng))> > );
		_ASSERTEQUAL( tc::back(rng), tc::explicit_cast< tc::range_value_t<decltype((rng))> >('\0') );
		tc::drop_last_inplace(rng);
		tc::assert_no_null_terminator(rng);
	}

	template<typename Char, std::size_t N>
	[[nodiscard]] constexpr Char const* take_null_terminated(Char const (&ach)[N]) noexcept {
		static_assert( tc::char_type<Char> );
		_ASSERTDEBUG( tc::find_first<tc::return_bool>(tc::as_array(ach), Char()) );
		return ach;
	}

	namespace get_buffer_detail {
#if defined(TC_PRIVATE) && defined(_DEBUG) && !defined(__clang__)
		namespace no_adl {
			template<typename Cont>
			struct container_with_sentinel final {
				using type = Cont;
			};

			template<typename T, tc::static_vector_size_t N>
			struct container_with_sentinel<tc::static_vector<T, N>> final {
				using type = tc::static_vector<T, N + 1>;
			};
		}
#endif

		/* Calls func(pBuffer, nBufferSize) with buffers of increasing size; func may read into
		[pBuffer, pBuffer+nBufferSize) and return nSize, which is either the correct size of the
		buffer (if nSize <= nBufferSize) or an estimate otherwise. */
		template<typename Cont, typename Func>
		void append_buffer_allowing_nulls(Cont& cont, Func func) MAYTHROW {
			auto const nOffset = tc::size(cont);

#if defined(TC_PRIVATE) && defined(_DEBUG) && !defined(__clang__)
			tc::cont_reserve(cont, tc::size(cont)+1);
#endif

			for (;;) {
				tc::cont_extend(cont, cont.capacity(), boost::container::default_init); // Allow func to use the whole allocated buffer

				// sentinel to detect buffer overrun
				IF_NO_MSVC_WORKAROUND(static) constexpr typename boost::range_size<Cont>::type nSentinel= // workaround MSVC compiler bug: https://developercommunity.visualstudio.com/t/code-generation-bug-on-static-variable-i/10541326
#if defined(TC_PRIVATE) && defined(_DEBUG) && !defined(__clang__)
					1;
				_ASSERT(nOffset+nSentinel <= tc::size(cont));
				tc::for_each(tc::begin_next<tc::return_drop>(cont, nOffset), tc_fn(UNINITIALIZED));
#else
					0;
#endif

				auto const nNeededBufferSize = func(tc::ptr_begin(cont)+nOffset, tc::size(cont)-nOffset-nSentinel);
#if defined(TC_PRIVATE) && defined(_DEBUG) && !defined(__clang__)
				tc::assert_uninitialized(tc::back(cont));
#endif

				auto const nSize = nOffset + nNeededBufferSize;
				if (nSize+nSentinel <= tc::size(cont)) {
					_ASSERT(0 <= nSize);
					tc::take_first_inplace(cont, nSize);
					IF_TC_CHECKS(if (!tc::empty(cont)) _ASSERTINITIALIZED(tc::back(cont));)
					return;
				}

				tc::take_first_inplace(cont, nOffset);
				tc::cont_reserve(cont, nSize+nSentinel); // The container must grow bigger, but let cont_reserve decide by how much
			}
		}

		/* Calls func(pBuffer, nBufferSize) with buffers of increasing size; func may read into
		[pBuffer, pBuffer+nBufferSize) and return nSize, which is either the correct size of the
		buffer (if nSize <= nBufferSize) or an estimate otherwise. */
		template<typename Cont, typename Func>
		Cont get_buffer_allowing_nulls(Func&& func) MAYTHROW {
			static_assert(tc::decayed<Cont>);

			// sentinel to detect buffer overrun
#if defined(TC_PRIVATE) && defined(_DEBUG) && !defined(__clang__)
			typename no_adl::container_with_sentinel<Cont>::type
#else
			Cont
#endif
				cont;
			if (0 == cont.capacity()) {
				tc::cont_reserve(cont, 8);
			}

			append_buffer_allowing_nulls(cont, tc_move_if_owned(func));
			if constexpr (std::is_same<Cont, decltype(cont)>::value) {
				return cont;
			} else {
				tc_return_cast( tc_move(cont) );
			}
		}

		template<typename Cont, typename Func>
		Cont get_truncating_buffer_allowing_nulls(Func func) noexcept {
			return tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>([&](auto pBuffer, auto const nBufferSize) noexcept {
				auto nSize = func(pBuffer, nBufferSize);
				if (nSize == nBufferSize) {
					return nSize+1; // Any return value larger than nBufferSize causes a retry with a larger buffer
				} else {
					_ASSERT(nSize < nBufferSize);
					return nSize;
				}
			});
		}
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_truncating_buffer(Func&& func) noexcept {
		auto cont=tc::get_buffer_detail::get_truncating_buffer_allowing_nulls<Cont>(tc_move_if_owned(func));
		tc::assert_no_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_truncating_null_terminated_buffer(Func&& func) noexcept {
		auto cont=tc::get_buffer_detail::get_truncating_buffer_allowing_nulls<Cont>(tc_move_if_owned(func));
		tc::remove_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_buffer(Func&& func) MAYTHROW {
		auto cont=tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>(tc_move_if_owned(func)); // MAYTHROW
		tc::assert_no_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	void append_buffer(Cont& cont, Func&& func) MAYTHROW {
		IF_TC_CHECKS(auto const nOffset = tc::size(cont));
		tc::get_buffer_detail::append_buffer_allowing_nulls<Cont>(cont, tc_move_if_owned(func)); // MAYTHROW
		IF_TC_CHECKS(tc::assert_no_null_terminator(tc::begin_next<tc::return_drop>(cont, nOffset)));
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_null_terminated_buffer(Func&& func) MAYTHROW {
		auto cont=tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>(tc_move_if_owned(func)); // MAYTHROW
		static_assert( tc::char_type< tc::range_value_t<decltype((cont))> > );
		tc::remove_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_buffer_may_be_null_terminated(Func&& func) MAYTHROW {
		auto cont = tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>(tc_move_if_owned(func)); // MAYTHROW
		static_assert( tc::char_type< tc::range_value_t<decltype((cont))> > );
		tc::take_inplace(cont, tc::ends_with<tc::return_border_or_end>(cont, tc::single(tc::explicit_cast< tc::range_value_t<decltype((cont))> >('\0'))));
		tc::assert_no_null_terminator(cont);
		return cont;
	}


	template<typename... MultiIndexArgs, typename K, typename... ValueTypeCtorArgs >
	std::pair< tc::iterator_t<boost::multi_index::detail::hashed_index<MultiIndexArgs...>>, bool >
	multi_index_try_emplace_with_key(boost::multi_index::detail::hashed_index<MultiIndexArgs...>& hashed_index, K const& key, ValueTypeCtorArgs&& ... valuetypectorargs) MAYTHROW
	{
		if(auto it = tc::cont_find<tc::return_element_or_null>(hashed_index, key)) {
			return std::make_pair(tc_move(it), false);
		} else {
			return hashed_index.emplace(tc_move_if_owned(valuetypectorargs)...); // MAYTHROW
		}
	}

	template<typename... MultiIndexArgs, typename K, typename... ValueTypeCtorArgs >
	std::pair< tc::iterator_t<boost::multi_index::detail::ordered_index<MultiIndexArgs...>>, bool >
	multi_index_try_emplace_with_key(boost::multi_index::detail::ordered_index<MultiIndexArgs...>& ordered_index, K const& key, ValueTypeCtorArgs&& ... valuetypectorargs) MAYTHROW
	{
		auto it = ordered_index.lower_bound(key);
		if (tc::end(ordered_index)==it || ordered_index.key_comp()(key, ordered_index.key_extractor()(*it))) {
			return std::make_pair(
				tc::cont_must_emplace_before(ordered_index, tc_move(it), tc_move_if_owned(valuetypectorargs)...), // MAYTHROW
				true
			);
		} else {
			return std::make_pair(tc_move(it), false);
		}
	}

	template<typename... MultiIndexArgs, typename K>
	std::pair<tc::iterator_t<boost::multi_index::detail::hashed_index<MultiIndexArgs...>>, bool>
	map_query_cache(boost::multi_index::detail::hashed_index<MultiIndexArgs...>& hashed_index, K&& key) MAYTHROW {
		if (auto it = tc::cont_find<tc::return_element_or_null>(hashed_index, tc::as_const(key))) {
			return std::make_pair(tc_move(it), false);
		} else {
			return hashed_index.emplace(tc_move_if_owned(key)); // MAYTHROW
		}
	}

	template<typename Cont, typename Key>
	void cont_must_erase(Cont& cont, Key&& key) noexcept {
		VERIFYEQUAL( cont.erase(tc_move_if_owned(key)), 1u );
	}

	template<typename Cont, typename Key>
	bool cont_try_erase(Cont& cont, Key&& key) noexcept {
		switch_no_default(cont.erase(tc_move_if_owned(key))) {
			case 0: return false;
			case 1: return true;
		};
	}

	template< typename Cont, typename SubRng >
	void cont_erase_range(Cont& cont, SubRng const& rng) noexcept {
		cont.erase(tc::begin(rng), tc::end(rng));
	}

	/////////////////////////////////////////////////////
	// remove_count_erase

	template<typename Cont, typename Pred>
	[[nodiscard]] 
	typename tc::size_proxy< typename boost::range_size<Cont>::type > remove_count_erase_if(Cont& cont, Pred pred) noexcept {
		typename boost::range_size<Cont>::type count=0;
		tc::filter_inplace( cont, [&]( auto&& t ) noexcept ->bool {
			bool const b=tc_invoke(pred, tc_move_if_owned(t));
			count += (b ? 1 : 0);
			return !b;
		} );
		return tc::size_proxy< typename boost::range_size<Cont>::type >(count);
	}

	template<typename Cont, typename T>
	[[nodiscard]] 
	typename tc::size_proxy< typename boost::range_size<Cont>::type > remove_count_erase(Cont& cont, T const& t) noexcept {
		return remove_count_erase_if( cont, [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}

	/////////////////////////////////////////////////////
	// reverse_inplace
	// inplace algorithms should accept only lvalue containers, but reverse_inplace is called with
	// subranges of containers, so it must accept rvalues.

	template<typename Rng>
	constexpr void reverse_inplace(Rng&& rng) noexcept {
		if constexpr( has_mem_fn_reverse<std::remove_reference_t<Rng>> ) {
			rng.reverse();
		} else {
			std::reverse(tc::begin(rng), tc::end(rng));
		}
	}

	template<typename Rng, typename Less>
	[[nodiscard]] auto ordered_unique(Rng&& rng, Less less) noexcept code_return_decltype (
		_ASSERTDEBUG( tc::is_sorted( rng, less ) );,
		tc::adjacent_unique( tc_move_if_owned(rng), std::not_fn( tc_move(less) ) )
	)

	template<typename Rng>
	[[nodiscard]] auto ordered_unique(Rng&& rng) return_decltype_noexcept(
		tc::ordered_unique( tc_move_if_owned(rng), tc::fn_less() )
	)

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sort_inplace_unique(Rng&& rng, Less less = Less()) noexcept code_return_decltype(
		static_assert( !std::is_reference<Rng>::value );
		tc::sort_inplace( rng, less );,
		tc::ordered_unique( tc_move_if_owned(rng), tc_move(less) )
	)

	template<typename Rng, typename Less>
	[[nodiscard]] auto sort_unique(Rng&& rng, Less less) return_decltype_noexcept(
		tc::ordered_unique(tc::sort(tc_move_if_owned(rng), less), less)
	)

	template<typename Rng>
	[[nodiscard]] auto sort_unique(Rng&& rng) return_decltype_noexcept(
		sort_unique(tc_move_if_owned(rng), tc::fn_less())
	)

	template<typename Rng, typename Less=tc::fn_less>
	[[nodiscard]] constexpr auto constexpr_sort_unique(Rng&& rng, Less&& less=Less()) noexcept {
		return tc_modified(
			tc::make_static_vector<tc::constexpr_size<Rng>()>(tc_move_if_owned(rng)),
			tc::sort_unique_inplace(_, tc_move_if_owned(less))
		);
	}

	namespace find_first_or_unique_adl {
		template< typename RangeReturn, IF_TC_CHECKS(bool c_bCheckUnique,) typename T, T... t, typename U >
		[[nodiscard]] constexpr decltype(auto) find_first_or_unique_impl(adl_tag_t, IF_TC_CHECKS(tc::constant<c_bCheckUnique>,) std::integer_sequence<T, t...>, U const& u) noexcept {
#ifdef _CHECKS
			if constexpr (c_bCheckUnique) {
				// This assert is stronger than the usual find_unique precondition, which allows duplicates as long as they are not searched for.
				static_assert( tc::is_strictly_sorted(tc::constexpr_sort(tc::make_array(tc::aggregate_tag, t...))) );
			}
#endif
			return ((tc::equal_to(t, u)) || ...);
		}
	}

	template< typename RangeReturn, typename SetType, typename T, typename Less >
	void binary_find_unique(tc::unordered_set<SetType> const& rng, T const& t, Less less) noexcept = delete;

	namespace binary_find_first_or_unique_adl {
		template< typename RangeReturn, bool c_bAssertUniqueness, typename Rng, typename T, typename Pred >
		[[nodiscard]] decltype(auto) binary_find_first_or_unique(Rng&& rng, T const& t, Pred predLessOr3way) noexcept {
			// The result of tc::binary_find_unique must be unambiguous. In general, this means that rng is strictly
			// ordered. In some cases, it is convenient to allow multiple occurrences of the same item in
			// rng, which is not a problem as long as these items are not searched for.

			// preserve order of arguments for 3way predicates
			auto const c_b3way = tc::is_comparison_category<decltype(predLessOr3way(tc::front(rng), t))>; // tc_static_auto_constexpr_capture causes ICE
			auto it=[&]() noexcept {
				if constexpr(c_b3way) {
					_ASSERTDEBUG( tc::is_sorted(rng, tc::lessfrom3way(std::ref(predLessOr3way))) );
					return tc::lower_bound<tc::return_border>( rng, t, tc::lessfrom3way(std::ref(predLessOr3way)) );
				} else {
					_ASSERTDEBUG( tc::is_sorted(rng, predLessOr3way) );
					return tc::lower_bound<tc::return_border>( rng, t, std::ref(predLessOr3way) );
				}
			}();
			if( it==tc::end( rng ) ) {
				return RangeReturn::pack_no_element(tc_move_if_owned(rng));
			} else {
				auto const Greater = [&](auto const& elem) noexcept {
					if constexpr(c_b3way) {
						return std::is_gt(predLessOr3way(elem, t));
					} else {
						return predLessOr3way(t, elem);
					}
				};
				auto && ref=*it;
				if (Greater(tc::as_const(ref))) {
					return RangeReturn::pack_no_element(tc_move_if_owned(rng));
				} else {
#ifdef _CHECKS
					if constexpr(c_bAssertUniqueness) {
						auto itNext = tc_modified(it, ++_);
						_ASSERT( tc::end(rng)==itNext || Greater(tc::as_const(*itNext)));
					}
#endif
					return RangeReturn::pack_element(it,tc_move_if_owned(rng),tc_move_if_owned(ref));
				}
			}
		}
	}
	
	template< typename RangeReturn, typename Rng, typename T, typename Pred >
	[[nodiscard]] decltype(auto) binary_find_unique(Rng&& rng, T const& t, Pred predLessOr3way) noexcept {
		return binary_find_first_or_unique_adl::binary_find_first_or_unique<RangeReturn, /*c_bAssertUniqueness*/ true>(tc_move_if_owned(rng), t, tc_move(predLessOr3way));
	}
	
	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] decltype(auto) binary_find_unique(Rng&& rng, T const& t) noexcept {
		return tc::binary_find_unique<RangeReturn>( tc_move_if_owned(rng), t, tc::fn_less() );
	}

	template< typename RangeReturn, typename Rng, typename T, typename Pred >
	[[nodiscard]] decltype(auto) binary_find_first(Rng&& rng, T const& t, Pred predLessOr3way) noexcept {
		return binary_find_first_or_unique_adl::binary_find_first_or_unique<RangeReturn, /*c_bAssertUniqueness*/ false>(tc_move_if_owned(rng), t, tc_move(predLessOr3way));
	}

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] decltype(auto) binary_find_first(Rng&& rng, T const& t) noexcept {
		return tc::binary_find_first<RangeReturn>( tc_move_if_owned(rng), t, tc::fn_less() );
	}

	// would be cleaner to search on the distance metric (starting with lower_bound(rng,0)),
	// but subtraction may cause unnecessary overflows
	template< typename Rng, typename T >
	[[nodiscard]] auto binary_closest(Rng&& rng, T const& t) noexcept {
		auto it = tc::lower_bound<tc::return_border>(rng, t);
		if( tc::begin(rng)==it ) {
			return it;
		} else if( tc::end(rng)==it ) {
			return tc_modified(it, --_);
		} else {
			auto itPrior = tc_modified(it, --_);
			return (t - *itPrior) < (*it - t) ? itPrior : it;
		}
	}

	template< typename RngA, typename RngB, typename Comp, typename FuncElementA, typename FuncElementB, typename FuncElementBoth > 
	auto interleave_may_remove_current_iterator(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB, FuncElementBoth fnElementBoth) noexcept -> tc::common_type_t<
		decltype(tc::continue_if_not_break(fnElementA, tc::begin(rngA), tc::begin(rngB))),
		decltype(tc::continue_if_not_break(fnElementB, tc::begin(rngA), tc::begin(rngB))),
		decltype(tc::continue_if_not_break(fnElementBoth, tc::begin(rngA), tc::begin(rngB)))
	> {
		auto itA=tc::begin(rngA);
		auto itEndA=tc::end(rngA);
		auto itB=tc::begin(rngB);
		auto itEndB=tc::end(rngB);
		if( itA==itEndA ) goto endA;
		if( itB==itEndB ) goto endB;
		for(;;) {
			if(tc_auto_cref(order, comp( tc::as_const(*itA), tc::as_const(*itB) )); std::is_lt(order)) {
				tc_return_if_break(tc::continue_if_not_break(fnElementA, itA++, itB))
				if( itA==itEndA ) goto endA;
			} else if(tc::is_eq(order)) {
				tc_return_if_break(tc::continue_if_not_break(fnElementBoth, itA++, itB++))
				if( itA==itEndA ) goto endA;
				if( itB==itEndB ) goto endB;
			} else {
				_ASSERTDEBUG(std::is_gt(order));
				tc_return_if_break(tc::continue_if_not_break(fnElementB, itA, itB++))
				if( itB==itEndB ) goto endB;
			}
		}
	endB:
		while (itA != itEndA) tc_return_if_break(tc::continue_if_not_break(fnElementA, itA++, itEndB))
		tc_return_cast(tc::continue_);
	endA:
		while(itB != itEndB) tc_return_if_break(tc::continue_if_not_break(fnElementB, itEndA, itB++))
		tc_return_cast(tc::continue_);
	}

	template< typename RngA, typename RngB, typename Comp, typename FuncElementA, typename FuncElementB, typename FuncElementBoth >
	auto interleave_may_remove_current(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB, FuncElementBoth fnElementBoth) noexcept {
		return interleave_may_remove_current_iterator(rngA, rngB, comp,
			[&](auto const& lhs, tc::unused /*rhs*/) noexcept { return tc_invoke(fnElementA, *lhs); },
			[&](tc::unused /*lhs*/, auto const& rhs) noexcept { return tc_invoke(fnElementB, *rhs); },
			[&](auto const& lhs, auto const& rhs) noexcept { return tc_invoke(fnElementBoth, *lhs, *rhs); }
		);
	}

	namespace interleave_2_detail {
		DEFINE_TAG_TYPE(lhs_tag)
		DEFINE_TAG_TYPE(rhs_tag)
		DEFINE_TAG_TYPE(lhsrhs_tag)

		namespace no_adl {
			// MSVC (from 15.8 to 17.0.2) sometimes crashes when this is inlined as a lambda in interleave_2.
			template<typename RngRhs, typename Comp, typename Sink>
			struct interleave_2_sink {
				tc::iterator_t<RngRhs>& m_itrhs;
				tc::sentinel_t<RngRhs> const& m_endrhs;
				Comp const m_comp;
				Sink const& m_sink;

				template<typename Lhs>
				constexpr auto operator()(Lhs&& lhs) const& MAYTHROW -> tc::common_type_t<
					decltype(tc::continue_if_not_break(m_sink, lhs_tag, std::declval<Lhs>())),
					decltype(tc::continue_if_not_break(m_sink, rhs_tag, *m_itrhs)),
					decltype(tc::continue_if_not_break(m_sink, lhsrhs_tag, std::declval<Lhs>(), *m_itrhs)),
					tc::constant<tc::continue_>
				> {
					for (;;) {
						if( m_itrhs == m_endrhs ) {
							return tc::continue_if_not_break(m_sink, lhs_tag, tc_move_if_owned(lhs));
						} else {
							decltype(auto) rhs = *m_itrhs;
							if(auto const order=tc_invoke(m_comp, tc::as_const(lhs), tc::as_const(rhs) ); std::is_lt(order)) {
								return tc::continue_if_not_break(m_sink, lhs_tag, tc_move_if_owned(lhs));
							} else if (std::is_gt(order)) {
								tc_return_if_break(tc::continue_if_not_break(m_sink, rhs_tag, tc_move_if_owned(rhs)))
								++m_itrhs;
							} else {
								tc_return_if_break(tc::continue_if_not_break(m_sink, lhsrhs_tag, tc_move_if_owned(lhs), tc_move_if_owned(rhs)))
								++m_itrhs;
								return tc::constant<tc::continue_>();
							}
						}
					}
				}
			};

			template<typename SinkLhs, typename SinkRhs, typename SinkLhsRhs>
			struct SDemultiplexByTagSink { // MSVC workaround: not a lambda for shorter symbol names
				SinkLhs const m_sinklhs;
				SinkRhs const m_sinkrhs;
				SinkLhsRhs const m_sinklhsrhs;
				template<typename... Args>
				constexpr auto operator()(lhs_tag_t, Args&&... args) const& return_decltype_MAYTHROW( m_sinklhs(tc_move_if_owned(args)...) )
				template<typename... Args>
				constexpr auto operator()(rhs_tag_t, Args&&... args) const& return_decltype_MAYTHROW( m_sinkrhs(tc_move_if_owned(args)...) )
				template<typename... Args>
				constexpr auto operator()(lhsrhs_tag_t, Args&&... args) const& return_decltype_MAYTHROW( m_sinklhsrhs(tc_move_if_owned(args)...) )
			};

			template<typename Sink>
			struct SExchangedRangeSink { // MSVC workaround: not a lambda for shorter symbol names
				Sink const m_sink;
				template<typename Rhs>
				constexpr auto operator()(lhs_tag_t, Rhs&& rhs) const& return_decltype_MAYTHROW( tc_invoke(m_sink, rhs_tag, tc_move_if_owned(rhs)) )
				template<typename Lhs>
				constexpr auto operator()(rhs_tag_t, Lhs&& lhs) const& return_decltype_MAYTHROW( tc_invoke(m_sink, lhs_tag, tc_move_if_owned(lhs)) )
				template<typename Rhs, typename Lhs>
				constexpr auto operator()(lhsrhs_tag_t, Rhs&& rhs, Lhs&& lhs) const& return_decltype_MAYTHROW( tc_invoke(m_sink, lhsrhs_tag, tc_move_if_owned(lhs), tc_move_if_owned(rhs)) )
			};

			template<typename Comp>
			struct SExchangedRangeComp { // MSVC workaround: not a lambda for shorter symbol names
				Comp const m_comp;
				constexpr auto operator()(auto const& rhs, auto const& lhs) const& MAYTHROW {
					return tc::negate(tc_invoke(m_comp, lhs, rhs));
				}
			};
		}

		template< typename RngLhs, tc::range_with_iterators RngRhs, typename Comp, typename Sink>
			requires tc::prefers_for_each<RngLhs> || (!tc::prefers_for_each<RngRhs>)
		constexpr auto internal_interleave_2(RngLhs&& rnglhs, RngRhs&& rngrhs, Comp&& comp, Sink const sink) MAYTHROW {
			auto itrhs=tc::begin(rngrhs);
			auto endrhs=tc::end(rngrhs);

			tc_return_if_break(tc::for_each(
				rnglhs,
				no_adl::interleave_2_sink<RngRhs, decay_t<Comp>, Sink>{itrhs, endrhs, tc_move_if_owned(comp), sink}
			));

			while (itrhs != endrhs) {
				tc_return_if_break(tc::continue_if_not_break(sink, rhs_tag, *itrhs))
				++itrhs;
			}
			return tc::implicit_cast<decltype(tc::for_each(rnglhs,
				std::declval<no_adl::interleave_2_sink<RngRhs, tc::decay_t<Comp>, Sink>>()
			))>(tc::constant<tc::continue_>());
		}

		template< tc::range_with_iterators RngLhs, tc::prefers_for_each RngRhs, typename Comp, typename Sink>
			requires (!tc::prefers_for_each<RngLhs>)
		constexpr auto internal_interleave_2(RngLhs&& rnglhs, RngRhs&& rngrhs, Comp&& comp, Sink&& sink) MAYTHROW {
			return interleave_2_detail::internal_interleave_2(
				tc_move_if_owned(rngrhs),
				tc_move_if_owned(rnglhs),
				no_adl::SExchangedRangeComp<tc::decay_t<Comp>>{tc_move_if_owned(comp)},
				no_adl::SExchangedRangeSink<tc::decay_t<Sink>>{tc_move_if_owned(sink)}
			);
		}
	}

	template<typename SinkLhs, typename SinkRhs, typename SinkLhsRhs>
	constexpr auto interleave_2(auto&& rnglhs, auto&& rngrhs, auto&& comp, SinkLhs&& sinklhs, SinkRhs&& sinkrhs, SinkLhsRhs&& sinklhsrhs) MAYTHROW  {
		return interleave_2_detail::internal_interleave_2(
			tc_move_if_owned(rnglhs),
			tc_move_if_owned(rngrhs),
			tc_move_if_owned(comp),
			interleave_2_detail::no_adl::SDemultiplexByTagSink<tc::decay_t<SinkLhs>, tc::decay_t<SinkRhs>, tc::decay_t<SinkLhsRhs>>{
				tc_move_if_owned(sinklhs),
				tc_move_if_owned(sinkrhs),
				tc_move_if_owned(sinklhsrhs)
			}
		);
	}

	namespace no_adl {
		template<typename Compare>
		struct SInterleaveImpl {
			tc::decay_t<Compare> m_compare;

			bool HasBetterElement(bool* const, tc::unused) const& noexcept {
				return false;
			}

			template<
				typename PairItItBest,
				typename PairItIt0,
				typename... Args
			>
			bool HasBetterElement(bool* const itb, PairItItBest const& argBest, PairItIt0 const& pairitit0, Args const&... args) const& noexcept {
				if (pairitit0.first != pairitit0.second) {
					if(tc_auto_cref(order, m_compare(tc::as_const(*argBest.first), tc::as_const(*pairitit0.first))); std::is_lt(order)) {
						*itb = false;
						return HasBetterElement(tc_modified(itb, ++_), argBest, args...);
					} else if(tc::is_eq(order)) {
						bool b = HasBetterElement(tc_modified(itb, ++_), argBest, args...);
						*itb = !b;
						return b;
					} else {
						_ASSERTDEBUG(std::is_gt(order));
						*itb = !HasBetterElement(tc_modified(itb, ++_), pairitit0, args...);
						return true;
					}
				} else {
					*itb = false;
					return HasBetterElement(tc_modified(itb, ++_), argBest, args...);
				}
			}

			bool FindBest(bool* const) const& {
				return false;
			}

			template<
				typename PairItIt0,
				typename... Args
			>
			bool FindBest(bool* const itb, PairItIt0 const& pairitit0, Args const&... args) const& {
				if (pairitit0.first != pairitit0.second) {
					*itb = !HasBetterElement(tc_modified(itb, ++_), pairitit0, args...);
					return true;
				} else {
					*itb = false;
					return FindBest(tc_modified(itb, ++_), args...);
				}
			}

			SInterleaveImpl(Compare&& compare) noexcept
				: m_compare(tc_move_if_owned(compare))
			{}

			template<
				typename Func,
				std::size_t... I,
				typename... PairItIt
			>
			tc::break_or_continue operator()(Func func, std::index_sequence<I...>, PairItIt... pairitit) const noexcept {
				bool ab[sizeof...(PairItIt)];

				while (FindBest(tc::begin(ab), pairitit...)) {
					tc_return_if_break(tc::continue_if_not_break(func, std::make_pair(pairitit.first, tc::at(ab, I))...  ));

					([](auto& it, bool const b) noexcept {if (b) ++it;}(pairitit.first, tc::at(ab,I)), ...);
				}
				return tc::continue_;
			}
		};
	}

	template<
		typename Compare,
		typename Func,
		typename... Rng
	>
	tc::break_or_continue interleave_n(Compare&& compare, Func&& func, Rng&&... rng) noexcept {
		return no_adl::SInterleaveImpl<Compare>(tc_move_if_owned(compare))(
			tc_move_if_owned(func),
			std::index_sequence_for<Rng...>(),
			std::make_pair(
				tc::begin(rng),
				tc::end(rng)
			)...
		);
	}

	template <typename RngRng>
	[[nodiscard]] auto common_prefix(RngRng&& rngrng) noexcept {
		auto&& rngFront = tc::front(rngrng);
		return tc::accumulate(
			tc::begin_next<tc::return_drop>(rngrng),
			tc::take(tc_move_if_owned(rngFront), tc::end(rngFront)),
			[&](auto& rngResult, auto const& rng) noexcept {
				tc::take_inplace(rngResult, boost::mismatch(rngResult, rng).first);
			}
		);
	}

	template< typename T, typename Rng >
	[[nodiscard]] auto make_variant_range_filter(Rng&& rng) noexcept {
		return tc::transform( 
			tc::filter( 
				tc_move_if_owned(rng), 
				tc_fn(std::holds_alternative<T>)
			),
			tc_fn(tc::get<T>)
		);
	}

	template< typename RangeReturn, typename Rng, typename T> requires (!RangeReturn::requires_iterator)
	[[nodiscard]] constexpr decltype(auto) linear_at(Rng&& rng, T n) noexcept {
		return tc::find_first_if<RangeReturn>(tc_move_if_owned(rng), [&](tc::unused) noexcept {
			if(0==n) {
				return true;
			} else {
				--n;
				return false;
			}
		});
	}

	// Create an infinite range by repeatedly applying funcIterate to t
	template <typename T, typename FuncIterate>
	auto iterate(T&& t, FuncIterate&& funcIterate) noexcept {
		return tc::generator_range_output<tc::decay_t<T> const&>([funcIterate=tc::make_reference_or_value(tc_move_if_owned(funcIterate)),t_=tc::make_reference_or_value(tc_move_if_owned(t))](auto func) noexcept {
			auto t = *t_;
			tc_return_if_break(tc::continue_if_not_break(func,tc::as_const(t)))
			for (;;) {
				tc_invoke(*funcIterate, t);
				tc_return_if_break(tc::continue_if_not_break(func,tc::as_const(t)))
			}
		});
	}

	template<typename RngSep, typename... Rngs>
	decltype(auto) concat_nonempty_with_separator(RngSep&& rngSep, Rngs&&... rngs) noexcept {
		return tc::join_with_separator(
			tc_move_if_owned(rngSep),
			tc::filter(tc::make_range(tc_move_if_owned(rngs)...), std::not_fn(tc_fn(tc::empty)))
		);
	}

	template<typename RngSep, typename Rng0, typename... Rngs>
	decltype(auto) concat_with_separator(RngSep&& rngSep, Rng0&& rng0, Rngs&&... rngs) noexcept {
		return tc::concat(tc_move_if_owned(rng0), tc::concat(/*copy if needed*/tc::implicit_cast<RngSep>(rngSep), tc_move_if_owned(rngs))...);
	}

	template<typename Rng, typename Val>
	void fill(Rng&& rng, Val const& value) noexcept {
		tc::for_each(tc_move_if_owned(rng), [&](auto&& element) noexcept {
			tc_move_if_owned(element) = value;
		});
	}

}
