
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
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
	[[nodiscard]] constexpr bool is_sorted(Rng const& rng, Less less = Less()) noexcept {
		return !tc::any_of(tc::adjacent<2>(rng), [&](auto const& first, auto const& second) noexcept {
			return less(second, first);
		});
	}

	template< typename Rng, typename Equal = tc::fn_equal_to >
	[[nodiscard]] constexpr bool all_same(Rng const& rng, Equal equal = Equal()) noexcept {
		if constexpr( tc::range_with_iterators<Rng> ) {
			auto const itBegin=tc::begin(rng);
			auto const itEnd=tc::end(rng);
			if(itBegin==itEnd) return true;
			auto const itNext=tc_modified(itBegin, ++_);
			if(itNext==itEnd) return true;
			tc_auto_cref( front, *itBegin );
			return all_of(
				tc::drop(rng, itNext),
				[&](auto const& _) noexcept { return equal(front, _); }
			);
		} else {
			std::optional<tc::range_value_t<Rng const&>> oFirst;
			return tc::continue_ == tc::for_each(rng, [&](auto&& val) noexcept {
				if (oFirst) {
					return tc::continue_if(equal(*oFirst, val));
				} else {
					oFirst.emplace(tc_move_if_owned(val));
					return tc::continue_;
				}
			});
		}
	}

	/////////////////////////////////
	// associative containers

	namespace explicit_convert_adl {
		template<typename TTarget, typename Rng>
			requires has_mem_fn_lower_bound<TTarget> || has_mem_fn_hash_function<TTarget>
		TTarget explicit_convert_impl(adl_tag_t, tc::type::identity<TTarget>, Rng&& rng) noexcept {
			TTarget cont;
			// force each element to be inserted
			tc::cont_must_insert_range(cont, std::forward<Rng>(rng));
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
	constexpr void sort_inplace(Rng&& rng, Less&& less = Less()) noexcept {
		if constexpr( has_mem_fn_sort< Rng >) {
			static_assert( std::is_lvalue_reference<Rng>::value );
			rng.sort( std::forward<Less>(less) );
		} else {
#ifdef __clang__ // xcode12 does not support constexpr std::sort
			if(std::is_constant_evaluated()) {
				constexpr_sort_inplace_detail::constexpr_sort_inplace(tc::begin(rng), tc::end(rng), less);
				_ASSERTE( tc::is_sorted(rng, less) );
			} else {
#endif
				std::sort( tc::begin(rng), tc::end(rng), std::forward<Less>(less) );
#ifdef __clang__
			}
#endif
		}
	}

	template<typename Rng, typename Less = tc::fn_less>
	void stable_sort_inplace(Rng&& rng, Less&& less = Less()) noexcept {
		std::stable_sort(tc::begin(rng), tc::end(rng), std::forward<Less>(less));
	}

	namespace no_adl {
		template<typename Rng, bool bStable>
		struct [[nodiscard]] sorted_index_adaptor final
			: tc::range_iterator_from_index<
				sorted_index_adaptor<Rng, bStable>,
				tc::iterator_t<tc::vector<tc::index_t<std::remove_reference_t<Rng>>> const>
			>
			, tc::range_adaptor_base_range<Rng>
			, private tc::nonmovable // disable copy ctor and default move ctor
		{
		private:
			using this_type = sorted_index_adaptor;

			tc::vector<tc::index_t<std::remove_reference_t<Rng>>> m_vecidx;

		public:
			using typename this_type::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex=false;

			using difference_type = typename decltype(m_vecidx)::difference_type;

			template<typename LessOrComp>
			explicit sorted_index_adaptor(Rng&& rng, LessOrComp lessorcomp) noexcept
				: tc::range_adaptor_base_range<Rng>(tc::aggregate_tag, std::forward<Rng>(rng))
			{
				if constexpr (tc::has_size<Rng>) {
					tc::cont_reserve(m_vecidx, tc::size(this->base_range()));
				}
				for(auto idx=this->base_begin_index(); !tc::at_end_index(this->base_range(), idx); tc::increment_index(this->base_range(), idx)) {
					tc::cont_emplace_back(m_vecidx, idx);
				}
				tc::sort_inplace(
					m_vecidx,
					[&](auto const& idxLhs, auto const& idxRhs ) noexcept -> bool {
						tc_auto_cref(lhs, tc::dereference_index(this->base_range(), idxLhs));
						tc_auto_cref(rhs, tc::dereference_index(this->base_range(), idxRhs));
						if constexpr (bStable) {
							static_assert(tc::random_access_range<Rng>);
							tc_auto_cref(order, lessorcomp(lhs, rhs));
							static_assert(tc::is_comparison_category<std::remove_cvref_t<decltype(order)>>);
							if(tc::is_eq(order)) {
								return 0<tc::distance_to_index(this->base_range(), idxLhs, idxRhs);
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

			sorted_index_adaptor(sorted_index_adaptor&& rng) noexcept requires tc::is_index_valid_for_move_constructed_range<Rng>::value // reference_or_value is movable for const Rng as well
				: range_adaptor_base_range<Rng>(tc_move(rng))
				, m_vecidx(tc_move(rng).m_vecidx)
			{
			}
		private:
			STATIC_FINAL(begin_index)() const& noexcept -> tc_index {
				return tc::begin(m_vecidx);
			}

			STATIC_FINAL(end_index)() const& noexcept -> tc_index {
				return tc::end(m_vecidx);
			}

			STATIC_FINAL(dereference_index)(tc_index const& idx) const& return_decltype_MAYTHROW(
				tc::dereference_index(this->base_range(), *idx)
			)

			STATIC_FINAL(dereference_index)(tc_index const& idx) & return_decltype_MAYTHROW(
				tc::dereference_index(this->base_range(), *idx)
			)

			STATIC_FINAL(increment_index)(tc_index& idx) const& noexcept -> void {
				++idx;
			}

			STATIC_FINAL(decrement_index)(tc_index& idx) const& noexcept -> void {
				--idx;
			}

			STATIC_FINAL(advance_index)(tc_index& idx, difference_type d) const& noexcept -> void {
				idx+=d;
			}

			STATIC_FINAL(distance_to_index)(tc_index const& idxLhs, tc_index const& idxRhs) const& noexcept -> difference_type {
				return idxRhs-idxLhs;
			}
		public:
			static decltype(auto) element_base_index(tc_index const& idx) noexcept {
				return *idx;
			}
			static decltype(auto) element_base_index(tc_index&& idx) noexcept {
				return *tc_move(idx);
			}

			constexpr decltype(auto) dereference_untransform(tc_index const& idx) const& noexcept {
				return this->base_range().dereference_untransform(*idx);
			}
		};
	}
	using no_adl::sorted_index_adaptor;

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sorted_iterator_range(Rng& rng, Less&& less = Less()) noexcept {
		auto vecitSorted=tc::make_vector( tc::make_range_of_iterators(rng) );
		tc::sort_inplace(vecitSorted, tc::projected( std::forward<Less>(less), tc::fn_indirection() ) );
		return vecitSorted;
	}

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sort(Rng&& rng, Less&& less = Less()) noexcept {
		return tc::sorted_index_adaptor<Rng, false/*bStable*/>(std::forward<Rng>(rng), std::forward<Less>(less));
	}

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] constexpr auto constexpr_sort(Rng&& rng, Less&& less = Less()) noexcept {
		auto a = tc::make_array(std::forward<Rng>(rng));
		tc::sort_inplace(a, std::forward<Less>(less));
		return a;
	}

	template<typename Rng, typename Comp = tc::fn_compare>
	[[nodiscard]] auto stable_sort(Rng&& rng, Comp&& comp = Comp()) noexcept {
		return tc::sorted_index_adaptor<Rng, true/*bStable*/>(std::forward<Rng>(rng), std::forward<Comp>(comp));
	}
	
	namespace no_adl {
		template< typename Rng >
		struct [[nodiscard]] untransform_adaptor
			:  tc::index_range_adaptor<
				untransform_adaptor<Rng>,
				Rng
			>
		{
		private:
			using base_ = tc::index_range_adaptor<untransform_adaptor<Rng>, Rng>;

		public:
			using typename base_::tc_index;
			using base_::base_;

			constexpr decltype(auto) STATIC_VIRTUAL_METHOD_NAME(dereference_index)(tc_index const& idx) & MAYTHROW {
				return this->base_range().dereference_untransform(idx);
			}

			constexpr decltype(auto) STATIC_VIRTUAL_METHOD_NAME(dereference_index)(tc_index const& idx) const& MAYTHROW {
				return this->base_range().dereference_untransform(idx);
			}
		};
	}

	template<typename Rng>
	[[nodiscard]] auto untransform(Rng&& rng) noexcept {
		return no_adl::untransform_adaptor<Rng>(tc::aggregate_tag, std::forward<Rng>(rng));
	}

	///////////////////////////////////////
	// partition ranges into subranges

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto ordered_unique_range(Rng&& rng, Less&& less = Less()) noexcept {
		_ASSERTDEBUG( tc::is_sorted( rng, less ) );
		return tc::adjacent_unique_range( std::forward<Rng>(rng), std::not_fn( std::forward<Less>(less) ) );
	}

	template<typename Rng, typename FuncRngStart, typename FuncRngElement, typename Less = tc::fn_less>
	auto generator_ordered_unique_range(Rng&& rng, FuncRngStart funcStart, FuncRngElement funcElem, Less&& less = Less()) noexcept {
		std::optional<tc::range_value_t<Rng>> oelem;
		tc::for_each(std::forward<Rng>(rng), [&](auto&& element) noexcept {
			if (!oelem || less(*oelem, element)) {
				tc_yield(funcStart);
				tc::optional_emplace(oelem, element);
			}
			return tc::continue_if_not_break(funcElem,tc_move_if_owned(element));
		});
	}

	template<typename Rng, typename Comp = tc::fn_compare>
	[[nodiscard]] auto stable_sort_unique_range(Rng&& rng, Comp const& comp = Comp()) noexcept {
		return tc::ordered_unique_range( tc::stable_sort( std::forward<Rng>(rng), comp ), tc::lessfrom3way(comp) );
	}

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sort_unique_range(Rng&& rng, Less const& less = Less()) noexcept {
		return tc::ordered_unique_range( tc::sort( std::forward<Rng>(rng), less ), less );
	}

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sort_inplace_unique_range(Rng&& rng, Less&& less = Less()) noexcept {
		static_assert( !std::is_reference<Rng>::value );
		tc::sort_inplace( rng, std::ref(less) );
		return tc::ordered_unique_range( std::forward<Rng>(rng), std::forward<Less>(less) );
	}

	template< typename Rng, typename Less, typename Accu >
	void sort_accumulate_each_unique_range(Rng&& cont, Less less, Accu accu) noexcept {
		tc::sort_inplace( cont, less );
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
					tc::invoke(accu,*tc::begin(rngEqualSubRange), *it);
				}
				rngfilter.keep( tc::begin(rngEqualSubRange) );
			}
		);
	}

	template< typename Cont, typename Equals = tc::fn_equal_to >
	void front_unique_inplace(Cont & cont, Equals&& pred=Equals()) noexcept {
		tc::range_filter< tc::decay_t<Cont> > rngfilter(cont);
		tc::for_each(
			tc::transform(
				tc::front_unique_range(cont, std::forward<Equals>(pred)),
				tc_fn(tc::begin) // fn_boost_begin does not work, need subrange as lvalue to get non-const iterator
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
	template< typename Cont, typename Equals=tc::fn_equal_to >
	constexpr void adjacent_unique_inplace(Cont & cont, Equals&& pred=Equals()) MAYTHROW {
		tc::range_filter< tc::decay_t<Cont> > rngfilter(cont);
		// When this function is evaluated at compile time, the range returned by tc::make_range_of_iterators cannot use an rvalue as a base range.
		// This is because it stores the base range in a mutable field inside tc::reference_or_value.
		tc::for_each(
			tc::may_remove_current(tc::make_range_of_iterators(tc::as_lvalue(tc::adjacent_unique(cont, std::forward<Equals>(pred/*MAYTHROW*/))))),
			[&](auto it) noexcept {
				rngfilter.keep(it.element_base());
			}
		);
	}

	template<typename Cont, typename Less=tc::fn_less>
	constexpr void ordered_unique_inplace( Cont& cont, Less less=Less() ) noexcept {
		_ASSERTDEBUG( tc::is_sorted( cont, less ) );
		tc::adjacent_unique_inplace( cont, std::not_fn( tc_move(less) ) );
	}

	template< typename Cont, typename Less=tc::fn_less >
	constexpr void sort_unique_inplace(Cont& cont, Less less=Less()) noexcept {
		tc::sort_inplace( cont, less );
		tc::ordered_unique_inplace( cont, tc_move(less) );
	}

	template< typename Cont, typename Less=tc::fn_less >
	void stable_sort_unique_inplace(Cont& cont, Less less=Less()) noexcept {
		tc::stable_sort_inplace( cont, less );
		tc::ordered_unique_inplace( cont, tc_move(less) );
	}

	template<typename Rng, typename Less, typename Func>
	auto ordered_for_each_occurrence(Rng&& rng, Less&& less, Func func) noexcept {
		return tc::for_each(tc::ordered_unique_range( std::forward<Rng>(rng), std::forward<Less>(less)), [&](auto const& rngSub) noexcept {
			return tc::continue_if_not_break( func, std::make_pair(
				tc::begin(rngSub),
				tc::implicit_cast<typename boost::range_size< std::remove_reference_t<Rng> >::type >(tc::size_linear(rngSub))
			) );
		});
	}

	template<typename RangeReturn, typename Rng>
	[[nodiscard]] auto plurality_element(Rng&& rng) noexcept {
		if(tc::empty(rng)) {
			return RangeReturn::pack_no_element(std::forward<Rng>(rng));
		}
		auto const rng2=tc::sorted_iterator_range(rng, tc::fn_less());

		auto it = *( tc::accumulate(
			[&](auto const& func) noexcept {
				return tc::ordered_for_each_occurrence(rng2, tc::projected(tc::fn_less(), fn_indirection()), func);
			},
			std::pair<
				tc::iterator_t<decltype(rng2)>,
				typename boost::range_size<decltype(rng2)>::type
			>(), // value-initialized, second=0
			tc::fn_assign_better(tc::projected(tc::fn_greater(), tc_member(.second)))
		).first );
		return RangeReturn::pack_element(it, std::forward<Rng>(rng));
	}

	template< typename RangeReturn, typename Rng, typename Pred >
	[[nodiscard]] auto trim_left_if(Rng&& rng, Pred&& pred) MAYTHROW code_return_decltype_xvalue_by_ref(
		static_assert( RangeReturn::allowed_if_always_has_border );,
		RangeReturn::pack_border( tc::find_first_if<tc::return_border_before_or_end>( rng, std::not_fn(std::forward<Pred>(pred)) ), std::forward<Rng>(rng))
	)

	template< typename RangeReturn, typename Rng, typename Pred >
	[[nodiscard]] auto trim_right_if(Rng&& rng, Pred&& pred) MAYTHROW code_return_decltype_xvalue_by_ref(
		static_assert( RangeReturn::allowed_if_always_has_border );,
		RangeReturn::pack_border( tc::find_last_if<tc::return_border_after_or_begin>( rng, std::not_fn(std::forward<Pred>(pred)) ), std::forward<Rng>(rng))
	)

	template< typename Rng, typename Pred >
	[[nodiscard]] decltype(auto) trim_if(Rng&& rng, Pred&& pred) MAYTHROW {
		auto rngTrimmed = tc::trim_right_if<tc::return_take>( std::forward<Rng>(rng), pred );
		return tc::trim_left_if<tc::return_drop>( tc_move(rngTrimmed), std::forward<Pred>(pred) );
	}

	template< typename RangeReturn, typename Rng, typename RngTrim >
	[[nodiscard]] decltype(auto) trim_left(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_left_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< typename RangeReturn, typename Rng, typename RngTrim >
	[[nodiscard]] decltype(auto) trim_right(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_right_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< typename Rng, typename RngTrim >
	[[nodiscard]] decltype(auto) trim(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_if( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
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
		return cont_find_detail::cont_find_impl<RangeReturn>(cont, cont.find(std::forward<Arg>(arg)));
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
				tc::cont_extend(cont, cont.capacity()/*, boost::container::default_init*/); // Allow func to use the whole allocated buffer

				// sentinel to detect buffer overrun
				static constexpr typename boost::range_size<Cont>::type nSentinel=
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

			append_buffer_allowing_nulls(cont, std::forward<Func>(func));
			if constexpr (std::is_same<Cont, decltype(cont)>::value) {
				return cont;
			} else {
				tc_return_cast( tc_move(cont) );
			}
		}

		template<typename Cont, typename Func>
		Cont get_truncating_buffer_allowing_nulls(Func func) noexcept {
			return tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>([&](auto pBuffer, auto nBufferSize) noexcept {
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
		auto cont=tc::get_buffer_detail::get_truncating_buffer_allowing_nulls<Cont>(std::forward<Func>(func));
		tc::assert_no_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_truncating_null_terminated_buffer(Func&& func) noexcept {
		auto cont=tc::get_buffer_detail::get_truncating_buffer_allowing_nulls<Cont>(std::forward<Func>(func));
		tc::remove_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_buffer(Func&& func) MAYTHROW {
		auto cont=tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>(std::forward<Func>(func)); // MAYTHROW
		tc::assert_no_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	void append_buffer(Cont& cont, Func&& func) MAYTHROW {
		IF_TC_CHECKS(auto const nOffset = tc::size(cont));
		tc::get_buffer_detail::append_buffer_allowing_nulls<Cont>(cont, std::forward<Func>(func)); // MAYTHROW
		IF_TC_CHECKS(tc::assert_no_null_terminator(tc::begin_next<tc::return_drop>(cont, nOffset)));
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_null_terminated_buffer(Func&& func) MAYTHROW {
		auto cont=tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>(std::forward<Func>(func)); // MAYTHROW
		static_assert( tc::char_type< tc::range_value_t<decltype((cont))> > );
		tc::remove_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_buffer_may_be_null_terminated(Func&& func) MAYTHROW {
		auto cont = tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>(std::forward<Func>(func)); // MAYTHROW
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
			return hashed_index.emplace(std::forward<ValueTypeCtorArgs>(valuetypectorargs)...); // MAYTHROW
		}
	}

	template<typename... MultiIndexArgs, typename K, typename... ValueTypeCtorArgs >
	std::pair< tc::iterator_t<boost::multi_index::detail::ordered_index<MultiIndexArgs...>>, bool >
	multi_index_try_emplace_with_key(boost::multi_index::detail::ordered_index<MultiIndexArgs...>& ordered_index, K const& key, ValueTypeCtorArgs&& ... valuetypectorargs) MAYTHROW
	{
		auto it = ordered_index.lower_bound(key);
		if (tc::end(ordered_index)==it || ordered_index.key_comp()(key, ordered_index.key_extractor()(*it))) {
			return std::make_pair(
				tc::cont_must_emplace_before(ordered_index, tc_move(it), std::forward<ValueTypeCtorArgs>(valuetypectorargs)...), // MAYTHROW
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
			return hashed_index.emplace(std::forward<K>(key)); // MAYTHROW
		}
	}

	template<typename Cont, typename Key>
	void cont_must_erase(Cont& cont, Key&& key) noexcept {
		VERIFYEQUAL( cont.erase(std::forward<Key>(key)), 1u );
	}

	template<typename Cont, typename Key>
	bool cont_try_erase(Cont& cont, Key&& key) noexcept {
		switch_no_default(cont.erase(std::forward<Key>(key))) {
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
			bool const b=tc::invoke(pred, tc_move_if_owned(t));
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
	void reverse_inplace(Rng&& rng) noexcept {
		if constexpr( has_mem_fn_reverse<std::remove_reference_t<Rng>> ) {
			rng.reverse();
		} else {
			std::reverse(tc::begin(rng), tc::end(rng));
		}
	}

	template<typename Rng, typename Less>
	[[nodiscard]] auto ordered_unique(Rng&& rng, Less less) noexcept code_return_decltype (
		_ASSERTDEBUG( tc::is_sorted( rng, less ) );,
		tc::adjacent_unique( std::forward<Rng>(rng), std::not_fn( tc_move(less) ) )
	)

	template<typename Rng>
	[[nodiscard]] auto ordered_unique(Rng&& rng) return_decltype_noexcept(
		tc::ordered_unique( std::forward<Rng>(rng), tc::fn_less() )
	)

	template<typename Rng, typename Less = tc::fn_less>
	[[nodiscard]] auto sort_inplace_unique(Rng&& rng, Less less = Less()) noexcept code_return_decltype(
		static_assert( !std::is_reference<Rng>::value );
		tc::sort_inplace( rng, less );,
		tc::ordered_unique( std::forward<Rng>(rng), tc_move(less) )
	)

	template<typename Rng, typename Less>
	[[nodiscard]] auto sort_unique(Rng&& rng, Less less) return_decltype_noexcept(
		tc::ordered_unique(tc::sort(std::forward<Rng>(rng), less), less)
	)

	template<typename Rng>
	[[nodiscard]] auto sort_unique(Rng&& rng) return_decltype_noexcept(
		sort_unique(std::forward<Rng>(rng), tc::fn_less())
	)

	template<typename Rng, typename Less=tc::fn_less>
	[[nodiscard]] constexpr auto constexpr_sort_unique(Rng&& rng, Less&& less=Less()) noexcept {
		return tc_modified(
			tc::make_static_vector<tc::constexpr_size<Rng>::value>(std::forward<Rng>(rng)),
			tc::sort_unique_inplace(_, std::forward<Less>(less))
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

	template< typename RangeReturn, typename Rng, typename T, typename Pred >
	[[nodiscard]] decltype(auto) binary_find_unique(Rng&& rng, T const& t, Pred predLessOr3way) noexcept {
		// The result of tc::binary_find_unique must be unambiguous. In general, this means that rng is strictly
		// ordered. In some cases, it is convenient to allow multiple occurrences of the same item in
		// rng, which is not a problem as long as these items are not searched for.

		// preserve order of arguments for 3way predicates
		static constexpr bool c_b3way = tc::is_comparison_category<decltype(predLessOr3way(tc::front(rng), t))>;
		auto it=[&]() noexcept {
			if constexpr(c_b3way) {
				return tc::lower_bound<tc::return_border>( rng, t, tc::lessfrom3way(std::ref(predLessOr3way)) );
			} else {
				_ASSERTDEBUG( tc::is_sorted(rng, predLessOr3way) );
				return tc::lower_bound<tc::return_border>( rng, t, std::ref(predLessOr3way) );
			}
		}();
		if( it==tc::end( rng ) ) {
			return RangeReturn::pack_no_element(std::forward<Rng>(rng));
		} else {
			auto Greater = [&](auto const& elem) noexcept {
				if constexpr(c_b3way) {
					return std::is_gt(predLessOr3way(elem, t));
				} else {
					return predLessOr3way(t, elem);
				}
			};
			auto && ref=*it;
			if (Greater(tc::as_const(ref))) {
				return RangeReturn::pack_no_element(std::forward<Rng>(rng));
			} else {
		#ifdef _CHECKS
				auto itNext = tc_modified(it, ++_);
				_ASSERT( tc::end(rng)==itNext || Greater(tc::as_const(*itNext)));
		#endif
				return RangeReturn::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
		}
	}

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] decltype(auto) binary_find_unique(Rng&& rng, T const& t) noexcept {
		return tc::binary_find_unique<RangeReturn>( std::forward<Rng>(rng), t, tc::fn_less() );
	}

	template< typename RangeReturn, typename Rng, typename T, typename Less >
	[[nodiscard]] decltype(auto) binary_find_first(Rng&& rng, T const& t, Less less) noexcept {
		_ASSERTDEBUG( tc::is_sorted(rng, less) );
		auto it=tc::lower_bound<tc::return_border>( rng, t, less );
		if (it == tc::end(rng)) {
			return RangeReturn::pack_no_element(std::forward<Rng>(rng));
		} else {
			auto && ref = *it;
			if (less(t, tc::as_const(ref))) {
				return RangeReturn::pack_no_element(std::forward<Rng>(rng));
			} else {
				return RangeReturn::pack_element(it, std::forward<Rng>(rng), tc_move_if_owned(ref));
			}
		}
	}

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] decltype(auto) binary_find_first(Rng&& rng, T const& t) noexcept {
		return tc::binary_find_first<RangeReturn>( std::forward<Rng>(rng), t, tc::fn_less() );
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
				tc_yield(fnElementA, itA++, itB);
				if( itA==itEndA ) goto endA;
			} else if(tc::is_eq(order)) {
				tc_yield(fnElementBoth, itA++, itB++);
				if( itA==itEndA ) goto endA;
				if( itB==itEndB ) goto endB;
			} else {
				_ASSERTDEBUG(std::is_gt(order));
				tc_yield(fnElementB, itA, itB++);
				if( itB==itEndB ) goto endB;
			}
		}
	endB:
		while (itA != itEndA) tc_yield(fnElementA, itA++, itEndB);
		tc_return_cast(tc::continue_);
	endA:
		while(itB != itEndB) tc_yield(fnElementB, itEndA, itB++);
		tc_return_cast(tc::continue_);
	}

	template< typename RngA, typename RngB, typename Comp, typename FuncElementA, typename FuncElementB, typename FuncElementBoth >
	auto interleave_may_remove_current(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB, FuncElementBoth fnElementBoth) noexcept {
		return interleave_may_remove_current_iterator(rngA, rngB, comp,
			[&](auto const& lhs, tc::unused /*rhs*/) noexcept { return tc::invoke(fnElementA, *lhs); },
			[&](tc::unused /*lhs*/, auto const& rhs) noexcept { return tc::invoke(fnElementB, *rhs); },
			[&](auto const& lhs, auto const& rhs) noexcept { return tc::invoke(fnElementBoth, *lhs, *rhs); }
		);
	}

	namespace no_adl {
		// MSVC (from 15.8 to 17.0.2) sometimes crashes when this is inlined as a lambda in interleave_2.
		template<typename ItB, typename EndB, typename Comp, typename SinkA, typename SinkB, typename SinkAB>
		struct interleave_2_sink {
			ItB& m_itb;
			EndB const& m_endb;
			Comp const& m_comp;
			SinkA const& m_sinka;
			SinkB const& m_sinkb;
			SinkAB const& m_sinkab;

			template<typename A>
			auto operator()(A&& a) const& MAYTHROW -> tc::common_type_t<
				decltype(tc::continue_if_not_break(m_sinka, std::declval<A>())),
				decltype(tc::continue_if_not_break(m_sinkb, *m_itb)),
				decltype(tc::continue_if_not_break(m_sinkab, std::declval<A>(), *m_itb)),
				tc::constant<tc::continue_>
			> {
				for (;;) {
					if( m_itb == m_endb ) {
						return tc::continue_if_not_break(m_sinka, std::forward<A>(a));
					} else if(auto const order=tc::invoke(m_comp, tc::as_const(a), tc::as_const(*m_itb) ); std::is_lt(order)) {
						return tc::continue_if_not_break(m_sinka, std::forward<A>(a));
					} else if (std::is_gt(order)) {
						tc_yield(m_sinkb, *m_itb);
						++m_itb;
					} else {
						tc_yield(m_sinkab, std::forward<A>(a), *m_itb);
						++m_itb;
						return tc::constant<tc::continue_>();
					}
				}
			}
		};
	}

	template< typename RngA, typename RngB, typename Comp, typename SinkA, typename SinkB, typename SinkAB >
	auto interleave_2(RngA&& rnga, RngB&& rngb, Comp const comp, SinkA const sinka, SinkB const sinkb, SinkAB const sinkab) MAYTHROW -> tc::common_type_t<
		decltype(tc::for_each(rnga,
			std::declval<no_adl::interleave_2_sink<decltype(tc::begin(rngb)), decltype(tc::end(rngb)), Comp, SinkA, SinkB, SinkAB>>()
		)),
		decltype(tc::continue_if_not_break(sinkb, *tc::begin(rngb)))
	> {
		auto itb=tc::begin(rngb);
		auto endb=tc::end(rngb);

		tc_return_if_break(tc::for_each(
			rnga,
			no_adl::interleave_2_sink<decltype(itb), decltype(endb), Comp, SinkA, SinkB, SinkAB>{itb, endb, comp, sinka, sinkb, sinkab}
		));

		while (itb != endb) {
			tc_yield(sinkb, *itb);
			++itb;
		}
		return tc::constant<tc::continue_>();
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
				: m_compare(std::forward<Compare>(compare))
			{}

			template<
				typename Func,
				std::size_t... I,
				typename... PairItIt
			>
			tc::break_or_continue operator()(Func func, std::index_sequence<I...>, PairItIt... pairitit) const noexcept {
				bool ab[sizeof...(PairItIt)];

				while (FindBest(tc::begin(ab), pairitit...)) {
					tc_yield(func,
						std::make_pair(pairitit.first, tc::at(ab, I))...
					);

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
		return no_adl::SInterleaveImpl<Compare>(std::forward<Compare>(compare))(
			std::forward<Func>(func),
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
				std::forward<Rng>(rng), 
				tc_fn(std::holds_alternative<T>)
			),
			tc_fn(tc::get<T>)
		);
	}

	template< typename RangeReturn, typename Rng, typename T> requires (!RangeReturn::requires_iterator)
	[[nodiscard]] constexpr decltype(auto) linear_at(Rng&& rng, T n) noexcept {
		return tc::find_first_if<RangeReturn>(std::forward<Rng>(rng), [&](tc::unused) noexcept {
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
		return tc::generator_range_output<tc::decay_t<T> const&>([funcIterate=tc::make_reference_or_value(std::forward<FuncIterate>(funcIterate)),t_=tc::make_reference_or_value(std::forward<T>(t))](auto func) noexcept {
			auto t = *t_;
			tc_yield(func,tc::as_const(t));
			for (;;) {
				tc::invoke(*funcIterate, t);
				tc_yield(func,tc::as_const(t));
			}
		});
	}

	template<typename RngSep, typename... Rngs>
	decltype(auto) concat_nonempty_with_separator(RngSep&& rngSep, Rngs&&... rngs) noexcept {
		return tc::join_with_separator(
			std::forward<RngSep>(rngSep),
			tc::filter(tc::make_range(std::forward<Rngs>(rngs)...), std::not_fn(tc_fn(tc::empty)))
		);
	}

	template<typename RngSep, typename Rng0, typename... Rngs>
	decltype(auto) concat_with_separator(RngSep&& rngSep, Rng0&& rng0, Rngs&&... rngs) noexcept {
		return tc::concat(std::forward<Rng0>(rng0), tc::concat(/*copy if needed*/tc::implicit_cast<RngSep>(rngSep), std::forward<Rngs>(rngs))...);
	}

	template<typename Rng, typename Val>
	void fill(Rng&& rng, Val const& value) noexcept {
		tc::for_each(tc_move_if_owned(rng), [&](auto&& element) noexcept {
			tc_move_if_owned(element) = value;
		});
	}

}
