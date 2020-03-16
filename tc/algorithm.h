
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "meta.h"
#include "container_traits.h"
#include "size.h"
#include "equal.h"
#include "functors.h"
#include "quantifier.h"
#include "partition_iterator.h"
#include "partition_range.h"
#include "empty.h"
#include "subrange.h"
#include "unique_range_adaptor.h"
#include "counting_range.h"
#include "minmax.h"
#include "scope.h"
#include "compare.h"
#include "for_each_xxx.h"
#include "accumulate.h"
#include "append.h"
#include "cont_assign.h"
#include "size_bounded.h"
#include "find.h"
#include "filter_inplace.h"
#include "best_element.h"

#include "storage_for.h"
#include "functors.h"
#ifdef _DEBUG
#include "scope.h"
#endif
#include "container.h" // tc::vector
#include "spirit.h"
#include "make_lazy.h"
#include "tag_type.h"
#include "concat_adaptor.h"
#include "insert.h"
#include "cont_reserve.h"
#include "size_linear.h"
#include "filter_adaptor.h"

#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/utility.hpp>
#include <boost/range/algorithm/stable_sort.hpp>
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
	template< typename Rng, typename Less, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] bool is_strictly_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_==tc::for_each_adjacent_tuple<2>(rng,[&](auto const& first, auto const& second) noexcept { return tc::continue_if(less(first,second)); });
	}
	template< typename Rng, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] bool is_strictly_sorted(Rng const& rng) noexcept {
		return is_strictly_sorted( rng, tc::fn_less());
	}
	template< typename Rng, typename Less, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] constexpr bool is_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_tuple<2>(rng, [&](auto const& first, auto const& second) noexcept { return tc::continue_if(!less(second, first)); });
	}
	template< typename Rng, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] constexpr bool is_sorted(Rng const& rng) noexcept {
		return is_sorted(rng, tc::fn_less() );
	}

	template<typename Rng, typename Less, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] bool is_strictly_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_pair(rng, [&](auto const& first, auto const& second) noexcept { return tc::continue_if(less(first, second)); });
	}
	template<typename Rng, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] bool is_strictly_sorted(Rng const& rng) noexcept {
		return is_strictly_sorted(rng, tc::fn_less());
	}
	template<typename Rng, typename Less, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] bool is_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_pair(rng, [&](auto const& first, auto const& second) noexcept { return tc::continue_if(!less(second, first)); });
	}
	template<typename Rng, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] bool is_sorted(Rng const& rng) noexcept {
		return is_sorted(rng, tc::fn_less());
	}

	template< typename Rng, typename Equal, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] constexpr bool all_same(Rng const& rng, Equal equal) noexcept {
		auto const itBegin=tc::begin(rng);
		auto const itEnd=tc::end(rng);
		if(itBegin==itEnd) return true;
		auto const itNext=tc::next(itBegin);
		if(itNext==itEnd) return true;
		auto_cref( front, *itBegin );
		return all_of(
			tc::drop(rng, itNext),
			[&](auto const& _) noexcept { return equal(front, _); }
		);
	}

	template< typename Rng, typename Equal, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	[[nodiscard]] bool all_same(Rng const& rng, Equal equal) noexcept {
		std::optional<tc::range_value_t<Rng const>> oFirst;
		return tc::continue_ == tc::for_each(rng, [&](auto&& val) noexcept {
			if (oFirst) {
				return tc::continue_if(equal(*oFirst, val));
			} else {
				oFirst.emplace(tc_move_if_owned(val));
				return tc::continue_;
			}
		});
	}

	template< typename Rng >
	[[nodiscard]] constexpr bool all_same(Rng const& rng) noexcept {
		return all_same(rng, tc::fn_equal_to());
	}

	/////////////////////////////////
	// associative containers

	namespace no_adl {
		template<typename TTarget>
		struct SConversions<TTarget, std::enable_if_t<has_mem_fn_lower_bound<TTarget>::value || has_mem_fn_hash_function<TTarget>::value>> final {
			// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
			template<typename Rng>
			static TTarget fn(Rng&& rng) noexcept {
				TTarget cont;
				// force each element to be inserted
				tc::cont_must_insert_range(cont, std::forward<Rng>(rng));
				return cont;
			}
		};
	}

	/////////////////////////////////////////////////////
	// sort

	template<typename Rng, typename Pred, std::enable_if_t<has_mem_fn_sort< Rng >::value>* = nullptr>
	void sort_inplace(Rng& rng, Pred&& pred) noexcept {
		rng.sort( std::forward<Pred>(pred) );
	}
	template<typename Rng, typename Pred, std::enable_if_t<!has_mem_fn_sort< Rng >::value>* = nullptr>
	void sort_inplace(Rng& rng, Pred&& pred) noexcept {
		std::sort( tc::begin(rng), tc::end(rng), std::forward<Pred>(pred) );
	}
	template<typename Rng>
	void sort_inplace(Rng& rng) noexcept {
		tc::sort_inplace( rng, tc::fn_less() );
	}

	namespace no_adl {
		template<typename Rng, bool bStable>
		struct [[nodiscard]] sorted_index_adaptor final:
			tc::range_iterator_generator_from_index<
				sorted_index_adaptor<Rng, bStable>,
				typename boost::range_iterator<tc::vector<tc::index_t<std::remove_reference_t<Rng>>> const>::type
			>,
			tc::nonmovable // disable copy ctor and default move ctor
		{
			static_assert(!std::is_rvalue_reference<Rng>::value);
			using index=typename sorted_index_adaptor::index;
		private:
			using this_type = sorted_index_adaptor;

			tc::reference_or_value<Rng> m_baserng;
			tc::vector<tc::index_t<std::remove_reference_t<Rng>>> m_vecidx;

		public:
			using difference_type = typename decltype(m_vecidx)::difference_type;

			template<typename LessOrComp>
			explicit sorted_index_adaptor(Rng&& rng, LessOrComp lessorcomp) noexcept
				: m_baserng(tc::aggregate_tag, std::forward<Rng>(rng))
			{
				if constexpr (tc::has_size<Rng>::value) {
					tc::cont_reserve(m_vecidx, tc::size(*m_baserng));
				}
				for(auto idx=tc::begin_index(m_baserng); idx!=tc::end_index(m_baserng); tc::increment_index(*m_baserng, idx)) {
					tc::cont_emplace_back(m_vecidx, idx);
				}
				tc::sort_inplace(
					m_vecidx,
					[&](auto const& idxLhs, auto const& idxRhs ) noexcept -> bool {
						auto_cref(lhs, tc::dereference_index(*m_baserng, idxLhs));
						auto_cref(rhs, tc::dereference_index(*m_baserng, idxRhs));
						if constexpr (bStable) {
							static_assert(tc::is_random_access_range<Rng>::value);
							STATICASSERTSAME(decltype(lessorcomp(lhs, rhs)), tc::order);
							switch_no_default(lessorcomp(lhs, rhs)) {
								case tc::order::equal:
									return 0<tc::distance_to_index(*m_baserng, idxLhs, idxRhs);
								case tc::order::less:
									return true;
								case tc::order::greater:
									return false;
							}
						} else {
							return lessorcomp(lhs, rhs);
						}
					}
				);
			}

			template<ENABLE_SFINAE, std::enable_if_t<
				std::is_lvalue_reference<SFINAE_TYPE(Rng)>::value ||
				tc::is_index_valid_for_move_constructed_range<tc::decay_t<SFINAE_TYPE(Rng)>>::value // reference_or_value is movable for const Rng as well
			>* = nullptr>
			explicit sorted_index_adaptor(sorted_index_adaptor&& rng) noexcept
				: m_baserng(tc_move(rng).m_baserng)
				, m_vecidx(tc_move(rng).m_vecidx)
			{
			}
		private:
			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return tc::begin(m_vecidx);
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return tc::end(m_vecidx);
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& return_decltype_MAYTHROW(
				tc::dereference_index(*m_baserng, *idx)
			)

			STATIC_FINAL(dereference_index)(index const& idx) & return_decltype_MAYTHROW(
				tc::dereference_index(*m_baserng, *idx)
			)

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return idxLhs==idxRhs;
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				++idx;
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				--idx;
			}

			STATIC_FINAL(advance_index)(index& idx, difference_type d) const& noexcept -> void {
				idx+=d;
			}

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> difference_type {
				return idxRhs-idxLhs;
			}
		public:
			auto element_base_index(index const& idx) const& noexcept {
				return *idx;
			}
			constexpr decltype(auto) base_range() & noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() const& noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() && noexcept {
				return *tc_move(m_baserng);
			}
			constexpr decltype(auto) base_range() const&& noexcept {
				return *std::move(m_baserng);
			}
		};
	}
	using no_adl::sorted_index_adaptor;

	template<typename Rng, typename Less>
	[[nodiscard]] auto sorted_iterator_range(Rng& rng, Less less) noexcept {
		auto vecitSorted=tc::make_vector( tc::make_range_of_iterators(rng) );
		tc::sort_inplace(vecitSorted, tc::projected( std::forward<Less>(less), tc::fn_indirection() ) );
		return vecitSorted;
	}

	template<typename Rng>
	[[nodiscard]] auto sorted_iterator_range(Rng& rng) noexcept {
		return tc::sorted_iterator_range(rng, tc::fn_less());
	}

	template<typename Rng, typename Less>
	[[nodiscard]] auto sort(Rng&& rng, Less&& less) noexcept {
		return tc::sorted_index_adaptor<Rng, false/*bStable*/>(std::forward<Rng>(rng), std::forward<Less>(less));
	}

	template<typename Rng>
	[[nodiscard]] auto sort(Rng&& rng) noexcept {
		return tc::sort(std::forward<Rng>(rng), tc::fn_less());
	}

	template<typename Rng, typename Comp>
	[[nodiscard]] auto stable_sort(Rng&& rng, Comp&& comp) noexcept {
		return tc::sorted_index_adaptor<Rng, true/*bStable*/>(std::forward<Rng>(rng), std::forward<Comp>(comp));
	}

	template<typename Rng>
	[[nodiscard]] auto stable_sort(Rng&& rng) noexcept {
		return tc::stable_sort(std::forward<Rng>(rng), tc::fn_compare());
	}

	///////////////////////////////////////
	// partition ranges into subranges

	template<typename Rng, typename Less>
	[[nodiscard]] auto ordered_unique_range(Rng&& rng, Less less) noexcept {
		_ASSERTDEBUG( tc::is_sorted( rng, less ) );
		return tc::adjacent_unique_range( std::forward<Rng>(rng), tc::not_fn( tc_move(less) ) );
	}

	template<typename Rng>
	[[nodiscard]] auto ordered_unique_range(Rng&& rng) noexcept {
		return tc::ordered_unique_range( std::forward<Rng>(rng), tc::fn_less() );
	}

	template<typename Rng, typename Comp>
	[[nodiscard]] auto stable_sort_unique_range(Rng&& rng, Comp comp) noexcept {
		return tc::ordered_unique_range( tc::stable_sort( std::forward<Rng>(rng), comp ), tc::lessfrom3way(comp) );
	}

	template<typename Rng>
	[[nodiscard]] auto stable_sort_unique_range(Rng&& rng) noexcept {
		return stable_sort_unique_range( std::forward<Rng>(rng), tc::fn_compare() );
	}

	template<typename Rng, typename Less>
	[[nodiscard]] auto sort_unique_range(Rng&& rng, Less less) noexcept {
		return tc::ordered_unique_range( tc::sort( std::forward<Rng>(rng), less ), less );
	}

	template<typename Rng>
	[[nodiscard]] auto sort_unique_range(Rng&& rng) noexcept {
		return sort_unique_range( std::forward<Rng>(rng), tc::fn_less() );
	}

	template<typename Rng, typename Less, std::enable_if_t<!std::is_reference<Rng>::value>* =nullptr>
	[[nodiscard]] auto sort_inplace_unique_range(Rng&& rng, Less less) noexcept {
		tc::sort_inplace( rng, std::ref(less) );
		return tc::ordered_unique_range( std::forward<Rng>(rng), tc_move(less) );
	}

	template<typename Rng, std::enable_if_t<!std::is_reference<Rng>::value>* =nullptr>
	[[nodiscard]] auto sort_inplace_unique_range(Rng&& rng) noexcept {
		return sort_inplace_unique_range( std::forward<Rng>(rng), tc::fn_less() );
	}


	template< typename Rng, typename Less, typename Accu >
	void sort_accumulate_each_unique_range(Rng&& cont, Less less, Accu accu) noexcept {
		tc::sort_inplace( cont, less );
		{ range_filter< tc::decay_t<Rng> > rngfilter( cont );
			tc::for_each(
				tc::ordered_unique_range(
					cont,
					tc_move(less)
				),
				[&accu,&rngfilter]( auto const& rngEqualSubRange ) noexcept {
					for(
						auto it=tc::begin_next(rngEqualSubRange);
						it!=tc::end(rngEqualSubRange);
						++it
					) {
						accu( *tc::begin(rngEqualSubRange), *it );
					}
					rngfilter.keep( tc::begin(rngEqualSubRange) );
				}
			);
		}
	}

	template< typename Cont, typename Equals = tc::fn_equal_to >
	void front_unique_inplace(Cont & cont, Equals&& pred = Equals()) noexcept {
		{
			tc::range_filter< tc::decay_t<Cont> > rngfilter(cont);
			tc::for_each(
				tc::transform(
					tc::front_unique_range(cont, std::forward<Equals>(pred)),
					[](auto subrange) noexcept { // fn_boost_begin does not work, need subrange as lvalue to get non-const iterator
						return tc::begin(subrange);
					}
				),
				[&](auto const& it) noexcept { // auto it causes Internal Compiler Error
					rngfilter.keep(it);
				}
			);
		}
	}

	/*
		In contrase to std::unique, tc::adjacent_unique / tc::adjacent_unique_inplace always compares adjacent elements. This allows implementing
		bidirectional tc::adjacent_unique, with tc::adjacent_unique_inplace yielding the same result.
	*/
	template< typename Cont, typename Equals=tc::fn_equal_to >
	constexpr void adjacent_unique_inplace( Cont & cont, Equals&& pred=Equals() ) noexcept {
		{
			tc::constexpr_range_filter< tc::decay_t<Cont> > rngfilter(cont);
			// When this function is evaluated at compile time, the range returned by tc::make_range_of_iterators cannot use an rvalue as a base range.
			// This is because it stores the base range in a mutable field inside tc::reference_or_value.
			tc::for_each_may_remove_current(
				tc::make_range_of_iterators(tc::as_lvalue(tc::adjacent_unique(cont, std::forward<Equals>(pred)))),
				[&](auto it) noexcept {
					rngfilter.keep(it.element_base());
				}
			);
			rngfilter.dtor();
		}
	}

	template<typename Cont, typename Less=tc::fn_less>
	constexpr void ordered_unique_inplace( Cont& cont, Less less=Less() ) noexcept {
		_ASSERTDEBUG( tc::is_sorted( cont, less ) );
		tc::adjacent_unique_inplace( cont, tc::not_fn( tc_move(less) ) );
	}

	template< typename Cont, typename Less=tc::fn_less >
	void sort_unique_inplace(Cont& cont, Less less=Less()) noexcept {
		tc::sort_inplace( cont, less );
		tc::ordered_unique_inplace( cont, tc_move(less) );
	}

	namespace constexpr_sort_inplace_unique_detail {

		// This duplicates the functionality of tc::sort, which cannot be used in a constant expression,
		// because it uses std::sort which is not constexpr (until C++20).
		template<typename Iterator, typename Pred>
		constexpr void constexpr_sort_inplace(Iterator itBegin, Iterator itEnd, Pred pred) noexcept {
#ifdef _CHECKS
			int nIterationCount = 0;
#endif
			while (1 < itEnd - itBegin) {
				_ASSERTENOTIFY( ++nIterationCount <= 32 ); // Do we actually run into O(n^2) complexity?
				auto itPartitionBegin = itBegin;
				auto itPartitionEnd = itEnd - 1;
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
					constexpr_sort_inplace_unique_detail::constexpr_sort_inplace(itBegin, itSplitPoint, pred);
					itBegin = itSplitPoint;
				} else {
					constexpr_sort_inplace_unique_detail::constexpr_sort_inplace(itSplitPoint, itEnd, pred);
					itEnd = itSplitPoint;
				}
			}
		}
	}

	template<typename Rng, typename Pred = tc::fn_less>
	constexpr void constexpr_sort_inplace(Rng& rng, Pred&& pred = Pred()) noexcept {
		constexpr_sort_inplace_unique_detail::constexpr_sort_inplace(tc::begin(rng), tc::end(rng), pred);
		_ASSERTE( tc::is_sorted(rng, pred) );
	}

	template<typename Rng, typename Pred = tc::fn_less>
	constexpr void constexpr_sort_unique_inplace(Rng& rng, Pred&& pred = Pred()) noexcept {
		constexpr_sort_inplace(rng, pred);
		tc::ordered_unique_inplace(rng, std::forward<Pred>(pred));
	}

	DEFINE_FN( sort_unique_inplace );

	template<typename Rng, typename Less, typename Func>
	auto ordered_for_each_occurrence(Rng&& rng, Less&& less, Func func) noexcept {
		return tc::for_each(tc::ordered_unique_range( std::forward<Rng>(rng), std::forward<Less>(less)), [&](auto const& rngSub) noexcept {
			return tc::continue_if_not_break( func, std::make_pair(
				tc::begin(rngSub),
				tc::implicit_cast<typename boost::range_size< std::remove_reference_t<Rng> >::type >(tc::size_linear(rngSub))
			) );
		});
	}

	template<template<typename> class RangeReturn, typename Rng>
	[[nodiscard]] auto plurality_element(Rng&& rng) noexcept {
		if(tc::empty(rng)) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		}
		auto const rng2=tc::sorted_iterator_range(rng, tc::fn_less());

		auto it = *( tc::accumulate(
			[&](auto const& func) noexcept {
				return tc::ordered_for_each_occurrence(rng2, tc::projected(tc::fn_less(), fn_indirection()), func);
			},
			std::pair<
				typename boost::range_iterator<decltype(rng2)>::type,
				typename boost::range_size<decltype(rng2)>::type
			>(), // value-initialized, second=0
			tc::fn_assign_better(tc::projected(tc::fn_greater(), dot_member_second()))
		).first );
		return RangeReturn<Rng>::pack_element(it, std::forward<Rng>(rng), *it);
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	[[nodiscard]] auto trim_left_if(Rng&& rng, Pred&& pred) return_decltype_xvalue_by_ref_MAYTHROW(
		RangeReturn<Rng>::pack_border( tc::find_first_if<tc::return_border_before_or_end>( rng, tc::not_fn(std::forward<Pred>(pred)) ), std::forward<Rng>(rng))
	)

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	[[nodiscard]] auto trim_right_if(Rng&& rng, Pred&& pred) return_decltype_xvalue_by_ref_MAYTHROW(
		RangeReturn<Rng>::pack_border( tc::find_last_if<tc::return_border_after_or_begin>( rng, tc::not_fn(std::forward<Pred>(pred)) ), std::forward<Rng>(rng))
	)

	template< typename Rng, typename Pred >
	[[nodiscard]] decltype(auto) trim_if(Rng&& rng, Pred&& pred) MAYTHROW {
		auto rngTrimmed = tc::trim_right_if<tc::return_take>( std::forward<Rng>(rng), pred );
		return tc::trim_left_if<tc::return_drop>( tc_move(rngTrimmed), std::forward<Pred>(pred) );
	}

	template< template<typename> class RangeReturn, typename Rng, typename RngTrim >
	[[nodiscard]] decltype(auto) trim_left(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_left_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< template<typename> class RangeReturn, typename Rng, typename RngTrim >
	[[nodiscard]] decltype(auto) trim_right(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_right_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< typename Rng, typename RngTrim >
	[[nodiscard]] decltype(auto) trim(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_if( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< typename Rng, typename T >
	[[nodiscard]] bool contains_single(Rng const& rng, T const& t) noexcept {
		return 1==size_bounded(rng,2) && tc_front( rng )==t;
	}

	namespace cont_find_detail {
		template< template<typename> class RangeReturn, typename Cont>
		decltype(auto) cont_find_impl(Cont& cont, typename boost::range_iterator< Cont >::type it) noexcept {
			if( it==tc::end(cont) ) {
				return RangeReturn<Cont&>::pack_no_element(
					cont
				);
			} else {
				return RangeReturn<Cont&>::pack_element(
					it,
					cont,
					*it
				);
			}
		}
	}

	template< template<typename> class RangeReturn, typename Cont, typename Arg >
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

	template<typename Rng, std::enable_if_t<tc::is_char< tc::range_value_t<Rng> >::value>* = nullptr>
	void assert_no_null_terminator(Rng const& rng) noexcept {
		_ASSERT( !tc::find_first<tc::return_bool>(rng, tc::explicit_cast<tc::range_value_t<Rng>>('\0') ));
	}

	template<typename Rng, std::enable_if_t<!tc::is_char< tc::range_value_t<Rng> >::value>* = nullptr>
	void assert_no_null_terminator(Rng const& rng) noexcept {}

	template<typename Rng>
	void remove_null_terminator(Rng& rng) noexcept {
		static_assert( tc::is_char< tc::range_value_t<Rng> >::value );
		_ASSERT( !tc::empty(rng) );
		_ASSERTEQUAL( tc_back(rng), tc::explicit_cast< tc::range_value_t<Rng> >('\0') );
		tc::take_inplace(rng,tc::end_prev(rng));
		tc::assert_no_null_terminator(rng);
	}

	template<typename Char, std::size_t N, std::enable_if_t<tc::is_char<Char>::value>* = nullptr>
	[[nodiscard]] constexpr auto take_null_terminated(Char const (&ach)[N]) noexcept {
		return tc::find_first<tc::return_take_before>(tc::as_array(ach), Char());
	}

	namespace get_buffer_detail {
#if defined _DEBUG && !defined __clang__
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
		Cont get_buffer_allowing_nulls(Func func) MAYTHROW {
			static_assert( tc::is_decayed<Cont>::value );

			// sentinel to detect buffer overrun
			static constexpr typename boost::range_size<Cont>::type nSentinel=
#if defined _DEBUG && !defined __clang__
				1;
			typename no_adl::container_with_sentinel<Cont>::type
#else
				0;
			Cont
#endif
				cont;
			if (0 == cont.capacity()) {
				tc::cont_reserve(cont, 8);
			}

			for (;;) {
				tc::cont_clear(cont, cont.capacity()/*, boost::container::default_init*/); // Allow func to use the whole allocated buffer
#if defined _DEBUG && !defined __clang__
				tc::for_each(tc::drop_last(cont, nSentinel), [](auto& x) noexcept { UNINITIALIZED(x); } );
#endif
				auto const nSize =
#if defined _DEBUG && !defined __clang__
				 [&]() MAYTHROW {
					tc::uninitialize(tc_back(cont));
					scope_exit( tc::assert_uninitialized(tc_back(cont)) );
					return 
#endif
						func(tc::ptr_begin(cont), tc::size(cont)-nSentinel);
#if defined _DEBUG && !defined __clang__
				}();
#endif
				if (nSize <= tc::size(cont)-nSentinel) {
					_ASSERT(0 <= nSize);
					tc::take_first_inplace(cont, nSize);
					if (!tc::empty(cont)) _ASSERTINITIALIZED(tc_back(cont));
					return
#if defined _DEBUG && !defined __clang__
						tc::explicit_cast<Cont>(cont)
#else
						cont
#endif
					;
				}
				tc::cont_reserve(cont, nSize+nSentinel); // The container must grow bigger, but let cont_reserve decide by how much
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
	[[nodiscard]] Cont get_null_terminated_buffer(Func&& func) MAYTHROW {
		static_assert( tc::is_char< tc::range_value_t<Cont> >::value );
		auto cont=tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>(std::forward<Func>(func)); // MAYTHROW
		tc::remove_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	[[nodiscard]] Cont get_buffer_may_be_null_terminated(Func&& func) MAYTHROW {
		static_assert(tc::is_char< tc::range_value_t<Cont> >::value);
		auto cont = tc::get_buffer_detail::get_buffer_allowing_nulls<Cont>(std::forward<Func>(func)); // MAYTHROW
		if (tc_back(cont) == tc::explicit_cast< tc::range_value_t<Cont> >('\0')) {
			tc::remove_null_terminator(cont);
		} else {
			tc::assert_no_null_terminator(cont);
		}
		return cont;
	}


	template<typename... MultiIndexArgs, typename K, typename... ValueTypeCtorArgs >
	std::pair< typename boost::range_iterator<boost::multi_index::detail::hashed_index<MultiIndexArgs...>>::type, bool >
	multi_index_try_emplace_with_key(boost::multi_index::detail::hashed_index<MultiIndexArgs...>& hashed_index, K const& key, ValueTypeCtorArgs&& ... valuetypectorargs) MAYTHROW
	{
		if(auto it = tc::cont_find<tc::return_element_or_null>(hashed_index, key)) {
			return std::make_pair(tc_move(it), false);
		} else {
			return hashed_index.emplace(std::forward<ValueTypeCtorArgs>(valuetypectorargs)...); // MAYTHROW
		}
	}

	template<typename... MultiIndexArgs, typename K, typename... ValueTypeCtorArgs >
	std::pair< typename boost::range_iterator<boost::multi_index::detail::ordered_index<MultiIndexArgs...>>::type, bool >
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
	std::pair<typename boost::range_iterator<boost::multi_index::detail::hashed_index<MultiIndexArgs...>>::type, bool>
	map_query_cache(boost::multi_index::detail::hashed_index<MultiIndexArgs...>& hashed_index, K&& key) MAYTHROW {
		if (auto it = tc::cont_find<tc::return_element_or_null>(hashed_index, tc::as_const(key))) {
			return std::make_pair(tc_move(it), false);
		} else {
			return hashed_index.emplace(std::forward<K>(key)); // MAYTHROW
		}
	}

	template< typename Cont >
	void cont_must_erase(Cont& cont, typename Cont::key_type const& data) noexcept {
		VERIFYEQUAL( cont.erase(data), 1u );
	}

	template< typename Cont >
	bool cont_try_erase(Cont& cont, typename Cont::key_type const& data) noexcept {
		switch_no_default(cont.erase(data)) {
			case 0: return false;
			case 1: return true;
		};
	}

	/////////////////////////////////////////////////////
	// remove_count_erase

	template<typename Cont, typename Pred>
	[[nodiscard]] 
	typename tc::size_proxy< typename boost::range_size<Cont>::type > remove_count_erase_if(Cont& cont, Pred pred) noexcept {
		typename boost::range_size<Cont>::type count=0;
		tc::filter_inplace( cont, [&]( tc::range_reference_t<Cont> t ) noexcept ->bool {
			bool const b=pred(tc_move_if_owned(t));
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

	template<typename Rng, std::enable_if_t<has_mem_fn_reverse< std::remove_reference_t<Rng> >::value>* = nullptr>
	void reverse_inplace(Rng&& rng) noexcept {
		rng.reverse();
	}
	template<typename Rng, std::enable_if_t<!has_mem_fn_reverse< std::remove_reference_t<Rng> >::value>* = nullptr>
	void reverse_inplace(Rng&& rng) noexcept {
		std::reverse(tc::begin(rng), tc::end(rng));
	}

	template<typename Rng, typename Less>
	[[nodiscard]] auto ordered_unique(Rng&& rng, Less less) noexcept code_return_decltype (
		_ASSERTDEBUG( tc::is_sorted( rng, less ) );,
		tc::adjacent_unique( std::forward<Rng>(rng), tc::not_fn( tc_move(less) ) )
	)

	template<typename Rng>
	[[nodiscard]] auto ordered_unique(Rng&& rng) return_decltype_noexcept(
		tc::ordered_unique( std::forward<Rng>(rng), tc::fn_less() )
	)

	template<typename Rng, typename Less, std::enable_if_t<!std::is_reference<Rng>::value>* =nullptr>
	[[nodiscard]] auto sort_inplace_unique(Rng&& rng, Less less) noexcept code_return_decltype(
		tc::sort_inplace( rng, less );,
		tc::ordered_unique( std::forward<Rng>(rng), tc_move(less) )
	)

	template<typename Rng, std::enable_if_t<!std::is_reference<Rng>::value>* =nullptr>
	[[nodiscard]] auto sort_inplace_unique(Rng&& rng) return_decltype_noexcept(
		sort_inplace_unique( std::forward<Rng>(rng), tc::fn_less() )
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
		using static_vector_t = tc::static_vector<tc::range_value_t<Rng>, tc::constexpr_size<Rng>::value>;
		static_vector_t vec(tc::constexpr_tag, rng);
		tc::constexpr_sort_unique_inplace(vec, std::forward<Less>(less));
		return static_vector_t(tc::constexpr_tag, vec); // force elision as we cannot constexpr move or copy static_vector aholmes/2020-01-16
	}

	template< template<typename> class RangeReturn, typename SetType, typename T, typename Less >
	void binary_find_unique(tc::unordered_set<SetType> const& rng, T const& t, Less less) noexcept = delete;

	template< template<typename> class RangeReturn, typename Rng, typename T, typename Less >
	[[nodiscard]] decltype(auto) binary_find_unique(Rng&& rng, T const& t, Less less) noexcept {
		// The result of tc::binary_find_unique must be unambiguous. In general, this means that rng is strictly
		// ordered. In some cases, it is convenient to allow multiple occurrences of the same item in
		// rng, which is not a problem as long as these items are not searched for.
		_ASSERTDEBUG( tc::is_sorted(rng, less) );
		auto it=tc::lower_bound<tc::return_border>( rng, t, std::ref(less) );
		if( it==tc::end( rng ) ) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			auto && ref=*it;
			if (less(t, tc::as_const(ref))) {
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			} else {
		#ifdef _CHECKS
				auto itNext = tc::next(it);
				_ASSERT( tc::end(rng)==itNext || less(t, tc::as_const(*itNext)) );
		#endif
				return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	[[nodiscard]] decltype(auto) binary_find_unique(Rng&& rng, T const& t) noexcept {
		return tc::binary_find_unique<RangeReturn>( std::forward<Rng>(rng), t, tc::fn_less() );
	}

	template< template<typename> class RangeReturn, typename Rng, typename T, typename Less >
	[[nodiscard]] decltype(auto) binary_find_first(Rng&& rng, T const& t, Less less) noexcept {
		_ASSERTDEBUG( tc::is_sorted(rng, less) );
		auto it=tc::lower_bound<tc::return_border>( rng, t, less );
		if (it == tc::end(rng)) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			auto && ref = *it;
			if (less(t, tc::as_const(ref))) {
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			} else {
				return RangeReturn<Rng>::pack_element(it, std::forward<Rng>(rng), tc_move_if_owned(ref));
			}
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
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
			return boost::prior(it);
		} else {
			auto itPrior = boost::prior(it);
			return (t - *itPrior) < (*it - t) ? itPrior : it;
		}
	}

	template< typename RngA, typename RngB, typename Comp, typename FuncElementA, typename FuncElementB, typename FuncElementBoth > 
	auto interleave_may_remove_current(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB, FuncElementBoth fnElementBoth) noexcept -> tc::common_type_t<
		decltype(tc::continue_if_not_break(fnElementA, *tc::begin(rngA))),
		decltype(tc::continue_if_not_break(fnElementB, *tc::begin(rngB))),
		decltype(tc::continue_if_not_break(fnElementBoth, *tc::begin(rngA), *tc::begin(rngB))),
		decltype(tc::for_each_may_remove_current(tc::drop(rngA, tc::begin(rngA)), fnElementA)),
		decltype(tc::for_each_may_remove_current(tc::drop(rngB, tc::begin(rngB)), fnElementB))
	> {
		auto itA=tc::begin(rngA);
		auto itEndA=tc::end(rngA);
		auto itB=tc::begin(rngB);
		auto itEndB=tc::end(rngB);
		if( itA==itEndA ) goto endA;
		if( itB==itEndB ) goto endB;
		for(;;) {
			switch_no_default( tc::implicit_cast<tc::order>(comp( tc::as_const(*itA), tc::as_const(*itB) )) ) { // make sure comp returns tc::order
			case tc::order::less:
				RETURN_IF_BREAK( tc::continue_if_not_break(fnElementA, *(itA++)));
				if( itA==itEndA ) goto endA;
				break;
			case tc::order::equal:
				RETURN_IF_BREAK( tc::continue_if_not_break(fnElementBoth, *(itA++), *(itB++)));
				if( itA==itEndA ) goto endA;
				if( itB==itEndB ) goto endB;
				break;
			case tc::order::greater:
				RETURN_IF_BREAK( tc::continue_if_not_break(fnElementB, *(itB++)));
				if( itB==itEndB ) goto endB;
				break;
			}
		}
	endB:
		return tc::for_each_may_remove_current(tc::drop(rngA,itA), fnElementA);
	endA:
		return tc::for_each_may_remove_current(tc::drop(rngB,itB), fnElementB);
	}

	template< typename RngA, typename RngB, typename Comp, typename FuncElementA, typename FuncElementB, typename FuncElementBoth >
	auto interleave_2(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB,  FuncElementBoth fnElementBoth) noexcept {
		auto itB=tc::begin(rngB);
		auto itEndB=tc::end(rngB);

		auto breakorcontinue = tc::for_each(rngA, 
			[&](auto&& refA) noexcept { /* MSVC fails to compile decltype(tc::continue_if_not_break(fnElementA, refA)) in trailing return type for the lambda */
				return 
					[&]() noexcept -> tc::common_type_t<
						decltype(tc::continue_if_not_break(fnElementA, refA)),
						decltype(tc::continue_if_not_break(fnElementB, *itB)),
						decltype(tc::continue_if_not_break(fnElementBoth, refA, *itB)),
						INTEGRAL_CONSTANT(tc::continue_)
					> {
						for (;;) {
							tc::order order;
							if( itB == itEndB || (order=comp(tc::as_const(refA), tc::as_const(*itB) ))<tc::order::equal ) {
								return tc::continue_if_not_break(fnElementA, std::forward<decltype(refA)>(refA));
							} else if (tc::order::equal < VERIFYINITIALIZED(order)) {
								RETURN_IF_BREAK(tc::continue_if_not_break(fnElementB, *itB));
								++itB;
							} else {
								return tc::continue_if_not_break(fnElementBoth, std::forward<decltype(refA)>(refA), *itB++);
							}
						}
					}();
			}
		);

		RETURN_IF_BREAK(breakorcontinue);

		while (itB != itEndB) {
			RETURN_IF_BREAK(tc::implicit_cast<decltype(breakorcontinue)>(tc::continue_if_not_break(fnElementB, *itB)));
			++itB;
		}
		return tc::implicit_cast<decltype(breakorcontinue)>(INTEGRAL_CONSTANT(tc::continue_)());
	}

	namespace no_adl {
		template<typename Compare>
		struct SInterleaveImpl {
			tc::decay_t<Compare> m_compare;

			template<typename PairItItBest>
			bool HasBetterElement(bool* const, PairItItBest const&) const& noexcept {
				return false;
			}

			template<
				typename PairItItBest,
				typename PairItIt0,
				typename... Args
			>
			bool HasBetterElement(bool* const itb, PairItItBest const& argBest, PairItIt0 const& pairitit0, Args const&... args) const& noexcept {
				if (pairitit0.first != pairitit0.second) {
					switch_no_default(m_compare(tc::as_const(*argBest.first), tc::as_const(*pairitit0.first))) {
					case tc::order::less:
						*itb = false;
						return HasBetterElement(tc::next(itb), argBest, args...);
					case tc::order::equal: {
						bool b = HasBetterElement(tc::next(itb), argBest, args...);
						*itb = !b;
						return b;
					}
					case tc::order::greater:
						*itb = !HasBetterElement(tc::next(itb), pairitit0, args...);
						return true;
					}
				} else {
					*itb = false;
					return HasBetterElement(tc::next(itb), argBest, args...);
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
					*itb = !HasBetterElement(tc::next(itb), pairitit0, args...);
					return true;
				} else {
					*itb = false;
					return FindBest(tc::next(itb), args...);
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
					RETURN_IF_BREAK(tc::continue_if_not_break(func,
						std::make_pair(pairitit.first, tc_at(ab, I))...
					));

					([](auto& it, bool const b) noexcept {if (b) ++it;}(pairitit.first, tc_at(ab,I)), ...);
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
		auto&& rngFront = tc_front(rngrng);
		return tc::accumulate(
			tc::drop_first(rngrng),
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
				[](auto const& var) noexcept {
					return std::holds_alternative<T>(var);
				}
			),
			[](auto&& var) noexcept -> decltype(auto) {
				return tc::get<T>(std::forward<decltype(var)>(var));
			}
		);
	}

	template< template<typename> class RangeReturn, typename Rng, typename T, std::enable_if_t<!RangeReturn<Rng>::requires_iterator>* = nullptr >
	[[nodiscard]] typename RangeReturn<Rng>::type linear_at(Rng&& rng, T n) noexcept {
		return tc::find_first_if<RangeReturn>(std::forward<Rng>(rng), [&](auto const&) noexcept {
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
		return [funcIterate=tc::make_reference_or_value(std::forward<FuncIterate>(funcIterate)),t_=tc::make_reference_or_value(std::forward<T>(t))](auto func) noexcept {
			auto t = *t_;
			RETURN_IF_BREAK(tc::continue_if_not_break(func,t));
			for (;;) {
				(*funcIterate)(t);
				RETURN_IF_BREAK(tc::continue_if_not_break(func,t));
			}
		};
	}
}
