
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
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
#include "sub_range.h"
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

#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/utility.hpp>
#include <boost/implicit_cast.hpp>
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
	bool is_strictly_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_==tc::for_each_adjacent_tuple<2>(rng,[&](auto const& first, auto const& second) noexcept { return tc::continue_if(less(first,second)); });
	}
	template< typename Rng, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_strictly_sorted(Rng const& rng) noexcept {
		return is_strictly_sorted( rng, tc::fn_less());
	}
	template< typename Rng, typename Less, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_tuple<2>(rng, [&](auto const& first, auto const& second) noexcept { return tc::continue_if(!less(second, first)); });
	}
	template< typename Rng, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng) noexcept {
		return is_sorted(rng, tc::fn_less() );
	}

	template<typename Rng, typename Less, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_strictly_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_pair(rng, [&](auto const& first, auto const& second) noexcept { return tc::continue_if(less(first, second)); });
	}
	template<typename Rng, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_strictly_sorted(Rng const& rng) noexcept {
		return is_strictly_sorted(rng, tc::fn_less());
	}
	template<typename Rng, typename Less, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_pair(rng, [&](auto const& first, auto const& second) noexcept { return tc::continue_if(!less(second, first)); });
	}
	template<typename Rng, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng) noexcept {
		return is_sorted(rng, tc::fn_less());
	}

	template< typename Rng, typename Equal >
	bool all_same(Rng const& rng, Equal equal) noexcept {
		auto const itBegin=tc::begin(rng);
		auto const itEnd=tc::end(rng);
		if(itBegin==itEnd) return true;
		auto const itNext=boost::next(itBegin);
		if(itNext==itEnd) return true;
		auto_cref( front, *itBegin );
		return all_of(
			tc::drop(rng, itNext),
			[&](auto const& _) noexcept { return equal(front, _); }
		);
	}

	template< typename Rng >
	bool all_same(Rng const& rng) noexcept {
		return all_same(rng, tc::fn_equal_to());
	}

	/////////////////////////////////
	// associative containers

	namespace no_adl {
		// SConversions cannot implement templated operator()(Rng&&) *and* use operator()(TSource&&)
		// from SDefaultConversions. Apparently, despite the std::enable_if constructs, both are considered
		// to have the same signatures and the using declaration is therefore ignored. Only Clang implements
		// this standard rule, however:
		// http://stackoverflow.com/questions/18861514/using-and-overloading-a-template-member-function-of-a-base-class
		template<typename TTarget>
		struct SSetConversionsHelper {
			// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
			template<typename Rng>
			static std::enable_if_t<
				!tc::is_safely_constructible< TTarget, Rng&& >::value // disable for trivial conversions to use move semantic / copy on write where possible
			,TTarget > fn(Rng&& rng) noexcept {
				TTarget cont;
				// force each element to be inserted
				tc::cont_must_insert_range(cont, std::forward<Rng>(rng));
				return cont;
			}
		};

		template<typename TTarget>
		struct SConversions<TTarget, std::enable_if_t<has_mem_fn_lower_bound<TTarget>::value || has_mem_fn_hash_function<TTarget>::value>> final : SDefaultConversions<TTarget>, SSetConversionsHelper<TTarget> {
			using SDefaultConversions<TTarget>::fn;
			using SSetConversionsHelper<TTarget>::fn;
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

	template<typename Rng, typename Less>
	auto sorted_iterator_range(Rng&& rng, Less less) noexcept {
		auto vecitSorted=tc::make_vector( tc::make_range_of_iterators(rng) );
		tc::sort_inplace(vecitSorted, tc::projected( std::forward<Less>(less), tc::fn_indirection() ) );
		return vecitSorted;
	}

	template<typename Rng>
	decltype(auto) sorted_iterator_range(Rng&& rng) noexcept {
		return tc::sorted_iterator_range(std::forward<Rng>(rng), tc::fn_less());
	}

	template<typename Rng, typename Less>
	decltype(auto) sort(Rng&& rng, Less&& less) noexcept {
		return tc::transform( tc::sorted_iterator_range(std::forward<Rng>(rng), std::forward<Less>(less)), tc::fn_indirection() );
	}

	template<typename Rng>
	decltype(auto) sort(Rng&& rng) noexcept {
		return tc::transform( tc::sorted_iterator_range(std::forward<Rng>(rng)), tc::fn_indirection() );
	}

	template<typename Rng, typename Comp>
	auto stable_sorted_iterator_range(Rng&& rng, Comp comp) noexcept {
		auto vecitSorted=tc::make_vector( tc::make_range_of_iterators(rng) );
		tc::sort_inplace(vecitSorted, [&](auto lhs, auto rhs) noexcept {
			switch_no_default(comp(*lhs, *rhs)) {
				case tc::order::equal:
					return lhs<rhs; // TODO: solution for non-random-access iterators
				case tc::order::less:
					return true;
				case tc::order::greater:
					return false;
			}
		});
		return vecitSorted;
	}

	template<typename Rng>
	decltype(auto) stable_sorted_iterator_range(Rng&& rng) noexcept {
		return tc::stable_sorted_iterator_range(std::forward<Rng>(rng), tc::fn_compare());
	}

	template<typename Rng, typename Comp>
	decltype(auto) stable_sort(Rng&& rng, Comp&& comp) noexcept {
		return tc::transform( tc::stable_sorted_iterator_range(std::forward<Rng>(rng), std::forward<Comp>(comp)), tc::fn_indirection() );
	}

	template<typename Rng>
	decltype(auto) stable_sort(Rng&& rng) noexcept {
		return tc::transform( tc::stable_sorted_iterator_range(std::forward<Rng>(rng)), tc::fn_indirection() );
	}

	///////////////////////////////////////
	// partition ranges into subranges

	template<typename Rng, typename Less>
	decltype(auto) ordered_unique_range(Rng&& rng, Less less) noexcept {
		_ASSERTDEBUG( tc::is_sorted( rng, less ) );
		return tc::adjacent_unique_range( std::forward<Rng>(rng), tc::not_fn( tc_move(less) ) );
	}

	template<typename Rng>
	decltype(auto) ordered_unique_range(Rng&& rng) noexcept {
		return tc::ordered_unique_range( std::forward<Rng>(rng), tc::fn_less() );
	}

	template<typename Rng, typename Comp>
	decltype(auto) stable_sort_unique_range(Rng&& rng, Comp comp) noexcept {
		return tc::ordered_unique_range( tc::stable_sort( std::forward<Rng>(rng), comp ), tc::lessfrom3way(comp) );
	}

	template<typename Rng>
	decltype(auto) stable_sort_unique_range(Rng&& rng) noexcept {
		return stable_sort_unique_range( std::forward<Rng>(rng), tc::fn_compare() );
	}

	template<typename Rng, typename Less>
	decltype(auto) sort_unique_range(Rng&& rng, Less less) noexcept {
		return tc::ordered_unique_range( tc::sort( std::forward<Rng>(rng), less ), less );
	}

	template<typename Rng>
	decltype(auto) sort_unique_range(Rng&& rng) noexcept {
		return sort_unique_range( std::forward<Rng>(rng), tc::fn_less() );
	}

	template<typename Rng, typename Less, std::enable_if_t<!std::is_reference<Rng>::value>* =nullptr>
	decltype(auto) sort_inplace_unique_range(Rng&& rng, Less less) noexcept {
		tc::sort_inplace( rng, std::ref(less) );
		return tc::ordered_unique_range( std::forward<Rng>(rng), tc_move(less) );
	}

	template<typename Rng, std::enable_if_t<!std::is_reference<Rng>::value>* =nullptr>
	decltype(auto) sort_inplace_unique_range(Rng&& rng) noexcept {
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
	void adjacent_unique_inplace( Cont & cont, Equals&& pred=Equals() ) noexcept {
		{
			tc::range_filter< tc::decay_t<Cont> > rngfilter(cont);
			tc::for_each_may_remove_current(
				tc::make_range_of_iterators(tc::adjacent_unique(cont, std::forward<Equals>(pred))),
				[&](auto it) noexcept {
					rngfilter.keep(it.element_base());
				}
			);
		}
	}

	template<typename Cont, typename Less=tc::fn_less>
	void ordered_unique_inplace( Cont& cont, Less less=Less() ) noexcept {
		_ASSERTDEBUG( tc::is_sorted( cont, less ) );
		tc::adjacent_unique_inplace( cont, tc::not_fn( tc_move(less) ) );
	}

	template< typename Cont, typename Less=tc::fn_less >
	void sort_unique_inplace(Cont& cont, Less less=Less()) noexcept {
		tc::sort_inplace( cont, less );
		tc::ordered_unique_inplace( cont, tc_move(less) );
	}

	DEFINE_FN( sort_unique_inplace );

	template<typename Rng, typename Less, typename Func>
	auto ordered_for_each_occurrence(Rng&& rng, Less&& less, Func func) noexcept {
		return tc::for_each(tc::ordered_unique_range( std::forward<Rng>(rng), std::forward<Less>(less)), [&](auto const& rngSub) noexcept {
			return tc::continue_if_not_break( func, std::make_pair(
				tc::begin(rngSub),
				boost::implicit_cast<typename boost::range_size< std::remove_reference_t<Rng> >::type >(tc::size_linear(rngSub))
			) );
		});
	}

	template<typename Rng>
	auto plurality_element(Rng&& rng) noexcept {
		_ASSERT( !tc::empty(rng) );
		auto const rng2=tc::sorted_iterator_range(rng, tc::fn_less());

		return *( tc::accumulate(
			[&](auto const& func) noexcept {
				return tc::ordered_for_each_occurrence(rng2, tc::projected(tc::fn_less(), fn_indirection()), func);
			},
			std::pair<
				typename boost::range_iterator<decltype(rng2)>::type,
				typename boost::range_size<decltype(rng2)>::type
			>(), // value-initialized, second=0
			tc::fn_assign_better(tc::projected(tc::fn_greater(), dot_member_second()))
		).first );
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	decltype(auto) trim_left_if(Rng&& rng, Pred&& pred) MAYTHROW {
		return RangeReturn<Rng>::pack_border( tc::find_first_if<tc::return_border_before_or_end>( rng, tc::not_fn(std::forward<Pred>(pred)) ), std::forward<Rng>(rng));
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	decltype(auto) trim_right_if(Rng&& rng, Pred&& pred) MAYTHROW {
		return RangeReturn<Rng>::pack_border( tc::find_last_if<tc::return_border_after_or_begin>( rng, tc::not_fn(std::forward<Pred>(pred)) ), std::forward<Rng>(rng));
	}

	template< typename Rng, typename Pred >
	decltype(auto) trim_if(Rng&& rng, Pred&& pred) MAYTHROW {
		auto rngTrimmed = tc::trim_right_if<tc::return_take>( std::forward<Rng>(rng), pred );
		return tc::trim_left_if<tc::return_drop>( tc_move(rngTrimmed), std::forward<Pred>(pred) );
	}

	template< template<typename> class RangeReturn, typename Rng, typename RngTrim >
	decltype(auto) trim_left(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_left_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< template<typename> class RangeReturn, typename Rng, typename RngTrim >
	decltype(auto) trim_right(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_right_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< typename Rng, typename RngTrim >
	decltype(auto) trim(Rng&& rng, RngTrim const& rngTrim) MAYTHROW {
		return tc::trim_if( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::find_first<tc::return_bool>(rngTrim, _); } );
	}

	template< typename Rng, typename T >
	bool contains_single(Rng const& rng, T const& t) noexcept {
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
	decltype(auto) cont_find(Cont& cont, Arg&& arg) noexcept {
		return cont_find_detail::cont_find_impl<RangeReturn>(cont, cont.find(std::forward<Arg>(arg)));
	}

#ifdef _DEBUG
	using static_vector_size_t = std::uint32_t; // fixed width integer for shared heap
	namespace no_adl {
		template< typename T, tc::static_vector_size_t N> struct static_vector;
	}
	using no_adl::static_vector;
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

	template<typename Cont, typename Func>
	Cont get_truncating_buffer(Func func) noexcept {
		static_assert( tc::is_decayed<Cont>::value );

		// sentinel to detect buffer overrun
		constexpr typename boost::range_size<Cont>::type nSentinel=
#if defined _DEBUG && !defined __clang__
			1;
		typename no_adl::container_with_sentinel<Cont>::type
#else
			0;
		Cont
#endif				
			cont;
		tc::cont_clear(cont, 0<cont.capacity() ? cont.capacity() : tc::explicit_cast<typename Cont::size_type>(8)/*, boost::container::default_init*/);

		for (;;) {
			auto const nSize =
#if defined _DEBUG && !defined __clang__
			 [&]() noexcept {
				tc::uninitialize(tc_back(cont));
				scope_exit( _ASSERTDEBUG( !tc::check_initialized(tc_back(cont))) );
				return 
#endif
					func(tc::ptr_begin(cont), tc::size(cont)-nSentinel);
#if defined _DEBUG && !defined __clang__
			}();
#endif
			if (nSize < tc::size(cont)-nSentinel) {
				_ASSERT(0 <= nSize);
				tc::take_first_inplace(cont, nSize);
				return
#if defined _DEBUG && !defined __clang__
					tc::explicit_cast<Cont>(cont)
#else
					cont
#endif
				;
			}
			_ASSERTEQUAL(nSize, tc::size(cont)-nSentinel);
			tc::cont_clear(cont,tc::cont_extended_memory(cont));
		}
	}

	namespace get_buffer_detail {
		template<typename Cont, typename Func>
		Cont get_sized_buffer_may_be_null_terminated(Func func) MAYTHROW {
			static_assert( tc::is_decayed<Cont>::value );

			// sentinel to detect buffer overrun
			constexpr typename boost::range_size<Cont>::type nSentinel=
#if defined _DEBUG && !defined __clang__
				1;
			typename no_adl::container_with_sentinel<Cont>::type
#else
				0;
			Cont
#endif
				cont;
			static_assert( std::is_trivially_copyable<tc::decay_t<decltype(*tc::ptr_begin(cont))>>::value );
			tc::cont_clear(cont,tc::max(cont.capacity(),nSentinel)/*, boost::container::default_init*/);

			for (;;) {
				auto const nSize = 
#if defined _DEBUG && !defined __clang__
				 [&]() MAYTHROW {
					tc::fill_with_dead_pattern(tc_back(cont));
					scope_exit( tc::assert_dead_pattern(tc_back(cont)) );
					return 
#endif
					func(tc::ptr_begin(cont), tc::size(cont)-nSentinel); // MAYTHROW
#if defined _DEBUG && !defined __clang__
				}();
#endif
				if (nSize <= tc::size(cont)-nSentinel) {
					_ASSERT(0 <= nSize);
					tc::take_first_inplace(cont, nSize);
					return
#if defined _DEBUG && !defined __clang__
						tc::explicit_cast<Cont>(cont)
#else
						cont
#endif
					;
				}
				tc::cont_clear(cont,nSize+nSentinel);
			}
		}
	} // namespace get_buffer_detail

	template<typename Cont, typename Func>
	Cont get_sized_buffer(Func&& func) MAYTHROW {
		auto cont=tc::get_buffer_detail::get_sized_buffer_may_be_null_terminated<Cont>(std::forward<Func>(func)); // MAYTHROW
		tc::assert_no_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	Cont get_sized_null_terminated_buffer(Func&& func) MAYTHROW {
		static_assert( tc::is_char< tc::range_value_t<Cont> >::value );
		auto cont=tc::get_buffer_detail::get_sized_buffer_may_be_null_terminated<Cont>(std::forward<Func>(func)); // MAYTHROW
		tc::remove_null_terminator(cont);
		return cont;
	}

	template<typename Cont, typename Func>
	Cont get_sized_buffer_may_be_null_terminated(Func&& func) MAYTHROW {
		static_assert(tc::is_char< tc::range_value_t<Cont> >::value);
		return tc::get_buffer_detail::get_sized_buffer_may_be_null_terminated<Cont>(std::forward<Func>(func)); // MAYTHROW
	}


	template< typename... MapArgs, typename K, typename... MappedTypeCtorArgs >
	auto map_try_emplace_with_key(std::map<MapArgs...>& map, K&& key, MappedTypeCtorArgs&& ... mappedtypectorargs) MAYTHROW {
		// TODO C++17: Use std::map::try_emplace
		auto it = map.lower_bound(key);
		if (tc::end(map)==it || map.key_comp()(key, it->first)) {
			return std::make_pair(
				tc::cont_must_emplace_before(
					map,
					tc_move(it),
					std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<MappedTypeCtorArgs>(mappedtypectorargs)...) // delay actual construction of mapped type
				), // MAYTHROW
				true
			);
		} else {
			return std::make_pair(tc_move(it), false);
		}
	}

	template< typename Key, typename T, typename K, typename... MappedTypeCtorArgs >
	auto unordered_map_try_emplace_with_key(tc::unordered_map<Key, T>& map, K&& key, MappedTypeCtorArgs&& ... mappedtypectorargs) MAYTHROW {
		// TODO C++17: Use std::unordered_map::try_emplace
		auto it = map.find(key);
		if (tc::end(map)==it) {
			return std::make_pair(
				tc::cont_must_emplace(
					map,
					std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<MappedTypeCtorArgs>(mappedtypectorargs)...) // delay actual construction of mapped type
				), // MAYTHROW
				true
			);
		} else {
			return std::make_pair(tc_move(it), false);
		}
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
	typename tc::size_proxy< typename boost::range_size<Cont>::type > remove_count_erase_if(Cont& cont, Pred pred) noexcept {
		typename boost::range_size<Cont>::type count=0;
		tc::filter_inplace( cont, [&]( tc::range_reference_t<Cont> t ) noexcept ->bool {
			bool const b=pred(tc_move_if_owned(t));
			count+=boost::implicit_cast<typename boost::range_size<Cont>::type>(b);
			return !b;
		} );
		return tc::size_proxy< typename boost::range_size<Cont>::type >(count);
	}

	template<typename Cont, typename T>
	typename tc::size_proxy< typename boost::range_size<Cont>::type > remove_count_erase(Cont& cont, T const& t) noexcept {
		return remove_count_erase_if( cont, [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}

	/////////////////////////////////////////////////////
	// reverse_inplace
	// inplace algorithms should accept only lvalue containers, but reverse_inplace is called with
	// sub_ranges of containers, so it must accept rvalues.

	template<typename Rng, std::enable_if_t<has_mem_fn_reverse< std::remove_reference_t<Rng> >::value>* = nullptr>
	void reverse_inplace(Rng&& rng) noexcept {
		rng.reverse();
	}
	template<typename Rng, std::enable_if_t<!has_mem_fn_reverse< std::remove_reference_t<Rng> >::value>* = nullptr>
	void reverse_inplace(Rng&& rng) noexcept {
		std::reverse(tc::begin(rng), tc::end(rng));
	}

	template<typename Rng, typename Less>
	auto ordered_unique(Rng&& rng, Less less) noexcept code_return_decltype (
		_ASSERTDEBUG( tc::is_sorted( rng, less ) );,
		tc::adjacent_unique( std::forward<Rng>(rng), tc::not_fn( tc_move(less) ) )
	)

	template<typename Rng>
	auto ordered_unique(Rng&& rng) noexcept return_decltype (
		tc::ordered_unique( std::forward<Rng>(rng), tc::fn_less() )
	)

	template<typename Rng, typename Less, std::enable_if_t<!std::is_reference<Rng>::value>* =nullptr>
	auto sort_inplace_unique(Rng&& rng, Less less) noexcept code_return_decltype (
		tc::sort_inplace( rng, less );,
		tc::ordered_unique( std::forward<Rng>(rng), tc_move(less) )
	)

	template<typename Rng, std::enable_if_t<!std::is_reference<Rng>::value>* =nullptr>
	auto sort_inplace_unique(Rng&& rng) noexcept return_decltype(
		sort_inplace_unique( std::forward<Rng>(rng), tc::fn_less() )
	)

	template<typename Rng, typename Less>
	auto sort_unique(Rng&& rng, Less less) noexcept return_decltype(
		tc::ordered_unique(tc::sort(std::forward<Rng>(rng), less), less)
	)

	template<typename Rng>
	auto sort_unique(Rng&& rng) noexcept return_decltype(
		sort_unique(std::forward<Rng>(rng), tc::fn_less())
	)

	template< template<typename> class RangeReturn, typename SetType, typename T, typename Less >
	void binary_find_unique(tc::unordered_set<SetType> const& rng, T const& t, Less less) noexcept = delete;

	template< template<typename> class RangeReturn, typename Rng, typename T, typename Less >
	decltype(auto) binary_find_unique(Rng&& rng, T const& t, Less less) noexcept {
		// The result of tc::binary_find_unique must be unambiguous. In general, this means that rng is strictly
		// ordered. In some cases, it is convenient to allow multiple occurrences of the same item in
		// rng, which is not a problem as long as these items are not searched for.
		_ASSERTDEBUG( tc::is_sorted(rng, less) );
		auto it=tc::lower_bound<tc::return_border>( rng, t, std::ref(less) );
		if( it==tc::end( rng ) ) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			auto && ref=*it;
			if (less(t, ref)) {
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			} else {
		#ifdef _CHECKS
				auto itNext = boost::next(it);
				_ASSERT( tc::end(rng)==itNext || less(t, *itNext) );
		#endif
				return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) binary_find_unique(Rng&& rng, T const& t) noexcept {
		return tc::binary_find_unique<RangeReturn>( std::forward<Rng>(rng), t, tc::fn_less() );
	}

	template< template<typename> class RangeReturn, typename Rng, typename T, typename Less >
	decltype(auto) binary_find_first(Rng&& rng, T const& t, Less less) noexcept {
		_ASSERTDEBUG( tc::is_sorted(rng, less) );
		auto it=tc::lower_bound<tc::return_border>( rng, t, less );
		if (it == tc::end(rng)) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			auto && ref = *it;
			if (less(t, ref)) {
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			} else {
				return RangeReturn<Rng>::pack_element(it, std::forward<Rng>(rng), tc_move_if_owned(ref));
			}
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) binary_find_first(Rng&& rng, T const& t) noexcept {
		return tc::binary_find_first<RangeReturn>( std::forward<Rng>(rng), t, tc::fn_less() );
	}

	// would be cleaner to search on the distance metric (starting with lower_bound(rng,0)),
	// but subtraction may cause unnecessary overflows
	template< typename Rng, typename T >
	auto binary_closest(Rng&& rng, T const& t) noexcept {
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
			switch_no_default( boost::implicit_cast<tc::order>(comp( *itA, *itB )) ) { // make sure comp returns tc::order
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
			RETURN_IF_BREAK(boost::implicit_cast<decltype(breakorcontinue)>(tc::continue_if_not_break(fnElementB, *itB)));
			++itB;
		}
		return boost::implicit_cast<decltype(breakorcontinue)>(INTEGRAL_CONSTANT(tc::continue_)());
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
					switch_no_default(m_compare(*argBest.first, *pairitit0.first)) {
			case tc::order::less:
				*itb = false;
				return HasBetterElement(boost::next(itb), argBest, args...);
			case tc::order::equal: {
				bool b = HasBetterElement(boost::next(itb), argBest, args...);
				*itb = !b;
				return b;
			}
			case tc::order::greater:
				*itb = !HasBetterElement(boost::next(itb), pairitit0, args...);
				return true;
					}
				} else {
					*itb = false;
					return HasBetterElement(boost::next(itb), argBest, args...);
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
					*itb = !HasBetterElement(boost::next(itb), pairitit0, args...);
					return true;
				} else {
					*itb = false;
					return FindBest(boost::next(itb), args...);
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

					// C++17: ([](auto& it, bool const b) noexcept {if (b) ++it;}(pairitit.first, tc_at(ab,I)), ...);
					static_cast<void>(std::initializer_list<int> {
						([](auto& it, bool const b) noexcept {if (b) ++it; }(pairitit.first, tc_at(ab, I)), 0)...
					});
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
	decltype(auto) common_prefix(RngRng&& rngrng) noexcept {
		auto&& rngFront = tc_front(rngrng);
		return tc::accumulate(
			tc::drop_first(rngrng),
			tc::take(tc_move_if_owned(rngFront), tc::end(rngFront)),
			[&](auto& rngResult, auto const& rng) noexcept {
				tc::take_inplace(rngResult, boost::mismatch(rngResult, rng).first);
			}
		);
	}
}
