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
#include "for_each_adjacent_tuple.h"

#include "storage_for.h"
#include "functors.h"
#ifdef _DEBUG
#include "scope.h"
#endif
#include "container.h" // tc::vector
#include "for_each_adjacent_tuple.h"
#include "spirit.h"
#include <boost/algorithm/string/compare.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/utility.hpp>
#include <boost/implicit_cast.hpp>
#include <boost/range/algorithm/stable_sort.hpp>

#include <boost/multi_index_container_fwd.hpp>
#include <boost/multi_index/hashed_index_fwd.hpp>
#include <boost/multi_index/ordered_index_fwd.hpp>
#include <boost/intrusive/set.hpp>

#include <type_traits>
#include <set>
#include <map>
#include <utility>

#pragma warning(push)
#pragma warning( disable: 4267 )
// warning C4267 : 'argument' : conversion from 'size_t' to 'int', possible loss of data
// _Median(...) causes warning C4267 when difference_type is int and size_t is 64 bit. 
// Stephan T. Lavavej [stl@exchange.microsoft.com] agrees this is a bug and filed DevDiv#1213041 
// "<algorithm>: _Median() doesn't handle fancy difference types" to track the problem.
#include <algorithm>
#pragma warning(pop)

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

	template<typename T, typename Rng, typename Less, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_strictly_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_pair<T>(rng, [&](auto const& first, auto const& second) noexcept { return tc::continue_if(less(first, second)); });
	}
	template<typename T, typename Rng, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_strictly_sorted(Rng const& rng) noexcept {
		return is_strictly_sorted<T>(rng, tc::fn_less());
	}
	template<typename T, typename Rng, typename Less, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_pair<T>(rng, [&](auto const& first, auto const& second) noexcept { return tc::continue_if(!less(second, first)); });
	}
	template<typename T, typename Rng, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng) noexcept {
		return is_sorted<T>(rng, tc::fn_less());
	}

	template< typename Rng, typename Equal >
	bool all_same(Rng const& rng, Equal equal) noexcept {
		auto const itBegin=boost::begin(rng);
		auto const itEnd=boost::end(rng);
		if(itBegin==itEnd) return true;
		auto const itNext=boost::next(itBegin);
		if(itNext==itEnd) return true;
		auto const& front=*itBegin;
		return all_of(
			tc::drop(rng, itNext),
			[&](auto const& _) noexcept { return equal(front, _); }
		);
	}

	template< typename Rng >
	bool all_same(Rng const& rng) noexcept {
		return all_same(rng, tc::fn_equal_to());
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	decltype(auto) find_unique_if(Rng&& rng, Pred pred) noexcept {
		auto const itEnd=boost::end(rng);
		for( auto it=boost::begin(rng); it!=itEnd; ++it ) {
			auto && ref=*it;
			if( pred(ref) ) {
				_ASSERT( std::none_of( boost::next(it), itEnd, pred ) );
				return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
		}
		return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
	}

	namespace find_first_if_adl_barrier {
		template< template<typename> class RangeReturn >
		struct find_first_if_impl final {
			template< typename Rng, typename Pred >
			decltype(auto) operator()(Rng&& rng, Pred pred) const& MAYTHROW {
				auto const itEnd=boost::end(rng);
				for( auto it=boost::begin(rng); it!=itEnd; ++it ) {
					auto && ref = *it;
					if (pred(ref)) {
						return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
					}
				}
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			}
		};
		template<>
		struct find_first_if_impl<tc::return_bool> final {
			template< typename Rng, typename Pred >
			typename tc::return_bool<Rng>::type operator()(Rng&& rng, Pred&& pred) const {
				return any_of( std::forward<Rng>(rng), std::forward<Pred>(pred) );
			}
		};
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	decltype(auto) find_first_if(Rng&& rng, Pred&& pred) MAYTHROW {
		return find_first_if_adl_barrier::find_first_if_impl<RangeReturn>()( std::forward<Rng>(rng),std::forward<Pred>(pred) );
	}

	namespace find_last_if_adl_barrier {
		template< template<typename> class RangeReturn, typename Rng, typename Pred >
		decltype(auto) find_last_if(Rng&& rng, Pred pred, boost::iterators::bidirectional_traversal_tag) noexcept {
			auto itBegin=boost::begin(rng);
			for( auto it=boost::end(rng); it!=itBegin; ) {
				--it;
				auto && ref = *it;
				if (pred(ref)) return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		}

		template< template<typename> class RangeReturn, typename Rng, typename Pred >
		decltype(auto) find_last_if(Rng&& rng, Pred pred, boost::iterators::forward_traversal_tag) noexcept {
			auto const itEnd=boost::end(rng);
			for( auto itFound=boost::begin(rng); itFound!=itEnd; ++itFound ) {
				tc::array<tc::storage_for<tc::iterator_cache<decltype(itFound)>>, 2> aic;
				int iFound = 0;
				aic[iFound].ctor(itFound);
				scope_exit(aic[iFound].dtor()); //iFound captured by reference
				if (pred(**aic[iFound])) {
					for (auto itNext = boost::next(itFound); itNext!=itEnd; ++itNext) {
						aic[1 - iFound].ctor(itNext);
						if (pred(**aic[1 - iFound])) {
							iFound = 1 - iFound;
						}
						aic[1 - iFound].dtor();
					}
					return RangeReturn<Rng>::pack_element(tc_move(itFound),std::forward<Rng>(rng),*tc_move_always(*aic[iFound]));
				}
			}
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	decltype(auto) find_last_if(Rng&& rng, Pred pred) noexcept {
		return find_last_if_adl_barrier::find_last_if<RangeReturn>(
			std::forward<Rng>(rng),
			tc_move(pred),
			typename boost::range_traversal<std::remove_reference_t<Rng>>::type()
		);
	}

	template < template<typename> class RangeReturn, typename Rng, typename It, typename Pred>
	decltype(auto) find_closest_if(Rng&& rng, It it, bool bSkipSelf, Pred pred) noexcept {
		auto const itEnd = boost::end(rng);
		auto const itBegin = boost::begin(rng);
		auto itForward = it;

		auto OnEnd = [&]() noexcept ->decltype(auto) {
			for (; it != itBegin; ) {
				--it;
				auto && ref = *it;
				if (pred(ref)) {
					return RangeReturn<Rng>::pack_element(it, std::forward<Rng>(rng), tc_move_if_owned(ref));
				}
			}
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		};

		if (itEnd == itForward) {
			return OnEnd();
		}
		if( !bSkipSelf ) {
			auto && ref = *itForward;
			if (pred(ref)) {
				return RangeReturn<Rng>::pack_element(itForward, std::forward<Rng>(rng), tc_move_if_owned(ref));
			}
		}
		++itForward;

		for (;;) {
			if (itBegin == it) {
				for (; itForward != itEnd; ++itForward) {
					auto && ref = *itForward;
					if (pred(ref)) {
						return RangeReturn<Rng>::pack_element(itForward, std::forward<Rng>(rng), tc_move_if_owned(ref));
					}
				}
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			}
			--it;
			{
				auto && ref = *it;
				if (pred(ref)) {
					return RangeReturn<Rng>::pack_element(it, std::forward<Rng>(rng), tc_move_if_owned(ref));
				}
			}
			if (itEnd == itForward) {
				 return OnEnd();
			}
			auto && ref = *itForward;
			if (pred(ref)) {
				return RangeReturn<Rng>::pack_element(itForward, std::forward<Rng>(rng), tc_move_if_owned(ref));
			}
			++itForward;
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) find_unique(Rng&& rng, T const& t) noexcept {
		return find_unique_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) find_first(Rng&& rng, T const& t) noexcept {
		return tc::find_first_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) find_last(Rng&& rng, T const& t) noexcept {
		return tc::find_last_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}


	template<template<typename> class RangeReturn, typename RngWhere, typename RngWhat, typename Pred>
	decltype(auto) search(RngWhere&& rngWhere, RngWhat const& rngWhat, Pred pred) noexcept {
		auto const itWhereEnd = boost::end(rngWhere);
		auto const itWhatBegin = boost::begin(rngWhat);
		auto const itWhatEnd = boost::end(rngWhat);
		for (auto itWhere = boost::begin(rngWhere);; ++itWhere) {
			auto itWhere2 = itWhere;
			auto itWhat = itWhatBegin;
			for (;;) {
				if (itWhat == itWhatEnd) {
					return RangeReturn<RngWhere>::pack_view(std::forward<RngWhere>(rngWhere), tc_move(itWhere), tc_move(itWhere2));
				}
				if (itWhere2 == itWhereEnd) {
					return RangeReturn<RngWhere>::pack_no_element(std::forward<RngWhere>(rngWhere));
				}
				if (!pred(*itWhere2, *itWhat)) break;
				++itWhere2;
				++itWhat;
			}
		}
	}
	template<template<typename> class RangeReturn, typename RngWhere, typename RngWhat>
	decltype(auto) search(RngWhere&& rngWhere, RngWhat const& rngWhat) noexcept {
		return tc::search<RangeReturn>(std::forward<RngWhere>(rngWhere), rngWhat, tc::fn_equal_to());
	}

	/////////////////////////////////
	// associative containers

	// std::set/map returns pair with bool=inserted?
	template< typename It >
	It && verify_inserted(std::pair<It,bool>&& pairitb) noexcept {
		_ASSERT(pairitb.second);
		return tc_move(pairitb.first);
	}

	// std::multiset/multimap always inserts and thus returns only iterator
	template< typename It >
	It && verify_inserted(It&& it) noexcept {
		return std::forward<It>(it);
	}

	template< typename Cont, typename It, std::enable_if_t<!is_instance<std::multiset,Cont>::value && !is_instance<std::multimap,Cont>::value && !is_instance<boost::intrusive::multiset,Cont>::value>* = nullptr >
	It && verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
		return std::forward<It>(it);
	}

	template< typename Cont, typename It, std::enable_if_t<is_instance<std::multiset,Cont>::value || is_instance<std::multimap,Cont>::value || is_instance<boost::intrusive::multiset,Cont>::value>* = nullptr >
	It && verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
#ifdef _DEBUG
		/* standard says: the inserted element has to be placed at upper bound */
		auto itNext = boost::next(it);
		_ASSERTDEBUG(boost::end(cont) == itNext || cont.value_comp()(*it, *itNext));
#endif
		return std::forward<It>(it);
	}

	template< typename Cont, typename TValue > // use extra template parameter instead of Cont::value_type to have both move and copy semantics
	auto cont_must_insert(Cont& cont, TValue&& val) noexcept {
		return verify_inserted( verify_at_upper_bound( cont, NOBADALLOC(cont.insert(std::forward<TValue>(val))) ) );
	}

	DEFINE_FN(insert);
	DEFINE_FN(cont_must_insert);

	namespace explicit_cast_adl_barrier {
		// SConversions cannot implement templated operator()(Rng&&) *and* use operator()(TSource&&)
		// from SDefaultConversions. Apparently, despite the std::enable_if constructs, both are considered
		// to have the same signatures and the using declaration is therefore ignored. Only Clang implements
		// this standard rule, however:
		// http://stackoverflow.com/questions/18861514/using-and-overloading-a-template-member-function-of-a-base-class
		template<typename TTarget>
		struct SSetConversionsHelper {
			// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
			template<typename Rng>
			std::enable_if_t<
				!tc::is_safely_constructible< TTarget, Rng&& >::value // disable for trivial conversions to use move semantic / copy on write where possible
			,TTarget > operator()(Rng&& rng) const& noexcept {
				TTarget cont;
				// force each element to be inserted
				tc::cont_must_insert_range(cont, std::forward<Rng>(rng));
				return cont;
			}
		};

		template<typename TTarget>
		struct SConversions<TTarget, std::enable_if_t<has_mem_fn_lower_bound<TTarget>::value || has_mem_fn_hash_function<TTarget>::value>> final : SDefaultConversions<TTarget>, SSetConversionsHelper<TTarget> {
			using SDefaultConversions<TTarget>::operator();
			using SSetConversionsHelper<TTarget>::operator();
		};

		// SDefaultConversions cannot implement templated operator()(Rng&&) *and* use operator()(TSource&&)
		// from SClassConversions. Apparently, despite the std::enable_if constructs, both are considered
		// to have the same signatures and the using declaration is therefore ignored. Only Clang implements
		// this standard rule, however:
		// http://stackoverflow.com/questions/18861514/using-and-overloading-a-template-member-function-of-a-base-class
		template<typename TTarget>
		struct SVectorConversionsHelper {
			// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
			template<typename Rng>
			std::enable_if_t<
				!tc::is_safely_constructible< TTarget,Rng&& >::value && // disable for trivial conversions to use move semantic / copy on write where possible
				!tc::is_char< typename tc::range_value<TTarget>::type >::value
			,TTarget > operator()(Rng&& rng) const& noexcept {
				TTarget cont;
 				NOEXCEPT(tc::cont_append<tc::return_void>(cont, std::forward<Rng>(rng)));
				return cont;
			}

			// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
			template<typename Rng>
			std::enable_if_t<
				!tc::is_safely_constructible< TTarget,Rng&& >::value && // disable for trivial conversions to use move semantic / copy on write where possible
				tc::is_char< typename tc::range_value<TTarget>::type >::value
			,TTarget > operator()(Rng&& rng) const& noexcept {
				TTarget cont;
				tc::string_range_converter_adl_barrier::template SStringRangeConverter<typename tc::range_value<TTarget>::type,typename tc::range_value< std::remove_reference_t<Rng> >::type>::Append(cont,std::forward<Rng>(rng));
				return cont;
			}
		};

		template<typename TTarget>
		struct SConversions<TTarget, std::enable_if_t<has_mem_fn_push_back<TTarget>::value || has_mem_fn_emplace_back<TTarget>::value>> final : SDefaultConversions<TTarget>, SVectorConversionsHelper<TTarget> {
			using SDefaultConversions<TTarget>::operator();
			using SVectorConversionsHelper<TTarget>::operator();
		};
	}

	template< typename Rng >
	auto make_vector(Rng&& rng) MAYTHROW {
		return tc::explicit_cast<tc::vector<typename tc::range_value<std::remove_reference_t<Rng>>::type>>(std::forward<Rng>(rng));
	}

	DEFINE_FN(make_vector);

	template< typename Rng >
	auto make_basic_string(Rng&& rng) MAYTHROW {
		return tc::explicit_cast<std::basic_string<typename tc::range_value<std::remove_reference_t<Rng>>::type>>(std::forward<Rng>(rng));
	}
	/////////////////////////////////////////////////////
	// sort

	template<typename Rng, typename Pred, std::enable_if_t<has_mem_fn_sort< Rng >::value>* = nullptr>
	Rng& sort_inplace(Rng& rng, Pred&& pred) noexcept {
		rng.sort( std::forward<Pred>(pred) );
		return rng;
	}
	template<typename Rng, typename Pred, std::enable_if_t<!has_mem_fn_sort< Rng >::value>* = nullptr>
	Rng& sort_inplace(Rng& rng, Pred&& pred) noexcept {
		std::sort( boost::begin(rng), boost::end(rng), std::forward<Pred>(pred) );
		return rng;
	}
	template<typename Rng>
	Rng& sort_inplace(Rng& rng) noexcept {
		return tc::sort_inplace( rng, tc::fn_less() );
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

	template<typename Rng, typename Less>
	decltype(auto) sort_unique_range(Rng&& rng, Less less) noexcept {
		return tc::ordered_unique_range( tc::sort( std::forward<Rng>(rng), less ), less );
	}

	template<typename Rng>
	decltype(auto) sort_unique_range(Rng&& rng) noexcept {
		return sort_unique_range( std::forward<Rng>(rng), tc::fn_less() );
	}

	template<typename Rng, typename Less>
	decltype(auto) sort_inplace_unique_range(Rng&& rng, Less less) noexcept {
		tc::sort_inplace( rng, std::ref(less) );
		return tc::ordered_unique_range( std::forward<Rng>(rng), tc_move(less) );
	}

	template<typename Rng>
	decltype(auto) sort_inplace_unique_range(Rng&& rng) noexcept {
		return sort_inplace_unique_range( std::forward<Rng>(rng), tc::fn_less() );
	}

	template<typename Rng, typename Less>
	decltype(auto) stable_sort_unique_range(Rng&& rng, Less less) noexcept {
		boost::stable_sort( rng, less );
		return tc::ordered_unique_range( std::forward<Rng>(rng), tc_move(less) );
	}

	template<typename Rng>
	decltype(auto) stable_sort_unique_range(Rng&& rng) noexcept {
		return stable_sort_unique_range( std::forward<Rng>(rng), tc::fn_less() );
	}

	template< typename Rng, typename Less, typename Accu >
	Rng&& sort_accumulate_each_unique_range(Rng&& cont, Less less, Accu accu) noexcept {
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
						it!=boost::end(rngEqualSubRange);
						++it
					) {
						accu( *boost::begin(rngEqualSubRange), *it );
					}
					rngfilter.keep( boost::begin(rngEqualSubRange) );
				}
			);
		}
		return std::forward<Rng>(cont);
	}

	template< typename Cont, typename Equals = tc::fn_equal_to >
	Cont& front_unique_inplace(Cont & cont, Equals&& pred = Equals()) noexcept {
		{
			tc::range_filter< tc::decay_t<Cont> > rngfilter(cont);
			tc::for_each(
				tc::transform(
					tc::front_unique_range(cont, std::forward<Equals>(pred)),
					[](auto subrange) noexcept { // fn_boost_begin does not work, need subrange as lvalue to get non-const iterator
						return boost::begin(subrange);
					}
				),
				[&](typename boost::range_iterator<Cont>::type it) noexcept { // auto it causes Internal Compiler Error
					rngfilter.keep(it);
				}
			);
		}
		return cont;
	}

	/*
		In contrast to std::unique, tc::adjacent_unique / tc::adjacent_unique_inplace always compares adjacent elements. This allows implementing
		bidirectional tc::adjacent_unique, with tc::adjacent_unique_inplace yielding the same result.
	*/
	template< typename Cont, typename Equals=tc::fn_equal_to >
	Cont& adjacent_unique_inplace( Cont & cont, Equals&& pred=Equals() ) noexcept {
		{
			tc::range_filter< tc::decay_t<Cont> > rngfilter(cont);
			tc::for_each_may_remove_current(
				tc::make_range_of_iterators(tc::adjacent_unique(cont, std::forward<Equals>(pred))),
				[&](auto it) noexcept {
					rngfilter.keep(it.element_base());
				}
			);
		}
		return cont;
	}

	template<typename Cont, typename Less=tc::fn_less>
	Cont& ordered_unique_inplace( Cont& cont, Less less=Less() ) noexcept {
		_ASSERTDEBUG( tc::is_sorted( cont, less ) );
		return tc::adjacent_unique_inplace( cont, tc::not_fn( tc_move(less) ) );
	}

	template< typename Cont, typename Less=tc::fn_less >
	Cont& sort_unique_inplace(Cont& cont, Less less=Less()) noexcept {
		tc::sort_inplace( cont, less );
		return tc::ordered_unique_inplace( cont, tc_move(less) );
	}

	DEFINE_FN( sort_unique_inplace );

	template<typename Rng, typename Less, typename Func>
	auto ordered_for_each_occurrence(Rng&& rng, Less&& less, Func func) noexcept {
		return tc::for_each(tc::ordered_unique_range( std::forward<Rng>(rng), std::forward<Less>(less)), [&](auto const& rngSub) noexcept {
			return tc::continue_if_not_break( func, std::make_pair(
				boost::begin(rngSub),
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

	template< typename Rng >
	auto size_bounded(Rng const& rng, typename boost::range_size<Rng>::type nBound, std::enable_if_t<tc::is_range_with_iterators<Rng>::value>* = nullptr) noexcept return_ctor(
		size_proxy< typename boost::range_size<Rng>::type >, ( advance_forward_bounded( boost::begin(rng), nBound, boost::end(rng) ) )
	)

	template< typename Rng >
	auto size_bounded(Rng const& rng, unsigned int const nBound, std::enable_if_t<!tc::is_range_with_iterators<Rng>::value>* = nullptr) noexcept {
		unsigned int n = 0;
		if (0 < nBound) {
			auto Enumerate = [&](auto const&) noexcept { return tc::continue_if(nBound!=++n); };
			static_assert(std::is_same<tc::break_or_continue, decltype(rng(Enumerate))>::value, "size_bounded only works with interruptible generators");
			tc::for_each(rng, Enumerate);
		}
		return tc::make_size_proxy(n);
	}

	template< typename Rng, typename T >
	bool contains_single(Rng const& rng, T const& t) noexcept {
		return 1==size_bounded(rng,2) && tc_front( rng )==t;
	}

	////////////////////////
	// generic container algorithms

	template <typename T>
	struct range_filter_by_move_element : std::integral_constant<bool,
		tc::is_instance<std::basic_string,T>::value || tc::is_instance<std::vector,T>::value
	> {};

	static_assert(tc::is_instance<std::vector,tc::vector<int>>::value);

	template< typename Cont, typename Rng, std::enable_if_t<!tc::is_view<Cont>::value && !has_mem_fn_lower_bound<Cont>::value && !has_mem_fn_hash_function<Cont>::value>* = nullptr>
	Cont& cont_assign(Cont& cont, Rng&& rng) MAYTHROW {
		tc::assert_no_overlap(cont, std::forward<Rng>(rng));
		tc::cont_clear(cont);
		tc::cont_append<tc::return_void>(cont, std::forward<Rng>(rng)); // MAYTHROW
		return cont;
	}

	template< typename Cont, typename Rng, std::enable_if_t<!tc::is_view<Cont>::value && (has_mem_fn_lower_bound<Cont>::value || has_mem_fn_hash_function<Cont>::value)>* = nullptr>
	Cont& cont_assign(Cont& cont, Rng&& rng) noexcept {
		tc::assert_no_overlap(cont, std::forward<Rng>(rng));
		tc::cont_clear(cont);
		tc::cont_must_insert_range(cont, std::forward<Rng>(rng));
		return cont;
	}

	namespace cont_assign_impl {
		template< typename It >
		struct assign final {
			assign(It itBegin, It itEnd) noexcept
			: m_it(tc_move(itBegin))
			, m_itEnd(tc_move(itEnd)) {}
			
			template< typename Rhs >
			void operator()( Rhs&& rhs ) & noexcept {
				*m_it=std::forward<Rhs>(rhs);
				++m_it;
			}

			~assign() {
				_ASSERTEQUAL( m_it, m_itEnd );
			}
		private:
			It m_it;
			It m_itEnd;
		};

		template<typename RngOut, typename RngIn>
		void copy_cont_assign(RngOut&& rngOut, RngIn&& rngIn) noexcept {
			tc::for_each(std::forward<RngIn>(rngIn), std::ref(tc::as_lvalue(assign<decltype(boost::begin(rngOut))>(boost::begin(rngOut), boost::end(rngOut)))));
		}
	}

	template< typename T, std::size_t N, typename Rng >
	void cont_assign(T (&at)[N], Rng&& rng) noexcept {
		cont_assign_impl::copy_cont_assign(at, rng);
	}

	template< typename View, typename Rng, std::enable_if_t<tc::is_view<std::remove_reference_t<View>>::value>* = nullptr>
	void cont_assign(View&& view, Rng&& rng) noexcept {
		cont_assign_impl::copy_cont_assign(view, rng);
	}

	template<typename Cont, typename Rng, typename Flag>
	void cont_change_with_or(Cont& cont, Rng const& rng, Flag& flag, Flag flagChanged) noexcept {
		_ASSERTINITIALIZED(flag);
		if( flag==flagChanged ) {
			tc::cont_assign(cont, rng);
		} else if( tc::cont_change(cont, rng) ) {
			flag=tc_move(flagChanged);
		}
	}

	template<typename Cont, typename Rng>
	void cont_change_with_or(Cont& cont, Rng const& rng, bool& flag) noexcept {
		cont_change_with_or(cont, rng, flag, true);
	}

	template< typename Cont, typename Rng >
	bool cont_change(Cont& cont, Rng const& rng) noexcept {
		auto itcont=boost::begin(cont);
		auto const itcontEnd=boost::end(cont);
		auto itrng=boost::begin(rng);
		auto const itrngEnd=boost::end(rng);
		for(;;) {
			if( itcont==itcontEnd ) {
				if( itrng==itrngEnd ) {
					return false;
				} else {
					break;
				}
			}
			if( itrng==itrngEnd || !tc::equal_to(*itcont, *itrng) ) {
				tc::take_inplace( cont, itcont );
				break;
			}
			++itcont;
			++itrng;
		}
		tc::cont_append<tc::return_void>(cont,tc::drop(rng,itrng));
		return true;
	}

	template<typename T, std::size_t N, typename Rng>
	bool cont_change(T (&a)[N], Rng const& rng) noexcept {
		auto it = boost::begin(rng);
		bool bChanged = false;
		for(std::size_t i=0; i<N; ++i) {
			_ASSERT(it != boost::end(rng));
			bChanged = tc::change(a[i], *it) || bChanged;
			++it;
		}

		_ASSERT(it == boost::end(rng));
		return bChanged;
	}

	//	renew overload and cont_resize together guarantee that for N element insertions in a sequence of any of
	//		emplace_back
	//		pop_back
	//		insert (at end)
	//		erase (at end)
	//		resize
	//		renew (instead of clear)
	//	- no more than O(N) element moves
	//	- no more than O(log N) memory operations

	template< typename Cont >
	typename boost::range_size< Cont >::type cont_extended_memory(Cont const& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=2) noexcept {
		// factor*cont.size() does not suffice for memory operation guarantee
		// 64 bit is enough to hold any memory money can buy
		return tc::max(n,static_cast< typename Cont::size_type >(static_cast<std::uint64_t>(cont.capacity())*8/5));
	}

	template< typename Cont, std::enable_if_t<!has_mem_fn_reserve<Cont>::value>* = nullptr >
	void cont_reserve( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
	}

	template< typename Cont, std::enable_if_t<has_mem_fn_reserve<Cont>::value>* = nullptr >
	void cont_reserve( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
		if( cont.capacity()<n ) {
			NOEXCEPT( cont.reserve(cont_extended_memory(cont,n) ));
		}
	}

	template< typename Cont, typename... Args >
	Cont& cont_extend_or_truncate( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		tc::cont_reserve(cont, n);
		NOBADALLOC(cont.resize(n, std::forward<Args>(args)...));
		return cont;
	}

	template< typename Cont
#ifdef _CHECKS
		, std::enable_if_t<!has_mem_fn_capacity<Cont>::value>* = nullptr
#endif
	>
	Cont& cont_clear( Cont& cont ) noexcept {
		cont.clear();
		return cont;
	}

#ifdef _CHECKS
	// In some older version of dinkumware's STL, std::vector::clear used to release the allocated memory.
	// Therefore, we did cont.erase(boost::begin(cont), boost::end(cont)) to keep the storage. According to the standard,
	// clear does _not_ change the capacity of a vector, see http://en.cppreference.com/w/cpp/container/vector/clear .
	// It seems that this has been fixed now, so we always use cont.clear() which may be more performant for some containers.
	template< typename Cont, std::enable_if_t<has_mem_fn_capacity<Cont>::value>* = nullptr>
	Cont& cont_clear( Cont& cont ) noexcept {
		auto const nCapacityBefore=cont.capacity();
		cont.clear();
		_ASSERTEQUAL(nCapacityBefore, cont.capacity());
		return cont;
	}
#endif

	template< typename Cont >
	auto safe_cont_erase( Cont& cont, typename boost::range_iterator<Cont const>::type it ) noexcept {
		typename tc::range_value<Cont>::type vt=std::move(*it); // *it may be const&
		return cont.erase(it);
	}

	// safer against reentrance in destructor of value by first moving the value out of the container, then erasing the element in the container and then letting the value go out of scope
	template< typename Cont, std::enable_if_t<tc::range_filter_by_move_element<Cont>::value>* = nullptr >
	Cont& safe_cont_clear( Cont& cont ) noexcept {
		auto it=boost::end(cont);
		auto itBegin=boost::begin(cont);
		if( it!=itBegin ) {
			for(;;) {
				--it;
				if (it==itBegin) break;
				it=tc::safe_cont_erase(cont,it);
			}
			// special treatment of last iteration necessary because it!=itBegin cannot be tested anymore after itBegin has been erased
			tc::safe_cont_erase(cont,it);
		}
		_ASSERT(tc::empty(cont)); // no one put anything into the container during reentrance
		return cont;
	}

	template< typename Cont, std::enable_if_t<!tc::range_filter_by_move_element<Cont>::value>* = nullptr >
	Cont& safe_cont_clear( Cont& cont ) noexcept {
		auto it=boost::begin(cont);
		auto itEnd=boost::end(cont);
		while( it!=itEnd ) {
			it=tc::safe_cont_erase(cont,it);
		}
		_ASSERT(tc::empty(cont)); // no one put anything into the container during reentrance
		return cont;
	}

	template< typename Cont, typename... Args >
	Cont& cont_clear( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n,  Args &&... args) noexcept {
		cont_clear( cont );
		cont_extend( cont, n, std::forward<Args>(args)...);
		return cont;
	}

	template< typename Cont, typename... Args >
	Cont& cont_extend( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		_ASSERT( cont.size()<=n );
		cont_extend_or_truncate( cont, n, std::forward<Args>(args)...);
		return cont;
	}

	template< typename Cont, typename... Args >
	tc::range_reference_t< Cont > cont_extend_at(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		if( cont.size()<=n ) {
			cont_extend( cont, n+1, std::forward<Args>(args)...);
		}
		return cont[n];
	}

	template< typename Cont, typename... Args >
	auto cont_must_emplace_before(Cont& cont, typename boost::range_iterator<Cont const>::type itHint, Args&& ... args) MAYTHROW {
		static_assert(tc::is_safely_constructible<typename tc::range_value<Cont>::type, Args&& ... >::value);
	#ifdef _CHECKS
		auto const c=cont.size();
	#endif
		auto it = NOBADALLOC(cont.emplace_hint(itHint, std::forward<Args>(args)...)); // MAYTHROW
		_ASSERTEQUAL( cont.size(), c+1 );
		_ASSERT( boost::next(it)==itHint );
		return it;
	}

#ifndef __clang__ // compiler bug in VC++ 14.12.25827
	template <typename Cont, typename... T>
	decltype(auto) cont_emplace_back_helper(std::true_type, Cont& cont, T&& ... value) MAYTHROW {
		NOBADALLOC( cont.emplace_back(std::forward<T>(value)...) ); // MAYTHROW
		return tc_back(cont);
	}

	template <typename Cont, typename... T>
	decltype(auto) cont_emplace_back_helper(std::false_type, Cont& cont, T&& ... value) MAYTHROW {
		return NOBADALLOC(cont.emplace_back(std::forward<T>(value)...)); // MAYTHROW
	}
#endif

	template <typename Cont, typename... T, std::enable_if_t<
		has_mem_fn_emplace_back_with_args<Cont, T&& ...>::value && (0==sizeof...(T) || tc::is_safely_constructible<typename tc::range_value<Cont>::type, T&& ... >::value)
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T&& ... value) MAYTHROW {
#ifndef __clang__ // compiler bug in VC++ 14.12.25827
		return cont_emplace_back_helper(
			std::is_void<decltype(cont.emplace_back(std::forward<T>(value)...))>{},
			cont,
			std::forward<T>(value)...
		);
#else
		if constexpr (std::is_void<decltype(cont.emplace_back(std::forward<T>(value)...))>::value) {
			NOBADALLOC( cont.emplace_back(std::forward<T>(value)...) ); // MAYTHROW
			return tc_back(cont);
		} else {
			return NOBADALLOC( cont.emplace_back(std::forward<T>(value)...) ); // MAYTHROW
		}
#endif
	}

	template <typename Cont, typename... T, std::enable_if_t<
		!(has_mem_fn_emplace_back_with_args<Cont, T&& ...>::value && (0==sizeof...(T) || tc::is_safely_constructible<typename tc::range_value<Cont>::type, T&& ... >::value))
		&& has_mem_fn_emplace_back_with_args<Cont, typename tc::range_value<Cont>::type&&>::value 
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T&& ... value) MAYTHROW {
		return cont_emplace_back(cont, tc::explicit_cast<typename tc::range_value<Cont>::type>(std::forward<T>(value)...));
	}

	template <typename Cont, typename... T, std::enable_if_t<has_mem_fn_lower_bound<Cont>::value>* = nullptr>
	auto cont_emplace_back(Cont& cont, T&& ... value) MAYTHROW return_decltype(
		// return_decltype saves having to duplicate the
		//	tc::is_safely_constructible<typename tc::range_value<Cont>::type, T&& ... >::value
		// which is already in cont_must_emplace_before.
		*tc::cont_must_emplace_before(cont, boost::end(cont), std::forward<T>(value)...)
	)

	template <typename Cont, typename T0, typename T1, typename... Ts, std::enable_if_t<!has_mem_fn_emplace_back_with_args<Cont, T0&&, T1&&, Ts&& ...>::value && !has_mem_fn_lower_bound<Cont>::value>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T0&& v0, T1&& v1, Ts&& ... vs) noexcept {
		static_assert(tc::is_safely_constructible<typename tc::range_value<Cont>::type, T0&&, T1&&, Ts&& ... >::value);
		NOBADALLOC( cont.push_back(tc::explicit_cast<typename tc::range_value<Cont>::type>(std::forward<T0>(v0), std::forward<T1>(v1), std::forward<Ts>(vs)...)) );
		return tc_back(cont);
	}

	template <typename Cont, typename T0, std::enable_if_t<!has_mem_fn_emplace_back_with_args<Cont, T0&&>::value && !has_mem_fn_lower_bound<Cont>::value>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T0&& v0) noexcept {
		if constexpr (tc::is_safely_constructible<typename tc::range_value<Cont>::type, T0&& >::value) {
			NOBADALLOC( cont.push_back(std::forward<T0>(v0)) );
		} else {
			NOBADALLOC( cont.push_back(tc::explicit_cast<typename tc::range_value<Cont>::type>(std::forward<T0>(v0))) );
		}
		return tc_back(cont);
	}

	template <typename Cont, std::enable_if_t<!has_mem_fn_emplace_back_with_args<Cont>::value && !has_mem_fn_lower_bound<Cont>::value>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont) noexcept {
		NOBADALLOC( cont.push_back(typename tc::range_value<Cont>::type()) );
		return tc_back(cont);
	}

	// in general, do not use Cont::insert() or Cont(it, it)
	// iterators are slower than for_each in many cases (eg. filter ranges)

	// cont_append for target containers without reserve() member:
	// just run a for_each over the input
	// assume iterators are stable to get iterator to first inserted element
	template< template<typename> class RangeReturn, typename Cont, typename... Rng, std::enable_if_t<!has_mem_fn_reserve<Cont>::value>* = nullptr >
	decltype(auto) cont_append(Cont& cont, Rng&&... rng) MAYTHROW {
		boost::optional<typename boost::range_iterator<Cont>::type> oit;
		if (!tc::empty(cont)) {
			oit = tc::end_prev(cont);
		}
		try {
			static_cast<void>(std::initializer_list<int> {(
				tc::for_each(std::forward<Rng>(rng), [&](auto&& t) MAYTHROW { tc::cont_emplace_back(cont, std::forward<decltype(t)>(t)); })
			, 0)...});
			return RangeReturn<Cont&>::pack_border(
				oit ? boost::next(*oit) : boost::begin(cont),
				cont
			);
		} catch(...) {
			tc::take_inplace(cont, oit ? boost::next(*oit) : boost::begin(cont));
			throw;
		}
	}

	namespace cont_append_impl {
		template< typename Cont, typename Rng, std::enable_if_t<
			tc::is_safely_constructible<typename tc::range_value<Cont>::type, tc::range_reference_t<Rng> >::value
		>* = nullptr >
		void append(Cont& cont, Rng&& rng) MAYTHROW {
			NOBADALLOC(cont.insert(boost::end(cont), boost::begin(rng), boost::end(rng)));
		}
		template< typename Cont, typename Rng, std::enable_if_t<
			!tc::is_safely_constructible<typename tc::range_value<Cont>::type, tc::range_reference_t<Rng> >::value
		>* = nullptr >
		void append(Cont& cont, Rng&& rng) MAYTHROW {
			auto rngTransformed = tc::transform(std::forward<Rng>(rng), tc::fn_explicit_cast<typename tc::range_value<Cont>::type>());
			NOBADALLOC(cont.insert(boost::end(cont), boost::begin(rngTransformed), boost::end(rngTransformed)));
		}
	}

	// cont_append for target containers with reserve() member.
	// If appending random-access iterator range, use Cont::insert() to give insert the opportunity for optimizations
	template< template<typename> class RangeReturn, typename Cont, typename... Rng, std::enable_if_t<has_mem_fn_reserve<Cont>::value>* = nullptr >
	decltype(auto) cont_append(Cont& cont, Rng&&... rng) MAYTHROW {
		auto const nOffset = cont.size();
		auto nReserve=nOffset;
		static_cast<void>(std::initializer_list<int> {([&](auto const& rng_) noexcept {
			if constexpr (tc::size_impl::has_size<decltype(rng_)>::value) {
				nReserve+=tc::size(rng_);
			}
		}(rng), 0)...});
		tc::cont_reserve(cont, nReserve);
		try {
			static_cast<void>(std::initializer_list<int> {([&](auto&& rng_) MAYTHROW {
				if constexpr (tc::is_random_access_range<decltype(rng_)>::value) {
					cont_append_impl::append(cont,std::forward<decltype(rng_)>(rng_));
				} else {
					tc::for_each(std::forward<decltype(rng_)>(rng_), [&](auto&& t) MAYTHROW{ tc::cont_emplace_back(cont, std::forward<decltype(t)>(t)); });
				}
			}(std::forward<Rng>(rng)), 0)...});
			return RangeReturn<Cont&>::pack_border(
				tc::begin_next(cont,nOffset),
				cont
			);
		} catch(...) {
			tc::take_first_inplace(cont, nOffset);
			throw;
		}
	}

	struct no_reference final {};

	namespace cont_find_adl_barrier {
		template< template<typename> class RangeReturn, typename Cont>
		decltype(auto) cont_find_impl(Cont& cont, typename boost::range_iterator< Cont >::type it) noexcept {
			if( it==boost::end(cont) ) {
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
		return cont_find_adl_barrier::cont_find_impl<RangeReturn>(cont, cont.find(std::forward<Arg>(arg)));
	}

#ifdef _DEBUG
	using static_vector_size_t = std::uint32_t; // fixed width integer for shared heap
	namespace static_vector_adl_barrier {
		template< typename T, tc::static_vector_size_t N> struct static_vector;
	}
	using static_vector_adl_barrier::static_vector;
#endif

	namespace get_buffer_adl_barrier {
		template<typename Rng, std::enable_if_t<tc::is_char< typename tc::range_value<Rng>::type >::value>* = nullptr>
		void assert_no_null_terminator(Rng const& rng) noexcept {
			_ASSERT( !tc::find_first<tc::return_bool>(rng, tc::explicit_cast<typename tc::range_value<Rng>::type>('\0') ));
		}

		template<typename Rng, std::enable_if_t<!tc::is_char< typename tc::range_value<Rng>::type >::value>* = nullptr>
		void assert_no_null_terminator(Rng const& rng) noexcept {}

		template<typename Rng>
		void remove_null_terminator(Rng& rng) noexcept {
			static_assert( tc::is_char< typename tc::range_value<Rng>::type >::value );
			_ASSERT( !tc::empty(rng) );
			_ASSERTEQUAL( tc_back(rng), tc::explicit_cast< typename tc::range_value<Rng>::type >('\0') );
			tc::take_inplace(rng,tc::end_prev(rng));
			tc::get_buffer_adl_barrier::assert_no_null_terminator(rng);
		}


#if defined _DEBUG && !defined __clang__
		template<typename Cont>
		struct container_with_sentinel final {
			using type = Cont;
		};

		template<typename T, tc::static_vector_size_t N>
		struct container_with_sentinel<tc::static_vector<T, N>> final {
			using type = tc::static_vector<T, N + 1>;
		};
#endif

		template<typename Cont, typename Func>
		Cont get_truncating_buffer(Func func) noexcept {
			static_assert( tc::is_decayed<Cont>::value );

			// sentinel to detect buffer overrun
			constexpr typename boost::range_size<Cont>::type nSentinel=
#if defined _DEBUG && !defined __clang__
				1;
			typename container_with_sentinel<Cont>::type
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
					tc::assert_no_null_terminator(cont);
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

		template<typename Cont, typename Func>
		Cont get_sized_buffer_may_be_null_terminated(Func func) MAYTHROW {
			static_assert( tc::is_decayed<Cont>::value );

			// sentinel to detect buffer overrun
			constexpr typename boost::range_size<Cont>::type nSentinel=
#if defined _DEBUG && !defined __clang__
				1;
			typename container_with_sentinel<Cont>::type
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

		template<typename Cont, typename Func>
		Cont get_sized_buffer(Func&& func) MAYTHROW {
			auto cont=tc::get_buffer_adl_barrier::get_sized_buffer_may_be_null_terminated<Cont>(std::forward<Func>(func)); // MAYTHROW
			tc::get_buffer_adl_barrier::assert_no_null_terminator(cont);
			return cont;
		}


		template<typename Cont, typename Func>
		Cont get_sized_null_terminated_buffer(Func&& func) MAYTHROW {
			static_assert( tc::is_char< typename tc::range_value<Cont>::type >::value );
			auto cont=tc::get_buffer_adl_barrier::get_sized_buffer_may_be_null_terminated<Cont>(std::forward<Func>(func)); // MAYTHROW
			tc::get_buffer_adl_barrier::remove_null_terminator(cont);
			return cont;
		}
	}
	using get_buffer_adl_barrier::assert_no_null_terminator;
	using get_buffer_adl_barrier::remove_null_terminator;
	using get_buffer_adl_barrier::get_truncating_buffer;
	using get_buffer_adl_barrier::get_sized_buffer_may_be_null_terminated;
	using get_buffer_adl_barrier::get_sized_buffer;
	using get_buffer_adl_barrier::get_sized_null_terminated_buffer;

	template< typename Cont, typename Rng >
	Cont& cont_try_insert_range(Cont& cont, Rng&& rng) noexcept {
		/*
			It might be more efficient for a container to insert a range at once
			with insert(begin(rng),end(rng)), but on the other hand, it is more
			efficient to access a range as generator range: If filters and costly
			transforms are involved, a generator range needs to dereference only once.
		*/
		tc::for_each(
			std::forward<Rng>(rng),
			[&](auto&& _) noexcept { cont.insert(std::forward<decltype(_)>(_)); }
		);
		return cont;
	}

	template< typename Cont, typename Rng >
	Cont& cont_must_insert_range(Cont& cont, Rng&& rng) noexcept {
		tc::for_each(
			std::forward<Rng>(rng),
			[&](auto&& _) noexcept { tc::cont_must_insert(cont, std::forward<decltype(_)>(_)); }
		);
		return cont;
	}

	template< typename Cont, typename... Args >
	auto cont_must_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		return verify_inserted( verify_at_upper_bound(
			cont,
			NOBADALLOC(cont.emplace(std::forward<Args>(args)...)) // MAYTHROW
		));
	}

	template< typename Cont, typename... Args >
	auto cont_try_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		return NOBADALLOC(cont.emplace(std::forward<Args>(args)...)); // MAYTHROW
	}

	template<typename... MapArgs, typename K, typename V, typename Better>
	void map_try_emplace_better(std::map<MapArgs...>& map, K&& key, V&& val, Better&& better) noexcept {
		auto it = map.lower_bound(key);
		if (boost::end(map) == it || map.key_comp()(key, it->first)) {
			NOEXCEPT( tc::cont_must_emplace_before(map, tc_move(it), std::forward<K>(key), std::forward<V>(val)) );
		} else {
			tc::assign_better(it->second, std::forward<V>(val), std::forward<Better>(better));
		}
	}
	
	template< typename... MapArgs, typename K, typename... MappedTypeCtorArgs >
	auto map_try_emplace_with_key(std::map<MapArgs...>& map, K&& key, MappedTypeCtorArgs&& ... mappedtypectorargs) MAYTHROW {
		// TODO C++17: Use std::map::try_emplace
		auto it = map.lower_bound(key);
		if (boost::end(map)==it || map.key_comp()(key, it->first)) {
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
		if (boost::end(map)==it) {
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
		if (boost::end(ordered_index)==it || ordered_index.key_comp()(key, ordered_index.key_extractor()(*it))) {
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

	template<typename Cont, typename Enable=void>
	struct range_filter;

	template<typename Cont>
	struct range_filter<Cont, std::enable_if_t< 
		has_efficient_erase<Cont>::value
		|| has_mem_fn_lower_bound<Cont>::value
		|| has_mem_fn_hash_function<Cont>::value
	> >: tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value );
		using iterator = typename boost::range_iterator<Cont>::type;
		using const_iterator = iterator; // no deep constness (analog to sub_range)

	private:
		Cont& m_cont;
		iterator m_itOutputEnd;

	public:
		explicit range_filter(Cont& cont) noexcept
			: m_cont(cont)
			, m_itOutputEnd(boost::begin(cont))
		{}

		range_filter(Cont& cont, iterator const& itStart) noexcept
			: m_cont(cont)
			, m_itOutputEnd(itStart)
		{}

		~range_filter() {
			tc::take_inplace( m_cont, m_itOutputEnd );
		}

		void keep(iterator it) & noexcept {
			_ASSERT( 0<=std::distance(m_itOutputEnd,it) );
			m_itOutputEnd=m_cont.erase(m_itOutputEnd,it);
			++m_itOutputEnd;
		}

		///////////////////////////////////////////
		// range interface for output range
		// no deep constness (analog to sub_range)

		iterator begin() const& noexcept {
			return boost::begin(tc::as_mutable(m_cont));
		}

		iterator end() const& noexcept {
			return m_itOutputEnd;
		}

		template< typename Cont2=Cont, std::enable_if_t<!has_mem_fn_hash_function<Cont2>::value>* = nullptr>
		void pop_back() & noexcept {
			_ASSERT( m_itOutputEnd!=boost::begin(m_cont) );
			--m_itOutputEnd;
			m_itOutputEnd=m_cont.erase(m_itOutputEnd);
		}
	};

	template<typename Cont>
	struct range_filter< Cont, std::enable_if_t<
		has_mem_fn_splice_after< Cont >::value
	> >: Cont, private tc::noncopyable {
		static_assert(tc::dependent_false<Cont>::value, "Careful: currently unused and without unit test");

		static_assert( tc::is_decayed< Cont >::value );
		using typename Cont::iterator;
		using const_iterator = iterator; // no deep constness (analog to sub_range)

	private:
		Cont& m_contInput;
		iterator m_itLastOutput;

	public:
		explicit range_filter(Cont& cont) noexcept
			: m_contInput(cont)
			, m_itLastOutput(before_begin())
		{}

		explicit range_filter(Cont& cont, iterator const& itStart) noexcept
			: range_filter(cont)
		{
			for(;;) {
				auto it=boost::begin(m_contInput);
				if( it==itStart ) break;
				this->splice_after(m_itLastOutput,m_contInput.before_begin());
				m_itLastOutput=it;
			}
		}

		~range_filter() {
			m_contInput=tc_move_always( tc::base_cast<Cont>(*this) );
		}

		void keep(iterator it) & noexcept {
			while( it!=boost::begin(m_contInput) ) m_cont.pop_front();
			this->splice_after(m_itLastOutput,m_contInput.before_begin());
			m_itLastOutput=it;
		}
	};

	template<typename Cont>
	struct range_filter< Cont, std::enable_if_t<
		has_mem_fn_splice<Cont >::value
	> >: Cont, private tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value );
		Cont& m_contInput;
		using typename Cont::iterator;
		using const_iterator = iterator; // no deep constness (analog to sub_range)

		explicit range_filter(Cont& cont) noexcept
			: m_contInput(cont)
		{}

		range_filter(Cont& cont, iterator const& itStart) noexcept
			: m_contInput(cont)
		{
			this->splice( boost::end(*this), m_contInput, boost::begin(m_contInput), itStart );
		}

		~range_filter() {
			m_contInput=tc_move_always( tc::base_cast<Cont>(*this) );
		}

		void keep(iterator it) & noexcept {
			_ASSERT( it!=boost::end(m_contInput) );
			this->splice( 
				boost::end(*this),
				m_contInput,
				m_contInput.erase( boost::begin(m_contInput), it )
			);
		}
	};

	template<typename Cont>
	struct range_filter<
		Cont,
		std::enable_if_t<range_filter_by_move_element<Cont>::value>
	>: tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value );
		using iterator = typename boost::range_iterator<Cont>::type;
		using const_iterator = iterator; // no deep constness (analog to sub_range)

	protected:
		Cont& m_cont;
		iterator m_itOutput;

	private:
#ifdef _CHECKS
		iterator m_itFirstValid;
#endif

	public:
		explicit range_filter(Cont& cont) noexcept
			: m_cont(cont)
			, m_itOutput(boost::begin(cont))
#ifdef _CHECKS
			, m_itFirstValid(boost::begin(cont))
#endif
		{}

		range_filter(Cont& cont, iterator itStart) noexcept
			: m_cont(cont)
			, m_itOutput(itStart)
#ifdef _CHECKS
			, m_itFirstValid(itStart)
#endif
		{}

		~range_filter() {
			tc::take_inplace( m_cont, m_itOutput );
		}

		void keep(iterator it) & noexcept {
#ifdef _CHECKS
			// Filter without reordering 
			_ASSERT( 0<=std::distance(m_itFirstValid,it) );
			m_itFirstValid=it;
			++m_itFirstValid;
#endif
			if (it != m_itOutput) { // self assignment with r-value-references is not allowed (17.6.4.9)
				*m_itOutput=tc_move_always(*it);
			}
			++m_itOutput;
		}

		///////////////////////////////////
		// range interface for output range
		// no deep constness (analog to sub_range)

		iterator begin() const& noexcept {
			return boost::begin(tc::as_mutable(m_cont));
		}

		iterator end() const& noexcept {
			return m_itOutput;
		}

		void pop_back() & noexcept {
			_ASSERT( boost::begin(m_cont)!=m_itOutput );
			--m_itOutput;
		}

		template <typename... Ts>
		void emplace_back(Ts&&... ts) & noexcept {
			_ASSERT( boost::end(m_cont)!=m_itOutput );
			tc::renew(*m_itOutput, std::forward<Ts>(ts)...);
			++m_itOutput;
		}
	};

	template<typename Cont>
	struct range_filter<
		tc::sub_range< Cont& >,
		std::enable_if_t<range_filter_by_move_element<Cont>::value>
	> {
		using iterator = typename boost::range_iterator<Cont>::type;
		using const_iterator = iterator; // no deep constness (analog to sub_range)

		range_filter(tc::sub_range< Cont& >& rng) noexcept : m_rng(rng)	{
			_ASSERT(boost::end(m_rng)==boost::end(Container())); // otherwise, we would need to keep [ end(m_rng), end(Container()) ) inside dtor
			m_orngfilter.ctor(Container(), boost::begin(rng));
		}

		void keep(iterator it) & noexcept {
			m_orngfilter->keep(it);
		}

		iterator begin() const& noexcept {
			return boost::begin(m_rng);
		}

		iterator end() const& noexcept {
			return boost::end(*m_orngfilter);
		}

		void pop_back() & noexcept {
			_ASSERT(boost::end(*this)!=boost::begin(*this));
			m_orngfilter->pop_back();
		}

		~range_filter() {
			auto& cont=Container();
			auto const nIndexBegin=boost::begin(m_rng)-boost::begin(cont);
			m_orngfilter.dtor(); // erases cont tail and invalidates iterators in m_rng
			m_rng=tc::drop_first(cont, nIndexBegin);
		}
	private:
		Cont& Container() const& noexcept {
			return boost::implicit_cast<Cont&>(m_rng.base_range());
		}

		tc::sub_range< Cont& >& m_rng;
		tc::storage_for< tc::range_filter<Cont> > m_orngfilter;
	};

	/////////////////////////////////////////////////////
	// filter_inplace

	template<typename Cont, typename Pred>
	Cont& filter_inplace(Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it, Pred pred) noexcept {
		for (auto const itEnd = boost::end(cont); it != itEnd; ++it) {
			if (!tc::bool_cast(pred(*it))) {
				tc::range_filter< tc::decay_t<Cont> > rngfilter(cont, it);
				++it;
				while (it != itEnd) {
					if (pred(*it)) {
						rngfilter.keep(it++); // may invalidate it, so move away first
					}
					else {
						++it;
					}
				}
				break;
			}
		}
		return cont;
	}

	template<typename Cont, typename Pred>
	Cont& filter_inplace(Cont& cont, Pred&& pred) noexcept {
		return tc::filter_inplace( cont, boost::begin(cont), std::forward<Pred>(pred) );
	}

	// cannot use list::remove because T may not be list::value_type
	// cannot use key-based lookup for set/map because T may not be Cont::value_type and !Cont::predicate()(a,b) && !Cont::predicate()(b,a) may not be the same as ==
	template<typename Cont, typename T, std::enable_if_t<!std::is_base_of<x3::parser_base, T>::value>* = nullptr>
	Cont& remove_inplace(Cont& cont, T const& t) noexcept {
		return tc::filter_inplace( cont, [&](auto const& _) noexcept { return !tc::equal_to(_, t); } );
	}

	template<typename Cont, typename Expr, std::enable_if_t<std::is_base_of<x3::parser_base, Expr>::value>* = nullptr>
	Cont& remove_inplace(Cont& cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it, Expr const& expr) noexcept {
		for(auto const itEnd = boost::end(cont); it != itEnd; ++it) {
			auto itBegin=it;
			if(tc::parse_iterator(it, itEnd, expr)) {
				tc::range_filter< tc::decay_t<Cont> > rngfilter(cont, itBegin);
				while(it != itEnd) {
					if(!tc::parse_iterator(it, itEnd, expr)) {
						rngfilter.keep(it++);  // may invalidate it, so move away first
					}
				}
				break;
			}
		}
		return cont;
	}

	template<typename Cont, typename Expr, std::enable_if_t<std::is_base_of<x3::parser_base, Expr>::value>* = nullptr>
	Cont& remove_inplace(Cont& cont, Expr const& expr) noexcept {
		return tc::remove_inplace(cont, boost::begin(cont), expr);
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
	Rng&& reverse_inplace(Rng&& rng) noexcept {
		rng.reverse();
		return std::forward<Rng>(rng);
	}
	template<typename Rng, std::enable_if_t<!has_mem_fn_reverse< std::remove_reference_t<Rng> >::value>* = nullptr>
	Rng&& reverse_inplace(Rng&& rng) noexcept {
		std::reverse(boost::begin(rng), boost::end(rng));
		return std::forward<Rng>(rng);
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

	template<typename Rng, typename Less>
	auto sort_inplace_unique(Rng&& rng, Less less) noexcept code_return_decltype (
		tc::sort_inplace( rng, less );,
		tc::ordered_unique( std::forward<Rng>(rng), tc_move(less) )
	)

	template<typename Rng>
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
		if( it==boost::end( rng ) ) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			auto && ref=*it;
			if (less(t, ref)) {
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			} else {
		#ifdef _CHECKS
				auto itNext = boost::next(it);
				_ASSERT( boost::end(rng)==itNext || less(t, *itNext) );
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
		if (it == boost::end(rng)) {
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
		if( boost::begin(rng)==it ) {
			return it;
		} else if( boost::end(rng)==it ) {
			return boost::prior(it);
		} else {
			auto itPrior = boost::prior(it);
			return (t - *itPrior) < (*it - t) ? itPrior : it;
		}
	}

	template< typename RngA, typename RngB, typename Comp, typename FuncElementA, typename FuncElementB, typename FuncElementBoth > 
	auto interleave_may_remove_current(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB, FuncElementBoth fnElementBoth) noexcept -> tc::common_type_t<
		decltype(tc::continue_if_not_break(fnElementA, *boost::begin(rngA))),
		decltype(tc::continue_if_not_break(fnElementB, *boost::begin(rngB))),
		decltype(tc::continue_if_not_break(fnElementBoth, *boost::begin(rngA), *boost::begin(rngB))),
		decltype(tc::for_each_may_remove_current(tc::drop(rngA, boost::begin(rngA)), fnElementA)),
		decltype(tc::for_each_may_remove_current(tc::drop(rngB, boost::begin(rngB)), fnElementB))
	> {
		auto itA=boost::begin(rngA);
		auto itEndA=boost::end(rngA);
		auto itB=boost::begin(rngB);
		auto itEndB=boost::end(rngB);
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
		auto itB=boost::begin(rngB);
		auto itEndB=boost::end(rngB);

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

	template<typename Compare>
	struct SInterleaveImpl {
		Compare m_compare;

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
						bool b=HasBetterElement(boost::next(itb), argBest, args...);
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

		template<
			typename Compare
		>
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

			while (FindBest(boost::begin(ab), pairitit...)) {
				RETURN_IF_BREAK(tc::continue_if_not_break(func,
					std::make_pair(pairitit.first, tc_at(ab, I))...
				));

				// C++17: ([](auto& it, bool const b) noexcept {if (b) ++it;}(pairitit.first, tc_at(ab,I)), ...);
				static_cast<void>(std::initializer_list<int> {
					([](auto& it, bool const b) noexcept {if (b) ++it;}(pairitit.first, tc_at(ab,I)), 0)...
				});
			}
			return tc::continue_;
		}
	};

	template<
		typename Compare,
		typename Func,
		typename... Rng
	>
	tc::break_or_continue interleave_n(Compare&& compare, Func&& func, Rng&&... rng) noexcept {
		return SInterleaveImpl<tc::decay_t<Compare>>(std::forward<Compare>(compare))(
			std::forward<Func>(func),
			std::index_sequence_for<Rng...>(),
			std::make_pair(
				boost::begin(rng),
				boost::end(rng)
			)...
		);
	}

	namespace best_element_adl_barrier {
		template <typename RangeReturn>
		struct is_returning_value final {
			static constexpr bool value = tc::is_instance<tc::return_value, RangeReturn>::value
				|| tc::is_instance<tc::return_value_or_default, RangeReturn>::value
				|| tc::is_instance<tc::return_value_or_none, RangeReturn>::value
			;
		};
	}

	template< template<typename> class RangeReturn, typename Rng, typename Less, std::enable_if_t<best_element_adl_barrier::is_returning_value<RangeReturn<Rng>>::value>* = nullptr >
	typename RangeReturn<Rng>::type best_element(Rng&& rng, Less&& less) MAYTHROW {
		if (auto ovalue = tc::accumulate_with_front(rng, tc::fn_assign_better(std::forward<Less>(less)))) {
			return RangeReturn<Rng>::pack_element(std::forward<Rng>(rng), *tc_move(ovalue));
		} else {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename Less, std::enable_if_t<!best_element_adl_barrier::is_returning_value<RangeReturn<Rng>>::value>* = nullptr >
	decltype(auto) best_element(Rng&& rng, Less less) MAYTHROW {
		auto const itEnd=boost::end(rng); // MAYTHROW
		decltype(boost::begin(rng)) ait[2]={ boost::begin(rng) }; // MAYTHROW
		if(ait[0]==itEnd) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			tc::storage_for< tc::reference_or_value<tc::range_reference_t< Rng >> > aoref[2];
			aoref[0].ctor( aggregate_tag(), *ait[0] ); // MAYTHROW
			for(;;){
				for( int i=0; i!=2; ++i ) { // we expect the compiler to unroll this loop
					// aoref[i] is constructed, aoref[1-i] is not constructed
					scope_exit( aoref[i].dtor() ); // also required in case of exception
					ait[1-i]=ait[i];
					for(;;) {
						// aoref[i] is constructed, aoref[1-i] is not constructed
						++ait[1-i];
						if(ait[1-i]==itEnd) {
							return RangeReturn<Rng>::pack_element(tc_move_always(ait[i]),std::forward<Rng>(rng),**tc_move_always(aoref[i]));
						}
						aoref[1-i].ctor( aggregate_tag(), *ait[1-i] ); // MAYTHROW
						try {
							if( less(**aoref[1-i],**aoref[i]) ) { // MAYTHROW
								break; // only path where aoref[1-i] is not destroyed
							}
						} catch(...) {
							aoref[1-i].dtor();
							throw;
						}
						aoref[1-i].dtor();
					}
				}
			}
		}
	}

	template< template<typename> class RangeReturn, typename Rng >
	decltype(auto) min_element(Rng&& rng) MAYTHROW {
		return best_element<RangeReturn>(std::forward<Rng>(rng), tc::fn_less());
	}

	template< template<typename> class RangeReturn, typename Rng >
	decltype(auto) max_element(Rng&& rng) MAYTHROW {
		return best_element<RangeReturn>(std::forward<Rng>(rng), tc::fn_greater());
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) closest_element(Rng&& rng, T const& t) MAYTHROW {
		return best_element<RangeReturn>(std::forward<Rng>(rng), tc::projected(tc::fn_less(), [&](T const& t1) MAYTHROW { return std::abs(t - t1); }));
	}

	template <typename RngRng>
	decltype(auto) common_prefix(RngRng&& rngrng) noexcept {
		auto&& rngFront = tc_front(rngrng);
		return tc::accumulate(
			tc::drop_first(rngrng),
			tc::take(tc_move_if_owned(rngFront), boost::end(rngFront)),
			[&](auto& rngResult, auto const& rng) noexcept {
				tc::take_inplace(rngResult, boost::mismatch(rngResult, rng).first);
			}
		);
	}
}
