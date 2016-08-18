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

#include "storage_for.h"
#include "functors.h"
#ifdef _DEBUG
#include "scope.h"
#endif
#include "container.h" // tc::vector
#include <boost/algorithm/string/compare.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/utility.hpp>
#include <boost/implicit_cast.hpp>
#include <boost/filesystem.hpp>
#include <boost/range/algorithm/stable_sort.hpp>

#include <boost/multi_index_container_fwd.hpp>

#include <type_traits>
#include <set>
#include <map>
#include <unordered_set>
#include <unordered_map>

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
		return tc::continue_==tc::for_each_adjacent_tuple<2>(rng,[&](auto const& first, auto const& second){ return tc::continue_if(less(first,second)); });
	}
	template< typename Rng, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_strictly_sorted(Rng const& rng) noexcept {
		return is_strictly_sorted( rng, tc::fn_less());
	}
	template< typename Rng, typename Less, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_tuple<2>(rng, [&](auto const& first, auto const& second) { return tc::continue_if(!less(second, first)); });
	}
	template< typename Rng, std::enable_if_t< is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng) noexcept {
		return is_sorted(rng, tc::fn_less() );
	}

	template<typename T, typename Rng, typename Less, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_strictly_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_pair<T>(rng, [&](auto const& first, auto const& second) { return tc::continue_if(less(first, second)); });
	}
	template<typename T, typename Rng, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_strictly_sorted(Rng const& rng) noexcept {
		return is_strictly_sorted<T>(rng, tc::fn_less());
	}
	template<typename T, typename Rng, typename Less, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng, Less&& less) noexcept {
		return tc::continue_ == tc::for_each_adjacent_pair<T>(rng, [&](auto const& first, auto const& second) { return tc::continue_if(!less(second, first)); });
	}
	template<typename T, typename Rng, std::enable_if_t< !is_range_with_iterators<Rng>::value >* = nullptr >
	bool is_sorted(Rng const& rng) noexcept {
		return is_sorted<T>(rng, tc::fn_less());
	}

	template< typename Rng, typename Equal >
	bool all_same(Rng const& rng, Equal equal) noexcept {
		return tc::empty(rng)
			|| all_of(
				tc::drop_first(rng),
				std::bind(equal, *boost::begin(rng), std::placeholders::_1)
			);
	}

	template< typename Rng >
	bool all_same(Rng const& rng) noexcept {
		return all_same(rng, tc::fn_equal_to());
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	typename RangeReturn<Rng>::type find_unique_if(Rng&& rng, Pred pred) noexcept {
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
			typename RangeReturn<Rng>::type operator()(Rng&& rng, Pred pred) const& MAYTHROW {
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
	typename RangeReturn<Rng>::type find_first_if(Rng&& rng, Pred&& pred) MAYTHROW {
		return find_first_if_adl_barrier::find_first_if_impl<RangeReturn>()( std::forward<Rng>(rng),std::forward<Pred>(pred) );
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	typename RangeReturn<Rng>::type find_last_if(Rng&& rng, Pred pred) noexcept {
		auto itBegin=boost::begin(rng);
		for( auto it=boost::end(rng); it!=itBegin; ) {
			--it;
			auto && ref = *it;
			if (pred(ref)) return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
		}
		return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
	}

	template < template<typename> class RangeReturn, typename Rng, typename It, typename Pred>
	typename RangeReturn<Rng>::type find_closest_if(Rng&& rng, It it, Pred pred) noexcept {
		auto const itEnd = boost::end(rng);
		auto const itBegin = boost::begin(rng);
		auto itForward = it;
		for (;;) {
			if (itEnd == itForward) { 
				for (; it != itBegin; ) {
					--it;
					auto && ref = *it;
					if (pred(ref)) return RangeReturn<Rng>::pack_element(it, std::forward<Rng>(rng), tc_move_if_owned(ref));
				}
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			}
			{
				auto && ref = *itForward;
				if (pred(ref)) {
					return RangeReturn<Rng>::pack_element(itForward, std::forward<Rng>(rng), tc_move_if_owned(ref));
				}
				++itForward;
			}
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
			auto && ref = *it;
			if (pred(ref)) {
				return RangeReturn<Rng>::pack_element(it, std::forward<Rng>(rng), tc_move_if_owned(ref));
			}
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	typename RangeReturn<Rng>::type find_unique(Rng&& rng, T const& t) noexcept {
		return find_unique_if<RangeReturn>( std::forward<Rng>(rng), std::bind( tc::fn_equal_to(), std::placeholders::_1, std::cref(t) ) );
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	typename RangeReturn<Rng>::type find_first(Rng&& rng, T const& t) noexcept {
		return tc::find_first_if<RangeReturn>( std::forward<Rng>(rng), std::bind( tc::fn_equal_to(), std::placeholders::_1, std::cref(t) ) );
	}

	DEFINE_FN_TMPL(find_first, (template<typename> class) )
	DEFINE_FN_TMPL(find_unique, (template<typename> class) )

	template< template<typename> class RangeReturn, typename Rng, typename T >
	typename RangeReturn<Rng>::type find_last(Rng&& rng, T const& t) noexcept {
		return tc::find_last_if<RangeReturn>( std::forward<Rng>(rng), std::bind( tc::fn_equal_to(), std::placeholders::_1, std::cref(t) ) );
	}

	template<typename Rng>
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type plurality_element(Rng&& rng) noexcept {
		_ASSERT( !tc::empty(rng) );
		using TVec = decltype(tc::sorted_iterator_range(rng, tc::fn_less()));

		return *( tc::accumulate(
			std::bind( tc::fn_ordered_for_each_occurrence(), tc::sorted_iterator_range( rng, tc::fn_less() ), tc::projected(tc::fn_less(), fn_indirection() ), std::placeholders::_1 ),
			std::pair<
				typename boost::range_iterator<TVec const>::type,
				typename boost::range_size< TVec >::type
			>(), // value-initialized, second=0
			std::bind( tc::fn_assign_better(), std::placeholders::_1, std::placeholders::_2, tc::projected(tc::fn_greater(), dot_member_second() ) )
		).first );
	}

	template< typename Rng, typename Pred >
	typename tc::return_drop_before_element_or_empty<Rng>::type trim_left_if(Rng&& rng, Pred&& pred) noexcept {
		return tc::find_first_if<tc::return_drop_before_element_or_empty>( std::forward<Rng>(rng), tc::not_fn(std::forward<Pred>(pred)) );
	}

	template< typename Rng, typename Pred >
	typename tc::return_take_after_element_or_empty<Rng>::type trim_right_if(Rng&& rng, Pred&& pred) noexcept {
		return tc::find_last_if<tc::return_take_after_element_or_empty>( std::forward<Rng>(rng), tc::not_fn(std::forward<Pred>(pred)) );
	}

	template< typename Rng, typename Pred >
	typename tc::return_drop_before_element_or_empty<typename tc::return_take_after_element_or_empty<Rng>::type>::type trim_if(Rng&& rng, Pred&& pred) noexcept {
		auto rngTrimmed = tc::trim_right_if( std::forward<Rng>(rng), pred );
		return tc::trim_left_if( tc_move(rngTrimmed), std::forward<Pred>(pred) );
	}

	template< typename Rng, typename RngTrim >
	typename tc::return_drop_before_element_or_empty<Rng>::type trim_left(Rng&& rng, RngTrim const& rngTrim) noexcept {
		return tc::trim_left_if( std::forward<Rng>(rng), std::bind( tc::fn_find_first<tc::return_bool>(), std::cref(rngTrim), std::placeholders::_1 ) );
	}

	template< typename Rng, typename RngTrim >
	typename tc::return_take_after_element_or_empty<Rng>::type trim_right(Rng&& rng, RngTrim const& rngTrim) noexcept {
		return tc::trim_right_if( std::forward<Rng>(rng), std::bind( tc::fn_find_first<tc::return_bool>(), std::cref(rngTrim), std::placeholders::_1 ) );
	}

	template< typename Rng, typename RngTrim >
	typename tc::return_drop_before_element_or_empty<typename tc::return_take_after_element_or_empty<Rng>::type>::type trim(Rng&& rng, RngTrim const& rngTrim) noexcept {
		return tc::trim_if( std::forward<Rng>(rng), std::bind( tc::fn_find_first<tc::return_bool>(), std::cref(rngTrim), std::placeholders::_1 ) );
	}

	template< typename Rng >
	auto size_bounded(Rng const& rng, typename boost::range_size<Rng>::type nBound, std::enable_if_t<tc::is_range_with_iterators<Rng>::value>* = nullptr) noexcept return_ctor(
		size_proxy< typename boost::range_size<Rng>::type >, ( advance_forward_bounded( boost::begin(rng), nBound, boost::end(rng) ) )
	)

	template< typename Rng >
	auto size_bounded(Rng const& rng, unsigned int const nBound, std::enable_if_t<!tc::is_range_with_iterators<Rng>::value>* = nullptr) noexcept {
		unsigned int n = 0;
		if (0 < nBound) {
			auto Enumerate = [&](auto const&){ return tc::continue_if(nBound!=++n); };
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
		tc::is_instance<std::basic_string,T>::value || tc::is_instance<boost::container::vector,T>::value || tc::is_instance<boost::container::deque,T>::value
	> {};

	static_assert(tc::is_instance<boost::container::vector,tc::vector<int>>::value,"");

	template< typename Cont, typename Rng, std::enable_if_t<!has_mem_fn_lower_bound<Cont>::value && !has_mem_fn_hash_function<Cont>::value>* = nullptr>
	Cont& cont_assign(Cont& cont, Rng&& rng) MAYTHROW {
		tc::cont_clear(cont);
		tc::cont_append<tc::return_void>(cont, std::forward<Rng>(rng)); // MAYTHROW
		return cont;
	}

	template< typename Cont, typename Rng, std::enable_if_t<has_mem_fn_lower_bound<Cont>::value || has_mem_fn_hash_function<Cont>::value>* = nullptr>
	Cont& cont_assign(Cont& cont, Rng&& rng) noexcept {
		tc::cont_clear(cont);
		tc::cont_must_insert_range(cont, std::forward<Rng>(rng));
		return cont;
	}

	namespace cont_assign_impl {
		template< typename T, std::size_t N >
		struct assign final {
			assign(T (&at)[N]) noexcept
			: m_at(at) {}
			
			template< typename Rhs >
			void operator()( Rhs&& rhs ) & noexcept {
				m_at[m_i]=std::forward<Rhs>(rhs);
				++m_i;
			}

			~assign() {
				_ASSERTEQUAL( m_i, N );
			}
		private:
			T (&m_at)[N];
			std::size_t m_i = 0;
		};
	}

	template< typename T, std::size_t N, typename Rng >
	void cont_assign(T (&at)[N], Rng&& rng) noexcept {
		for_each( std::forward<Rng>(rng), std::ref(tc::as_lvalue(cont_assign_impl::assign<T,N>(at))) );
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

	template< typename T, typename A, typename Rng >
	bool cont_change(tc::vector<T,A>& vec, Rng const& rng) noexcept {
		typename tc::vector<T,A>::iterator itvec=boost::begin(vec);
		auto itrng=boost::const_begin(rng);
		for(;;) {
			if( itvec==boost::end(vec) ) {
				if( itrng==boost::const_end(rng) ) {
					return false;
				} else {
					break;
				}
			}
			if( itrng==boost::const_end(rng) || !tc::bool_cast(*itvec==*itrng) ) {
				tc::take_inplace( vec, itvec );
				break;
			}
			++itvec;
			++itrng;
		}
		tc::cont_append<tc::return_void>(vec,tc::drop(rng,itrng));
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
	typename boost::range_size< Cont >::type cont_extended_memory(Cont const& cont) noexcept {
		// factor*cont.size() does not suffice for memory operation guarantee
		// 64 bit is enough to hold any memory money can buy
		return static_cast< typename Cont::size_type >(
			static_cast<std::uint64_t>(cont.capacity())*8/5
		);
	}

	template< typename Cont, std::enable_if_t<!has_mem_fn_reserve<Cont>::value>* = nullptr >
	void cont_reserve( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
	}

	template< typename Cont, std::enable_if_t<has_mem_fn_reserve<Cont>::value>* = nullptr >
	void cont_reserve( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
		if( cont.capacity()<n ) {
			NOEXCEPT( cont.reserve(tc::max(cont_extended_memory(cont),n)) );
		}
	}

	template< typename Cont, typename... Args >
	Cont& cont_extend_or_truncate( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		tc::cont_reserve(cont, n);
		cont.resize(n, std::forward<Args>(args)...);
		return cont;
	}

	template< typename Cont, std::enable_if_t<!has_mem_fn_reserve<Cont>::value>* = nullptr>
	Cont& cont_clear( Cont& cont ) noexcept {
		cont.clear();
		return cont;
	}

	// erase elements in vector-like container to keep allocated memory
	template< typename Cont, std::enable_if_t<has_mem_fn_reserve<Cont>::value>* = nullptr>
	Cont& cont_clear( Cont& cont ) noexcept {
		cont.erase(boost::begin(cont),boost::end(cont));
		return cont;
	}

	template< typename Cont >
	typename boost::range_iterator<Cont>::type safe_cont_erase( Cont& cont, typename boost::range_iterator<Cont const>::type it ) noexcept {
		typename tc::range_value<std::remove_reference_t<Cont>>::type vt=std::move(*it); // *it may be const&
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
	typename boost::range_reference< std::remove_reference_t<Cont> >::type cont_extend_at(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		if( cont.size()<=n ) {
			cont_extend( cont, n+1, std::forward<Args>(args)...);
		}
		return cont[n];
	}

	template <typename Cont, typename... T, std::enable_if_t<has_mem_fn_emplace_back<Cont>::value>* = nullptr>
	void cont_emplace_back(Cont& cont, T&& ... value) noexcept {
		NOBADALLOC( cont.emplace_back(std::forward<T>(value)...) );
	}

	template <typename Cont, typename... T, std::enable_if_t<has_mem_fn_lower_bound<Cont>::value>* = nullptr>
	void cont_emplace_back(Cont& cont, T&& ... value) noexcept {
		tc::cont_must_emplace_before(cont, boost::end(cont), std::forward<T>(value)...);
	}

	template <typename Cont, typename T0, typename T1, typename... Ts, std::enable_if_t<!has_mem_fn_emplace_back<Cont>::value && !has_mem_fn_lower_bound<Cont>::value>* = nullptr>
	void cont_emplace_back(Cont& cont, T0&& v0, T1&& v1, Ts&& ... vs) noexcept {
		NOBADALLOC( cont.push_back(typename tc::range_value<Cont>::type(std::forward<T0>(v0), std::forward<T1>(v1), std::forward<Ts>(vs)...)) );
	}

	template <typename Cont, typename T0, std::enable_if_t<!has_mem_fn_emplace_back<Cont>::value && !has_mem_fn_lower_bound<Cont>::value>* = nullptr>
	void cont_emplace_back(Cont& cont, T0&& v0) noexcept {
		NOBADALLOC( cont.push_back(std::forward<T0>(v0)) );
	}

	template <typename Cont, std::enable_if_t<!has_mem_fn_emplace_back<typename Cont::value_type, Cont>::value && !has_mem_fn_lower_bound<Cont>::value>* = nullptr>
	void cont_emplace_back(Cont& cont) noexcept {
		NOBADALLOC( cont.push_back(typename tc::range_value<Cont>::type()) );
	}

	namespace cont_append_adl_barrier {
		template <typename Cont>
		struct mem_fn_emplace_back final {
			mem_fn_emplace_back(Cont& cont) noexcept
				: m_cont(cont)
			{}

			template <typename T, std::enable_if_t<tc::is_safely_convertible<T, typename Cont::value_type>::value>* = nullptr>
			void operator()(T&& t) const& noexcept {
				tc::cont_emplace_back(m_cont, std::forward<T>(t));
			}

		private:
			Cont& m_cont;
		};
	}

	// do not use Cont::insert() or Cont(it, it)
	// iterators are slower than for_each in many cases (eg. filter ranges)

	// cont_append for target containers without reserve() member:
	// just run a for_each over the input
	// assume iterators are stable to get iterator to first inserted element
	template< template<typename> class RangeReturn, typename Cont, typename Rng, std::enable_if_t<!has_mem_fn_reserve<Cont>::value>* = nullptr >
	typename RangeReturn<Cont&>::type cont_append(Cont& cont, Rng&& rng) MAYTHROW {
		boost::optional<typename boost::range_iterator<Cont>::type> oit;
		if (!tc::empty(cont)) {
			oit = tc::end_prev(cont);
		}
		try {
			tc::for_each(std::forward<Rng>(rng), tc::cont_append_adl_barrier::mem_fn_emplace_back<Cont>(cont)); // MAYTHROW
			return RangeReturn<Cont&>::pack_bound(
				oit ? boost::next(*oit) : boost::begin(cont),
				cont
			);
		} catch(...) {
			tc::take_inplace(cont, oit ? boost::next(*oit) : boost::begin(cont));
			throw;
		}
	}

	// cont_append for target containers with reserve() member and input range without tc::size():
	// just run a for_each over the input
	// assume random_access on container, and get iterator from offset
	template< template<typename> class RangeReturn, typename Cont, typename Rng, std::enable_if_t<has_mem_fn_reserve<Cont>::value && !tc::size_impl::has_size<Rng>::value>* = nullptr >
	typename RangeReturn<Cont&>::type cont_append(Cont& cont, Rng&& rng) MAYTHROW {
		typename Cont::size_type const nOffset = cont.size();
		try {
			tc::for_each(std::forward<Rng>(rng), tc::cont_append_adl_barrier::mem_fn_emplace_back<Cont>(cont)); // MAYTHROW
			return RangeReturn<Cont&>::pack_bound(
				tc::begin_next(cont,nOffset),
				cont
			);
		} catch(...) {
			tc::take_first_inplace(cont,nOffset);
			throw;
		}
	}

	// cont_append for target containers with reserve() member and input range with tc::size():
	// same as above, but do a reserve() on the target container first
	template< template<typename> class RangeReturn, typename Cont, typename Rng, std::enable_if_t<has_mem_fn_reserve<Cont>::value && tc::size_impl::has_size<Rng>::value>* = nullptr >
	typename RangeReturn<Cont&>::type cont_append(Cont& cont, Rng&& rng) MAYTHROW {
		auto const nOffset = cont.size();
		tc::cont_reserve(cont, nOffset + tc::size(rng));
		try {
			tc::for_each(std::forward<Rng>(rng), tc::cont_append_adl_barrier::mem_fn_emplace_back<Cont>(cont)); // MAYTHROW
			return RangeReturn<Cont&>::pack_bound(
				tc::begin_next(cont,nOffset),
				cont
			);
		} catch(...) {
			tc::take_first_inplace(cont, nOffset);
			throw;
		}
	}

	struct no_reference final {};

	template< template<typename> class RangeReturn, typename Cont, typename Arg >
	auto cont_find(Cont& cont, Arg&& arg) noexcept -> typename RangeReturn<Cont&>::type {
		auto it=cont.find(std::forward<Arg>(arg));
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

#ifdef _DEBUG
	namespace static_vector_adl_barrier {
		template< typename T, std::size_t N> struct static_vector;
	}
	using static_vector_adl_barrier::static_vector;
#endif

	namespace get_buffer_adl_barrier {
		template<typename Rng, std::enable_if_t<tc::is_char< typename tc::range_value<Rng>::type >::value>* = nullptr>
		void assert_no_null_terminator(Rng const& rng) noexcept {
			_ASSERT( !tc::find_first<tc::return_bool>(rng, tc::char_cast<typename tc::range_value<Rng>::type>('\0') ));
		}

		template<typename Rng, std::enable_if_t<!tc::is_char< typename tc::range_value<Rng>::type >::value>* = nullptr>
		void assert_no_null_terminator(Rng const& rng) noexcept {}

		template<typename Rng>
		void remove_null_terminator(Rng& rng) noexcept {
			static_assert( tc::is_char< typename tc::range_value<Rng>::type >::value, "" );
			_ASSERT( !tc::empty(rng) );
			_ASSERTEQUAL( tc_back(rng), tc::char_cast< typename tc::range_value<Rng>::type >('\0') );
			tc::take_inplace(rng,tc::end_prev(rng));
			tc::get_buffer_adl_barrier::assert_no_null_terminator(rng);
		}


#ifdef _DEBUG
		template<typename Cont>
		struct container_with_sentinel final {
			using type = Cont;
		};

		template<typename T, std::size_t N>
		struct container_with_sentinel<tc::static_vector<T, N>> final {
			using type = tc::static_vector<T, N + 1>;
		};
#endif

		template<typename Cont, typename Func>
		Cont get_truncating_buffer(Func func) noexcept {
			static_assert( tc::is_decayed<Cont>::value, "" );

			// sentinel to detect buffer overrun
			constexpr typename boost::range_size<Cont>::type nSentinel=
#ifdef _DEBUG
				1;
			typename container_with_sentinel<Cont>::type
#else
				0;
			Cont
#endif				
				cont;
			tc::cont_clear(cont, 0<cont.capacity() ? cont.capacity() : tc::numeric_cast<typename Cont::size_type>(8)/*, boost::container::default_init*/);

			for (;;) {
				auto const nSize =
#ifdef _DEBUG
				 [&] {
					tc::uninitialize(tc_back(cont));
					scope_exit( _ASSERTDEBUG( !tc::check_initialized(tc_back(cont))) );
					return 
#endif
						func(tc::ptr_begin(cont), tc::size(cont)-nSentinel);
#ifdef _DEBUG
				}();
#endif
				if (nSize < tc::size(cont)-nSentinel) {
					_ASSERT(0 <= nSize);
					tc::take_first_inplace(cont, nSize);
					tc::assert_no_null_terminator(cont);
					return
#ifdef _DEBUG
						tc::make_container<Cont>(cont)
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
			static_assert( tc::is_decayed<Cont>::value, "" );

			// sentinel to detect buffer overrun
			constexpr typename boost::range_size<Cont>::type nSentinel=
#ifdef _DEBUG
				1;
			typename container_with_sentinel<Cont>::type
#else
				0;
			Cont
#endif
				cont;
			static_assert( std::is_trivially_copyable<tc::decay_t<decltype(*tc::ptr_begin(cont))>>::value, "" );
			tc::cont_clear(cont,tc::max(cont.capacity(),nSentinel)/*, boost::container::default_init*/);

			for (;;) {
				auto const nSize = 
#ifdef _DEBUG
				 [&] {
					tc::fill_with_dead_pattern(tc_back(cont));
					scope_exit( tc::assert_dead_pattern(tc_back(cont)) );
					return 
#endif
					func(tc::ptr_begin(cont), tc::size(cont)-nSentinel); // MAYTHROW
#ifdef _DEBUG
				}();
#endif
				if (nSize <= tc::size(cont)-nSentinel) {
					_ASSERT(0 <= nSize);
					tc::take_first_inplace(cont, nSize);
					return
#ifdef _DEBUG
						tc::make_container<Cont>(cont)
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
			static_assert( tc::is_char< typename tc::range_value<Cont>::type >::value, "" );
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

	template< typename Cont, typename It, std::enable_if_t<!is_instance<std::multiset,Cont>::value && !is_instance<std::multimap,Cont>::value>* = nullptr >
	It && verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
		return std::forward<It>(it);
	}

	template< typename Cont, typename It, std::enable_if_t<is_instance<std::multiset,Cont>::value || is_instance<std::multimap,Cont>::value>* = nullptr >
	It && verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
#ifdef _DEBUG
		/* standard says: the inserted element has to be placed at upper bound */
		auto itNext = boost::next(it);
		_ASSERTDEBUG(boost::end(cont) == itNext || cont.value_comp()(*it, *itNext));
#endif
		return std::forward<It>(it);
	}

	template< typename Cont, typename TValue > // use extra template parameter instead of Cont::value_type to have both move and copy semantics
	typename Cont::iterator cont_must_insert(Cont& cont, TValue&& val) noexcept {
		return verify_inserted( verify_at_upper_bound( cont, NOBADALLOC(cont.insert(std::forward<TValue>(val))) ) );
	}

	DEFINE_FN(insert);
	DEFINE_FN(cont_must_insert);

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
			std::bind(mem_fn_insert(), &cont, std::placeholders::_1)
		);
		return cont;
	}

	template< typename Cont, typename Rng >
	Cont& cont_must_insert_range(Cont& cont, Rng&& rng) noexcept {
		tc::for_each(
			std::forward<Rng>(rng),
			std::bind(fn_cont_must_insert(), std::ref(cont), std::placeholders::_1)
		);
		return cont;
	}

	template< typename Cont, typename... Args >
	typename Cont::iterator cont_must_emplace(Cont& cont, Args&& ... args) noexcept {
		return verify_inserted( verify_at_upper_bound( cont, NOBADALLOC(cont.emplace(std::forward<Args>(args)...)) ) );
	}

	template< typename Cont, typename... Args >
	std::pair< typename Cont::iterator, bool > cont_try_emplace(Cont& cont, Args&& ... args) noexcept {
		return NOBADALLOC(cont.emplace(std::forward<Args>(args)...));
	}

	template< typename Cont, typename... Args >
	typename Cont::iterator cont_must_emplace_before(Cont& cont, typename Cont::const_iterator itHint, Args&& ... args) noexcept {
	#ifdef _CHECKS
		auto const c=cont.size();
	#endif
		typename Cont::iterator it = NOBADALLOC(cont.emplace_hint(itHint, std::forward<Args>(args)...));
		_ASSERTEQUAL( cont.size(), c+1 );
		_ASSERT( boost::next(it)==itHint );
		return it;
	}

	template<typename... MapArgs, typename K, typename V, typename Better>
	void map_try_emplace_better(std::map<MapArgs...>& map, K&& key, V&& val, Better&& better) noexcept {
		auto it = map.lower_bound(key);
		if (boost::end(map) == it || map.key_comp()(key, it->first)) {
			cont_must_emplace_before(map, it, std::forward<K>(key), std::forward<V>(val));
		} else {
			tc::assign_better(it->second, std::forward<V>(val), std::forward<Better>(better));
		}
	}
	
	template< typename... MapArgs, typename K, typename... MappedTypeCtorArgs >
	std::pair< typename std::map<MapArgs...>::iterator, bool > map_try_emplace_with_key(std::map<MapArgs...>& map, K&& key, MappedTypeCtorArgs&& ... mappedtypectorargs) noexcept {
		// TODO C++17: Use std::map::try_emplace
		auto it = map.lower_bound(key);
		if (boost::end(map)==it || map.key_comp()(key, it->first)) {
			return std::make_pair(
				tc::cont_must_emplace_before(
					map,
					it,
					std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<MappedTypeCtorArgs>(mappedtypectorargs)...) // delay actual construction of mapped type
				),
				true
			);
		} else {
			return std::make_pair(it, false);
		}
	}

	template< typename... MapArgs, typename K, typename... MappedTypeCtorArgs >
	std::pair< typename std::unordered_map<MapArgs...>::iterator, bool > unordered_map_try_emplace_with_key(std::unordered_map<MapArgs...>& map, K&& key, MappedTypeCtorArgs&& ... mappedtypectorargs) noexcept {
		// TODO C++17: Use std::unordered_map::try_emplace
		auto it = map.find(key);
		if (boost::end(map)==it) {
			return std::make_pair(
				tc::cont_must_emplace(
					map,
					std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<MappedTypeCtorArgs>(mappedtypectorargs)...) // delay actual construction of mapped type
				),
				true
			);
		} else {
			return std::make_pair(it, false);
		}
	}

	template<typename... MultiIndexArgs, typename K, typename... ValueTypeCtorArgs >
	std::pair< typename boost::multi_index_container<MultiIndexArgs...>::iterator, bool > multi_index_try_emplace_with_key(boost::multi_index_container<MultiIndexArgs...>& multi_index, K const& key, ValueTypeCtorArgs&& ... valuetypectorargs) noexcept {
		static_assert(1==boost::mpl::size<typename boost::multi_index_container<MultiIndexArgs...>::index_type_list>::value, "You may be able to use multi_index with more than two indices, but check carefully if it actually does what you expect.");
		auto it = multi_index.lower_bound(key);
		if (boost::end(multi_index)==it || multi_index.key_comp()(key, multi_index.key_extractor()(*it))) {
			return std::make_pair(
				tc::cont_must_emplace_before(multi_index, it, std::forward<ValueTypeCtorArgs>(valuetypectorargs)...),
				true
			);
		} else {
			return std::make_pair(it, false);
		}
	}

	template< typename Cont >
	void cont_must_erase(Cont& cont, typename Cont::key_type const& data) noexcept {
		VERIFYEQUAL( cont.erase(data), 1u );
	}

	template< typename Cont >
	void cont_try_erase(Cont& cont, typename Cont::key_type const& data) noexcept {
		VERIFY( cont.erase(data)<=1 );
	}

	template<typename Cont, typename Enable=void>
	struct range_filter;

	template<typename Cont>
	struct range_filter<Cont, std::enable_if_t< 
		has_efficient_erase<Cont>::value
		|| has_mem_fn_lower_bound<Cont>::value
		|| has_mem_fn_hash_function<Cont>::value
	> >: tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value, "" );
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
		static_assert( tc::is_decayed< Cont >::value, "" );
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
		static_assert( tc::is_decayed< Cont >::value, "" );
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
		static_assert( tc::is_decayed< Cont >::value, "" );
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
			*m_itOutput=tc_move_always(*it);
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
			m_rngfilter.ctor(Container(), boost::begin(rng));
		}

		void keep(iterator it) & noexcept {
			m_rngfilter->keep(it);
		}

		iterator begin() const& noexcept {
			return boost::begin(m_rng);
		}

		iterator end() const& noexcept {
			return boost::end(*m_rngfilter);
		}

		void pop_back() & noexcept {
			_ASSERT(boost::end(*this)!=boost::begin(*this));
			m_rngfilter->pop_back();
		}

		~range_filter() {
			auto& cont=Container();
			auto const nIndexBegin=boost::begin(m_rng)-boost::begin(cont);
			m_rngfilter.dtor(); // erases cont tail and invalidates iterators in m_rng
			m_rng=tc::drop_first(cont, nIndexBegin);
		}
	private:
		Cont& Container() const& noexcept {
			return boost::implicit_cast<Cont&>(m_rng.base_range());
		}

		tc::sub_range< Cont& >& m_rng;
		tc::storage_for< tc::range_filter<Cont> > m_rngfilter;
	};

	/////////////////////////////////////////////////////
	// filter_inplace

	template<typename Cont, typename Pred>
	Cont & filter_inplace(Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it, Pred pred) noexcept {
		for (auto const itEnd = boost::end(cont); it != itEnd; ++it) {
			if (!pred(*it)) {
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
	Cont & filter_inplace(Cont& cont, Pred&& pred) noexcept {
		return tc::filter_inplace( cont, boost::begin(cont), std::forward<Pred>(pred) );
	}

	// cannot use list::remove because T may not be list::value_type
	// cannot use key-based lookup for set/map because T may not be Cont::value_type and !Cont::predicate()(a,b) && !Cont::predicate()(b,a) may not be the same as ==
	template<typename Cont, typename T>
	Cont& remove_inplace(Cont& cont, T const& t) noexcept {
		return tc::filter_inplace( cont, std::bind( tc::fn_not_equal_to(), std::placeholders::_1, std::cref(t) ) );
	}

	/////////////////////////////////////////////////////
	// remove_count_erase

	template<typename Cont, typename Pred>
	typename tc::size_proxy< typename boost::range_size<Cont>::type > remove_count_erase_if( Cont& cont, Pred pred ) noexcept {
		typename boost::range_size<Cont>::type count=0;
		tc::filter_inplace( cont, [&]( typename boost::range_reference<Cont>::type t )->bool {
			bool const b=pred(tc_move_if_owned(t));
			count+=boost::implicit_cast<
#ifdef __clang__
				typename
#endif
				boost::range_size<Cont>::type
			>(b);
			return !b;
		} );
		return tc::size_proxy< typename boost::range_size<Cont>::type >(count);
	}

	template<typename Cont, typename T>
	typename tc::size_proxy< typename boost::range_size<Cont>::type > remove_count_erase(Cont& cont, T const& t) noexcept {
		return remove_count_erase_if( cont, std::bind( tc::fn_equal_to(), std::placeholders::_1, std::cref(t) ) );
	}

	/////////////////////////////////////////////////////
	// sort_inplace

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
	auto sort_unique(Rng&& rng, Less less) noexcept code_return_decltype (
		tc::sort_inplace( rng, less );,
		tc::ordered_unique( std::forward<Rng>(rng), tc_move(less) )
	)

	template<typename Rng>
	auto sort_unique(Rng&& rng) noexcept return_decltype(
		sort_unique( std::forward<Rng>(rng), tc::fn_less() )
	)

	///////////////////////////////////////
	// partition ranges into subranges

	template<typename Rng, typename Less>
	auto ordered_unique_range(Rng&& rng, Less less) noexcept code_return_decltype (
		_ASSERTDEBUG( tc::is_sorted( rng, less ) );,
		tc::adjacent_unique_range( std::forward<Rng>(rng), tc::not_fn( tc_move(less) ) )
	)

	template<typename Rng>
	auto ordered_unique_range(Rng&& rng) noexcept return_decltype (
		tc::ordered_unique_range( std::forward<Rng>(rng), tc::fn_less() )
	)

	template<typename Rng, typename Less>
	auto sort_unique_range(Rng&& rng, Less less) noexcept code_return_decltype (
		tc::sort_inplace( rng, less );,
		tc::ordered_unique_range( std::forward<Rng>(rng), tc_move(less) )
	)

	template<typename Rng>
	auto sort_unique_range(Rng&& rng) noexcept return_decltype(
		sort_unique_range( std::forward<Rng>(rng), tc::fn_less() )
	)

	template<typename Rng, typename Less>
	auto stable_sort_unique_range(Rng&& rng, Less less) noexcept code_return_decltype (
		boost::stable_sort( rng, less );,
		tc::ordered_unique_range( std::forward<Rng>(rng), tc_move(less) )
	)

	template<typename Rng>
	auto stable_sort_unique_range(Rng&& rng) noexcept return_decltype(
		stable_sort_unique_range( std::forward<Rng>(rng), tc::fn_less() )
	)

	template< typename Rng, typename Pred, typename Func >
	break_or_continue for_each_subrange_where(Rng&& rng, Pred pred, Func func) MAYTHROW {
		auto it=boost::begin(rng);
		auto const itEnd=boost::end(rng);
		for(;;) {
			for(;;) {
				if( it==itEnd ) return continue_;
				if( pred(*it) ) break;
				++it;
			}
			auto itStart=it;
			do {
				++it;
				if( it==itEnd ) {
					return continue_if_not_break( func, slice( rng, itStart, it ) ); // may throw
				};
			} while( pred(*it) );
			if( break_==continue_if_not_break( func, slice( rng, itStart, it ) ) ) return break_; // may throw
			++it;
		}
	}

	DEFINE_FN( for_each_subrange_where );

	template< typename Rng, typename Less, typename Accu >
	Rng&& sort_accumulate_each_unique_range(Rng&& cont, Less less, Accu accu) noexcept {
		tc::sort_inplace( cont, less );
		{ range_filter< tc::decay_t<Rng> > rngfilter( cont );
			tc::for_each(
				tc::ordered_unique_range(
					cont,
					tc_move(less)
				),
				[&accu,&rngfilter]( typename make_sub_range_result< std::add_lvalue_reference_t<Rng> >::type rngEqualSubRange ) {
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
					[](auto subrange) { // fn_boost_begin does not work, need subrange as lvalue to get non-const iterator
						return boost::begin(subrange);
					}
				),
				[&](typename boost::range_iterator<Cont>::type it) { // auto it causes Internal Compiler Error
					rngfilter.keep(it);
				}
			);
		}
		return cont;
	}

	/*
		In contrase to std::unique, tc::adjacent_unique / tc::adjacent_unique_inplace always compares adjacent elements. This allows implementing
		bidirectional tc::adjacent_unique, with tc::adjacent_unique_inplace yielding the same result.
	*/
	template< typename Cont, typename Equals=tc::fn_equal_to >
	Cont& adjacent_unique_inplace( Cont & cont, Equals&& pred=Equals() ) noexcept {
		{
			tc::range_filter< tc::decay_t<Cont> > rngfilter(cont);
			tc::for_each_may_remove_current(
				tc::make_range_of_iterators(tc::adjacent_unique(cont, std::forward<Equals>(pred))),
				[&](auto it) {
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

	template<typename Cont, typename Rng, std::enable_if_t<std::is_constructible<Cont, Rng&&>::value>* = nullptr>
	Cont make_container(Rng&& rng) noexcept {
		return tc::verify_class<Cont>(std::forward<Rng>(rng));
	}

	template<typename Cont, typename Rng, std::enable_if_t<!std::is_constructible<Cont, Rng&&>::value && !has_mem_fn_lower_bound<Cont>::value && !has_mem_fn_hash_function<Cont>::value>* = nullptr>
	Cont make_container(Rng&& rng) MAYTHROW {
		static_assert(tc::is_decayed<Cont>::value,"");
		Cont cont;
 		tc::cont_append<tc::return_void>(cont, std::forward<Rng>(rng)); // MAYTHROW
		return cont;
	}

	template<typename Cont, typename Rng, std::enable_if_t<!std::is_constructible<Cont, Rng&&>::value && (has_mem_fn_lower_bound<Cont>::value || has_mem_fn_hash_function<Cont>::value)>* = nullptr>
	Cont make_container(Rng&& rng) noexcept {
		static_assert(tc::is_decayed<Cont>::value, "");
		Cont cont;
		// force each element to be inserted
		// if there is need to run make_container with cont_try_insert_range (e.g. for removal of duplicates) implement a new function
		tc::cont_must_insert_range(cont, std::forward<Rng>(rng));
		return cont;
	}

	template< typename Rng >
	auto make_vector(Rng&& rng) MAYTHROW return_decltype(
		tc::make_container<tc::vector<typename tc::range_value<std::remove_reference_t<Rng>>::type>>(std::forward<Rng>(rng))
	)

	template< typename Rng >
	auto make_basic_string(Rng&& rng) noexcept return_decltype(
		tc::make_container<std::basic_string<typename tc::range_value<std::remove_reference_t<Rng>>::type>>(std::forward<Rng>(rng))
	)

	template< typename Char, typename Traits, typename Allocator >
	Char const* as_c_str(std::basic_string< Char, Traits, Allocator > const& str) noexcept
	{
		return str.c_str();
	}

	template<typename Char, std::enable_if_t< tc::is_char<Char>::value >* = nullptr, std::enable_if_t<tc::is_char<Char>::value>* = nullptr>
	Char const* as_c_str(Char const* psz) noexcept {
		return psz;
	}

	inline boost::filesystem::path::value_type const* as_c_str(boost::filesystem::path const& path) noexcept {
		return path.c_str();
	}

	template< template<typename> class RangeReturn, typename SetType, typename T, typename Pred >
	typename RangeReturn<std::unordered_set<SetType> const&>::type binary_find_unique(std::unordered_set<SetType> const& rng, T const& t, Pred pred) noexcept = delete;

	template< template<typename> class RangeReturn, typename Rng, typename T, typename Pred >
	typename RangeReturn<Rng>::type binary_find_unique(Rng&& rng, T const& t, Pred pred) noexcept {
		// The result of tc::binary_find_unique must be unambiguous. In general, this means that rng is strictly
		// ordered. In some cases, it is convenient to allow multiple occurrences of the same item in
		// rng, which is not a problem as long as these items are not searched for.
		_ASSERTDEBUG( tc::is_sorted( rng, pred ) );
		auto it=tc::lower_bound<tc::return_bound>( rng, t, pred );
		if( it==boost::end( rng ) ) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			auto && ref=*it;
			if (pred(t, ref)) {
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			} else {
		#ifdef _CHECKS
				auto itNext = boost::next(it);
				_ASSERT( boost::end(rng)==itNext || pred(t, *itNext) );
		#endif
				return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	typename RangeReturn<Rng>::type binary_find_unique(Rng&& rng, T const& t) noexcept {
		return tc::binary_find_unique<RangeReturn>( std::forward<Rng>(rng), t, tc::fn_less() );
	}

	template< template<typename> class RangeReturn, typename Rng, typename T, typename Pred >
	typename RangeReturn<Rng>::type binary_find_first(Rng&& rng, T const& t, Pred pred) noexcept {
		_ASSERTDEBUG( tc::is_sorted( rng, pred ) );
		auto it=tc::lower_bound<tc::return_bound>( rng, t, pred );
		if (it == boost::end(rng)) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			auto && ref = *it;
			if (pred(t, ref)) {
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			} else {
				return RangeReturn<Rng>::pack_element(it, std::forward<Rng>(rng), tc_move_if_owned(ref));
			}
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	typename RangeReturn<Rng>::type binary_find_first(Rng&& rng, T const& t) noexcept {
		return tc::binary_find_first<RangeReturn>( std::forward<Rng>(rng), t, tc::fn_less() );
	}

	// would be cleaner to search on the distance metric (starting with lower_bound(rng,0)),
	// but subtraction may cause unnecessary overflows
	template< typename Rng, typename T >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type binary_closest(Rng&& rng, T const& t) noexcept {
		auto it = tc::lower_bound<tc::return_bound>(rng, t);
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
	break_or_continue interleave_may_remove_current(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB,  FuncElementBoth fnElementBoth) noexcept {
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

	namespace interleave_impl {
		// Workaround until [](auto&&) lamdas are available
		template<typename ItB, typename Comp, typename FnElementA, typename FnElementB, typename FnElementBoth>
		struct FInterleaveRngA final {
			ItB& m_itB;
			ItB const& m_itEndB;
			Comp& m_comp;
			FnElementA& m_fnElementA;
			FnElementB& m_fnElementB;
			FnElementBoth& m_fnElementBoth;

			template<typename TypeA>
			tc::break_or_continue operator()(TypeA&& refA) & noexcept {
				for (;;) {
					tc::order order;
					if( m_itB == m_itEndB || (order=m_comp( refA, *m_itB ))<tc::order::equal ) {
						return tc::continue_if_not_break(m_fnElementA, refA);
					} else if( tc::order::equal<VERIFYINITIALIZED(order) ) {
						RETURN_IF_BREAK( tc::continue_if_not_break(m_fnElementB, *m_itB));
						++m_itB;
					} else {
						return tc::continue_if_not_break(m_fnElementBoth, refA, *m_itB++);
					}
				}
			}
		};
	}

	template< typename RngA, typename RngB, typename Comp, typename FuncElementA, typename FuncElementB, typename FuncElementBoth >
	tc::break_or_continue interleave(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB,  FuncElementBoth fnElementBoth) noexcept {
		auto itB=boost::begin(rngB);
		auto itEndB=boost::end(rngB);

		if (tc::continue_ == tc::for_each(
			rngA,
			interleave_impl::FInterleaveRngA<decltype(itB),Comp,FuncElementA,FuncElementB,FuncElementBoth>{itB, itEndB, comp, fnElementA, fnElementB, fnElementBoth}
			) ) {
			while (itB != itEndB) {
				RETURN_IF_BREAK( tc::continue_if_not_break(fnElementB, *itB));
				++itB;
			}
			return tc::continue_;
		} else {
			return tc::break_;
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	typename RangeReturn<Rng>::type best_element(Rng && rng, Pred pred) MAYTHROW {
		auto const itEnd=boost::end(rng); // MAYTHROW
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type ait[2]={ boost::begin(rng) }; // MAYTHROW
		if(ait[0]==itEnd) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			tc::storage_for< tc::reference_or_value<typename boost::range_reference< std::remove_reference_t<Rng> >::type> > aref[2];
			aref[0].ctor( aggregate_tag(), *ait[0] ); // MAYTHROW
			for(;;){
				for( int i=0; i!=2; ++i ) { // we expect the compiler to unroll this loop
					// aref[i] is constructed, aref[1-i] is not constructed
					scope_exit( aref[i].dtor() ); // also required in case of exception
					ait[1-i]=ait[i];
					for(;;) {
						// aref[i] is constructed, aref[1-i] is not constructed
						++ait[1-i];
						if(ait[1-i]==itEnd) {
							return RangeReturn<Rng>::pack_element(tc_move_always(ait[i]),std::forward<Rng>(rng),**tc_move_always(aref[i]));
						}
						aref[1-i].ctor( aggregate_tag(), *ait[1-i] ); // MAYTHROW
						try {
							if( pred(**aref[1-i],**aref[i]) ) { // MAYTHROW
								break; // only path where aref[1-i] is not destroyed
							}
						} catch(...) {
							aref[1-i].dtor();
							throw;
						}
						aref[1-i].dtor();
					}
				}
			}
		}
	}

	template< template<typename> class RangeReturn, typename Rng >
	typename RangeReturn<Rng>::type min_element(Rng && rng) MAYTHROW {
		return best_element<RangeReturn>(std::forward<Rng>(rng), tc::fn_less());
	}

	template< template<typename> class RangeReturn, typename Rng >
	typename RangeReturn<Rng>::type max_element(Rng && rng) MAYTHROW {
		return best_element<RangeReturn>(std::forward<Rng>(rng), tc::fn_greater());
	}
}
