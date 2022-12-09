
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../range/meta.h"
#include "../algorithm/minmax.h"
#include "../algorithm/empty.h"
#include "../algorithm/filter_inplace.h"
#include "../algorithm/element.h"

namespace tc {

	template< typename Cont >
	typename boost::range_size< Cont >::type cont_extended_memory(Cont const& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=2) noexcept {
		// factor*cont.size() does not suffice for memory operation guarantee
		// 64 bit is enough to hold any memory money can buy
		return tc::max(n,static_cast< typename Cont::size_type >(static_cast<std::uint64_t>(cont.capacity())*8/5));
	}

	template< typename Cont >
	void cont_reserve( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
		if constexpr( has_mem_fn_reserve<Cont>::value ) {
			if( cont.capacity()<n ) {
				NOEXCEPT( cont.reserve(cont_extended_memory(cont,n) ));
			}
		}
	}

	template< typename Cont >
	auto safe_cont_erase( Cont& cont, tc::iterator_t<Cont const> it ) noexcept {
		[[maybe_unused]] auto vt=tc::decay_copy(std::move(*it)); // *it may be const&
		return cont.erase(it);
	}

	// safer against reentrance in destructor of value by first moving the value out of the container, then erasing the element in the container and then letting the value go out of scope
	template<typename Cont>
	void safe_cont_clear( Cont& cont ) noexcept {
		if constexpr( tc::range_filter_by_move_element<Cont>::value ) {
			auto it=tc::end(cont);
			auto itBegin=tc::begin(cont);
			if( it!=itBegin ) {
				for(;;) {
					--it;
					if (it==itBegin) break;
					it=tc::safe_cont_erase(cont,it);
				}
				// special treatment of last iteration necessary because it!=itBegin cannot be tested anymore after itBegin has been erased
				tc::safe_cont_erase(cont,it);
			}
		} else {
			auto it=tc::begin(cont);
			auto itEnd=tc::end(cont);
			while( it!=itEnd ) {
				it=tc::safe_cont_erase(cont,it);
			}
		}
		_ASSERT(tc::empty(cont)); // no one put anything into the container during reentrance
	}

	template< typename Cont, typename... Args,
		typename = decltype(std::declval<Cont&>().resize(std::declval<typename boost::range_size< std::remove_reference_t<Cont> >::type>(), std::declval<Args&&>()...))
	>
	void cont_extend( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		_ASSERT( cont.size()<=n );
		tc::cont_reserve(cont, n);
		NOBADALLOC(cont.resize(n, std::forward<Args>(args)...));
	}

	template< typename Cont, typename... Args,
		typename = decltype(tc::cont_extend(std::declval<Cont&>(), std::declval<typename boost::range_size< std::remove_reference_t<Cont> >::type>(), std::declval<Args&&>()...))
	>
	void cont_fill( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n,  Args &&... args) noexcept {
		cont.clear();
		cont_extend( cont, n, std::forward<Args>(args)...);
	}

	template< typename Cont, typename... Args,
		typename = decltype(tc::cont_extend(std::declval<Cont&>(), std::declval<typename boost::range_size< std::remove_reference_t<Cont> >::type>()+1, std::declval<Args&&>()...))
	>
	[[nodiscard]] decltype(auto) cont_extend_at(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		if( cont.size()<=n ) {
			cont_extend( cont, n+1, std::forward<Args>(args)...);
		}
		static_assert( !tc::is_stashing_element<decltype(tc::at<tc::return_element>(cont,n))>::value );
		return tc::at(cont,n);
	}
}
