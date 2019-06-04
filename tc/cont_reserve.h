
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "meta.h"
#include "minmax.h"
#include "empty.h"
#include "filter_inplace.h"

namespace tc {

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

	template< typename Cont >
	void cont_clear( Cont& cont ) noexcept {
		cont.clear();
	}

	template< typename Cont >
	auto safe_cont_erase( Cont& cont, typename boost::range_iterator<Cont const>::type it ) noexcept {
		auto vt=tc::decay_copy(std::move(*it)); // *it may be const&
		return cont.erase(it);
	}

	// safer against reentrance in destructor of value by first moving the value out of the container, then erasing the element in the container and then letting the value go out of scope
	template< typename Cont, std::enable_if_t<tc::range_filter_by_move_element<Cont>::value>* = nullptr >
	void safe_cont_clear( Cont& cont ) noexcept {
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
		_ASSERT(tc::empty(cont)); // no one put anything into the container during reentrance
	}

	template< typename Cont, std::enable_if_t<!tc::range_filter_by_move_element<Cont>::value>* = nullptr >
	void safe_cont_clear( Cont& cont ) noexcept {
		auto it=tc::begin(cont);
		auto itEnd=tc::end(cont);
		while( it!=itEnd ) {
			it=tc::safe_cont_erase(cont,it);
		}
		_ASSERT(tc::empty(cont)); // no one put anything into the container during reentrance
	}

	template< typename Cont, typename... Args >
	void cont_clear( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n,  Args &&... args) noexcept {
		cont_clear( cont );
		cont_extend( cont, n, std::forward<Args>(args)...);
	}

	template< typename Cont, typename... Args >
	void cont_extend( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		_ASSERT( cont.size()<=n );
		tc::cont_reserve(cont, n);
		NOBADALLOC(cont.resize(n, std::forward<Args>(args)...));
	}

	template< typename Cont, typename... Args >
	tc::range_reference_t< Cont > cont_extend_at(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		if( cont.size()<=n ) {
			cont_extend( cont, n+1, std::forward<Args>(args)...);
		}
		return tc_at(cont,n);
	}
}
