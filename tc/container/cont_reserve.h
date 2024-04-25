
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
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

#include <boost/container/container_fwd.hpp>

namespace tc {

	template <typename SizeT>
	SizeT cont_extended_memory(SizeT const nCurrentCapacity, std::type_identity_t<SizeT> const nRequestedCapacity) noexcept {
		// 64 bit is enough to hold any memory money can buy
		return tc::max(nRequestedCapacity, static_cast<SizeT>(static_cast<std::uint64_t>(nCurrentCapacity)*8/5));
	}

	template< typename Cont >
	typename boost::range_size< Cont >::type cont_extended_memory(Cont const& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type const n) noexcept {
		// factor*cont.size() does not suffice for memory operation guarantee
		return tc::cont_extended_memory(cont.capacity(), n);
	}

	template< typename Cont >
	void cont_reserve( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
		if constexpr( has_mem_fn_reserve<Cont> ) {
			if( cont.capacity()<n ) {
				NOEXCEPT( cont.reserve(cont_extended_memory(cont,n) ));
			}
		}
	}

	template< typename Cont >
	auto safe_cont_erase( Cont& cont, tc::iterator_t<Cont const> it ) noexcept {
		[[maybe_unused]] auto vt=tc::decay_copy(tc_move_always_even_const(*it)); // *it may be const&
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

	namespace cont_extend_detail {
		template<typename Cont, typename... Args> requires
			requires { std::declval<Cont&>().resize(std::declval<typename boost::range_size< std::remove_reference_t<Cont> >::type>(), std::declval<Args&&>()...); }
		void cont_extend_impl(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args&&... args) noexcept {
			_ASSERT( cont.size()<=n );
			tc::cont_reserve(cont, n);
			NOBADALLOC(cont.resize(n, tc_move_if_owned(args)...));
		}
	}

	// value initialize extended elements
	template<typename Cont, typename... Args> requires
		requires { std::declval<Cont&>().resize(std::declval<typename boost::range_size< std::remove_reference_t<Cont> >::type>(), std::declval<Args&&>()...); }
	void cont_extend(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args&&... args) noexcept {
		tc::cont_extend_detail::cont_extend_impl(cont, n, tc_move_if_owned(args)...);
	}

	// default initialize extended elements
	template<typename Cont> requires
		// Many containers don't support default initialization of trivially default constructible types and use value initialization instead.
		// We enable boost::container::default_init tag where needed.
		//   1. for containers that support boost::container::default_init_t, forward it.
		//   2. for containers that do not support boost::container::default_init_t: do not forward it and use cont.resize(n) (most likely value initialization) if available.
		//      TODO: use the right format once default initialization is supported
		requires { std::declval<Cont&>().resize(std::declval<typename boost::range_size< std::remove_reference_t<Cont> >::type>(), boost::container::default_init); } ||
		requires { std::declval<Cont&>().resize(std::declval<typename boost::range_size< std::remove_reference_t<Cont> >::type>()); }
	void cont_extend(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, boost::container::default_init_t) noexcept {
		if constexpr (requires { cont.resize(n, boost::container::default_init);}) {
			tc::cont_extend_detail::cont_extend_impl(cont, n, boost::container::default_init);
		} else {
			tc::cont_extend_detail::cont_extend_impl(cont, n); // should be default initialized but container does not support it yet and use value initialization instead.
		}
	}

	template< typename Cont, typename... Args,
		typename = decltype(tc::cont_extend(std::declval<Cont&>(), std::declval<typename boost::range_size< std::remove_reference_t<Cont> >::type>(), std::declval<Args&&>()...))
	>
	void cont_fill( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n,  Args &&... args) noexcept {
		cont.clear();
		cont_extend( cont, n, tc_move_if_owned(args)...);
	}

	template< typename Cont, typename... Args,
		typename = decltype(tc::cont_extend(std::declval<Cont&>(), std::declval<typename boost::range_size< std::remove_reference_t<Cont> >::type>()+1, std::declval<Args&&>()...))
	>
	[[nodiscard]] decltype(auto) cont_extend_at(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n, Args &&... args) noexcept {
		if( cont.size()<=n ) {
			cont_extend( cont, n+1, tc_move_if_owned(args)...);
		}
		static_assert( !tc::is_stashing_element<decltype(tc::at<tc::return_element>(cont,n))>::value );
		return tc::at(cont,n);
	}
}
