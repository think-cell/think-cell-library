#pragma once

#include "conversion_traits.h"
#include "Library/ErrorReporting/tc_move.h"

#include <boost/mpl/has_xxx.hpp>
#include <boost/mpl/logical.hpp>

#include <boost/utility/enable_if.hpp>

#include <type_traits>
#include <memory>

namespace tc {
	
	struct ctor_const_overload {};
	BOOST_MPL_HAS_XXX_TRAIT_DEF(ctor_const_overload_support)

	template<typename T> struct need_ctor_const_overload 
		: std::integral_constant< bool, std::is_const<typename std::remove_reference<T>::type>::value 
										&& has_ctor_const_overload_support<typename std::remove_reference<T>::type>::value >
	{};

	template< typename T >
	class reference_or_value {
		static_assert( !std::is_reference<T>::value, "" );
		typename std::remove_cv<T>::type mutable m_t;
	public:
		reference_or_value() {} // m_t may be default-constructible

		template< typename Rhs >
		reference_or_value( Rhs && rhs, typename std::enable_if< !std::is_base_of< reference_or_value, typename std::decay< Rhs >::type >::value
																&& !need_ctor_const_overload<T>::value, unused_arg>::type=unused_arg() )
		:	m_t( std::forward<Rhs>(rhs) )
		{}
		template< typename Rhs >
		reference_or_value( Rhs && rhs, typename std::enable_if< std::is_base_of< reference_or_value, typename std::decay< Rhs >::type >::value 
																&& !need_ctor_const_overload<T>::value, unused_arg>::type=unused_arg() )
		:	m_t( std::forward<Rhs>(rhs).m_t )
		{}
		template< typename Rhs >
		reference_or_value( Rhs && rhs, typename std::enable_if< !std::is_base_of< reference_or_value, typename std::decay< Rhs >::type >::value
																&& need_ctor_const_overload<T>::value, unused_arg>::type=unused_arg() )
		:	m_t( std::forward<Rhs>(rhs), ctor_const_overload() )
		{}
		template< typename Rhs >
		reference_or_value( Rhs && rhs, typename std::enable_if< std::is_base_of< reference_or_value, typename std::decay< Rhs >::type >::value 
																&& need_ctor_const_overload<T>::value, unused_arg>::type=unused_arg() )
		:	m_t(std::forward<Rhs>(rhs).m_t, ctor_const_overload() )
		{}

		T& best_access() const {
			// When declaring m_t non-mutable and using const_cast here be undefined behavior in code like:
			//	reference_or_value<T> _const_ foo;
			//	foo.best_access();
			// ?
			return m_t;
		}
		T* operator->() {
			return std::addressof(m_t);
		}
		T const* operator->() const {
			return std::addressof(m_t);
		}
		T& operator*() {
			return m_t;
		}
		T const& operator*() const {
			return m_t;
		}
	};

	template< typename T >
	class reference_or_value<T&> {
		T* m_pt;
	public:
		reference_or_value()
		:	m_pt(nullptr)
		{}
		reference_or_value(T& t)
		:	m_pt(std::addressof(t))
		{}
		reference_or_value(reference_or_value const& rhs)
		:	m_pt(rhs.m_pt)
		{}
		T & best_access() const {
			return *m_pt;
		}
		T * operator->() {
			return m_pt;
		}
		T const* operator->() const {
			return m_pt;
		}
		T & operator*() {
			return *m_pt;
		}
		T const& operator*() const {
			return *m_pt;
		}
	};

}
