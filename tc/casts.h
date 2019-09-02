
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "functors.h"
#include "tc_move.h"

#include "return_decltype.h"
#include "type_traits.h"
#include "meta.h"
#include "container_traits.h"

#include <boost/mpl/identity.hpp>
#include <boost/integer.hpp>
#ifndef __EMSCRIPTEN__
#include <boost/filesystem/path.hpp>
#endif

#include <type_traits>

//-----------------------------------------------------------------------------------------------------------------------------

namespace tc {
	template< typename T >
	struct is_plain_type final
		: std::is_same< T, std::remove_pointer_t< tc::remove_cvref_t<T> > >::type
	{};

	template<typename Dst, typename Src>
	struct same_cvref {
		static_assert( is_plain_type<Dst>::value, "use non-cv-qualified non-reference type" );
		static_assert( std::is_same< Src, tc::remove_cvref_t<Src> >::value && !std::is_reference_v<Src>, "Src must not be cv-qualified. Check if a template specialization of same_cvref is missing." );
		using type = Dst;
	};

	#pragma push_macro("SAME_CVREF_IMPL")
	#define SAME_CVREF_IMPL(cvref) \
	template<typename Dst, typename Src> \
	struct same_cvref<Dst, Src cvref> { \
		static_assert( is_plain_type<Dst>::value, "use non-cv-qualified non-reference type" ); \
		using type = Dst cvref; \
	};

	SAME_CVREF_IMPL(&)
	SAME_CVREF_IMPL(&&)
	SAME_CVREF_IMPL(const&)
	SAME_CVREF_IMPL(const&&)
	SAME_CVREF_IMPL(const)
	SAME_CVREF_IMPL(volatile&)
	SAME_CVREF_IMPL(volatile&&)
	SAME_CVREF_IMPL(volatile)
	SAME_CVREF_IMPL(volatile const&)
	SAME_CVREF_IMPL(volatile const&&)
	SAME_CVREF_IMPL(volatile const)

	#pragma pop_macro("SAME_CVREF_IMPL")
	
	template< typename Dst, typename Src >
	using same_cvref_t = typename same_cvref<Dst, Src>::type;

	//-------------------------------------------------------------------------------------------------------------------------

	/////////////////////////////////////////////
	// base_cast
	// (cannot be implemented like derived_cast because when deriving protected, derived to base cast is often publicly inaccessible)

	#pragma push_macro("BASE_CAST_IMPL")
	#define BASE_CAST_IMPL(cvref) \
	template<typename Dst, std::enable_if_t<std::is_class<Dst>::value>* = nullptr> \
	constexpr Dst cvref base_cast(typename boost::mpl::identity<Dst>::type cvref t) noexcept { \
		STATICASSERTSAME(tc::remove_cvref_t<Dst>, Dst); \
		return static_cast<Dst cvref>(t); \
	}
	BASE_CAST_IMPL(&)
	BASE_CAST_IMPL(&&)
	BASE_CAST_IMPL(*)
	BASE_CAST_IMPL(const&)
	BASE_CAST_IMPL(const&&)
	BASE_CAST_IMPL(const*)
	BASE_CAST_IMPL(volatile&)
	BASE_CAST_IMPL(volatile&&)
	BASE_CAST_IMPL(volatile*)
	BASE_CAST_IMPL(volatile const&)
	BASE_CAST_IMPL(volatile const&&)
	BASE_CAST_IMPL(volatile const*)
	#pragma pop_macro("BASE_CAST_IMPL")

	/////////////////////////////////////////////
	// derived_cast

	template< typename To, typename From >
	constexpr same_cvref_t< To, From&&> derived_cast( From&& t ) noexcept {
		static_assert( std::is_base_of<std::remove_reference_t<From>, To>::value, "derived_cast is for downcasts only.");
		return static_cast< same_cvref_t< To, From&&> >(t);
	}

	template< typename To, typename From >
	constexpr same_cvref_t< To, From>* derived_cast( From* pt ) noexcept {
		static_assert( std::is_base_of<std::remove_pointer_t<From>, To>::value, "derived_cast is for downcasts only.");
		return static_cast< same_cvref_t< To, From>* >(pt);
	}

	template< typename To, typename From >
	constexpr same_cvref_t< To, From&&> derived_or_base_cast( From&& t ) noexcept {
		return static_cast< same_cvref_t< To, From&&> >(t);
	}

	template< typename To, typename From >
	constexpr same_cvref_t< To, From>* derived_or_base_cast( From* pt ) noexcept {
		return static_cast< same_cvref_t< To, From>* >(pt);
	}

	DEFINE_FN2_TMPL( derived_cast, (typename) );

	//-------------------------------------------------------------------------------------------------------------------------

	template< typename T, std::enable_if_t<tc::is_char<T>::value>* =nullptr>
	constexpr auto underlying_cast(T t) noexcept
		return_decltype( static_cast<typename boost::uint_t<sizeof(T)*8>::exact>(t) )

	// gcc (4.8.3) does not like the string literals inside _ASSERTE so:
	template< typename T, std::enable_if_t<tc::is_actual_integer<T>::value>* =nullptr>
	auto unsigned_cast(T t) noexcept code_return_decltype( 
		_ASSERT( 0<=t );,
		static_cast<std::make_unsigned_t<T>>(t)
	)

	// gcc (4.8.3) does not like the string literals inside _ASSERTE so:
	template< typename T, std::enable_if_t<tc::is_actual_integer<T>::value>* =nullptr>
	std::make_signed_t<T> signed_cast(T t) noexcept {
		_ASSERT( std::is_signed<T>::value || t<=tc::unsigned_cast( std::numeric_limits<std::make_signed_t<T>>::max() ) );
		return static_cast<std::make_signed_t<T>>(t);
	}

	#pragma warning( push )
	#pragma warning( disable: 4180 ) // qualifier applied to function type has no meaning; ignored

		template< typename T >
		constexpr T const& as_const(T& t) noexcept { // intention is to avoid side-effects
			return static_cast<T const&>(t);
		}
		template <typename T>
		constexpr T&& as_const(T&& t) noexcept { // needed in generic code when both values and references can occur
			static_assert(!std::is_lvalue_reference<T&&>::value);
			return std::forward<T&&>(t);
		}

		template< typename T >
		constexpr std::remove_const_t<T>& as_mutable(T& t) noexcept {
			return const_cast<std::remove_const_t<T>&>(t);
		}

	#pragma warning( pop )

	template< typename T >
	constexpr T const* make_const_ptr( T const* pt ) noexcept {
		return pt;
	}

	template< typename T >
	constexpr T* make_mutable_ptr( T const* pt ) noexcept {
		return const_cast<T*>(pt);
	}

	template<typename Func>
	struct make_arg_mutable_impl final {
	private:
		tc::decay_t<Func> m_func;
	public:
		make_arg_mutable_impl(Func&& func) noexcept : m_func(std::forward<Func>(func)) {}
		template<typename T> auto operator()(T const& t) const& MAYTHROW return_decltype( m_func( as_mutable(t) ) )
	};

	template<typename Func>
	auto make_arg_mutable(Func&& func) noexcept return_decltype( make_arg_mutable_impl<Func>( std::forward<Func>(func) ) )

	/////////////////////////////////////////////
	// void_cast

	template<typename Dst, typename Src>
	Dst* void_cast(Src* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		STATICASSERTSAME(tc::remove_cvref_t<Dst>, Dst);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst*>(p);
	}

	template<typename Dst, typename Src>
	Dst const* void_cast(Src const* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		STATICASSERTSAME(tc::remove_cvref_t<Dst>, Dst);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst const*>(p);
	}

	template<typename Dst, typename Src>
	Dst volatile* void_cast(Src volatile* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		STATICASSERTSAME(tc::remove_cvref_t<Dst>, Dst);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst volatile*>(p);
	}

	template<typename Dst, typename Src>
	Dst volatile const* void_cast(Src volatile const* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		STATICASSERTSAME(tc::remove_cvref_t<Dst>, Dst);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst volatile const*>(p);
	}

	/////////////////////////////////////////////
	// reluctant_implicit_cast
	// Returns a reference to its argument whenever possible, otherwise performs an implicit conversion.

	template<typename TTarget, typename TSource>
	std::conditional_t<
		tc::is_base_of_decayed<TTarget, TSource>::value,
		TSource&&,
		TTarget
	> reluctant_implicit_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	/////////////////////////////////////////////
	// reluctant_static_cast
	// Returns a reference to its argument whenever possible, otherwise performs an explicit conversion.

	template<typename TTarget, typename TSource, std::enable_if_t<tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	TSource&& reluctant_static_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource, std::enable_if_t<!tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	TTarget reluctant_static_cast(TSource&& src) noexcept {
		return static_cast<TTarget>(std::forward<TSource>(src));
	}

	/////////////////////////////////////////////
	// as_c_str

	template< typename Char, typename Traits, typename Allocator >
	Char const* as_c_str(std::basic_string< Char, Traits, Allocator > const& str) noexcept
	{
		return str.data(); // since C++ 11, performs same function as c_str()
	}

	template< typename Char, typename Traits, typename Allocator >
	Char* as_c_str(std::basic_string< Char, Traits, Allocator >& str) noexcept
	{
		return str.data(); // since C++ 11, performs same function as c_str()
	}

	template< typename Char, typename Traits, typename Allocator >
	Char* as_c_str(std::basic_string< Char, Traits, Allocator >&& str) noexcept
	{
		return str.data(); // since C++ 11, performs same function as c_str()
	}

	template<typename Char, std::enable_if_t<tc::is_char<Char>::value>* = nullptr>
	Char const* as_c_str(Char const* psz) noexcept {
		return psz;
	}

	template<typename Char, std::enable_if_t<tc::is_char<Char>::value>* = nullptr>
	Char* as_c_str(Char* psz) noexcept {
		return psz;
	}

#ifndef __EMSCRIPTEN__
	inline boost::filesystem::path::value_type const* as_c_str(boost::filesystem::path const& path) noexcept {
		return path.c_str();
	}
#endif
}

