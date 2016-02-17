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

#include "remove_cvref.h"

#include <boost/numeric/conversion/conversion_traits.hpp>

#include <boost/mpl/if.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/zip_view.hpp>
#include <boost/mpl/unpack_args.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/min_element.hpp>

#include <type_traits>

// Use as type of constructor arguments that are required for enabling / disabling constructor through SFINAE.
// To be replaced by template parameter default when Visual C++ supports template parameter defaults for functions.
struct unused_arg final {};

namespace tc {
	namespace is_char_detail {
		template<typename T>
		struct is_char: std::false_type {};
		template<>
		struct is_char<char>: std::true_type {};
		template<>
		struct is_char<wchar_t>: std::true_type {};
#ifndef BOOST_NO_CXX11_CHAR16_T
		template<>
		struct is_char<char16_t>: std::true_type {};
#endif
#ifndef BOOST_NO_CXX11_CHAR32_T
		template<>
		struct is_char<char32_t>: std::true_type {};
#endif
	}
	template< typename T >
	struct is_char: is_char_detail::is_char< std::remove_cv_t< T > > {};

	template< typename T >
	using is_bool=std::is_same< std::remove_cv_t<T>, bool >;

	template< typename T >
	using is_decayed=std::is_same< T, std::decay_t<T> >;

	template<typename T>
	using is_actual_integer=std::integral_constant< bool, std::is_integral<T>::value && !tc::is_char<T>::value && !tc::is_bool<T>::value >;

	// Both boost::is_base_of<X,X> and std::is_base_of<X,X> inherit from true_type if and only if X is a class type.
	// Also, std::is_base_of requires Base and Derived to be complete.
	template<typename Base, typename Derived>
	using is_base_of=std::integral_constant< bool,
		std::is_same< std::remove_cv_t<Base>, std::remove_cv_t<Derived> >::value ||
		std::is_base_of<Base, Derived>::value
	>;

	template<typename Base, typename Derived>
	struct is_base_of_decayed : std::integral_constant< bool,
		tc::is_base_of< Base, std::decay_t<Derived> >::value
	> {
		static_assert(tc::is_decayed<Base>::value, "");
	};

	// For references that may bind to temporaries, i.e.,
	// - TTarget const&
	// - TTarget &&
	// - TTarget const&&
	// prevent initialization by
	//  - TSource {&&}, since this would bind a temporary,
	//  - TSource {{const}&}, if an implicit conversion to a temporary TTarget takes place.
	//    As this cannot be tested, disallow initialization unless TTarget is base or same
	//    as TSource.
	template<typename TSource, typename TTarget>
	struct creates_no_reference_to_temporary : std::integral_constant<
		bool,
		!std::is_rvalue_reference<TTarget>::value
		&& (!std::is_lvalue_reference<TTarget>::value || !std::is_const<std::remove_reference_t<TTarget>>::value) // a mutable lvalue reference does not bind to temporary objects, so it is safe to allow it
		|| std::is_lvalue_reference<TSource>::value && tc::is_base_of< 
			std::remove_reference_t<TTarget>,
			std::remove_reference_t<TSource>
		>::value
	> {
	};

	namespace is_safely_convertible_adl_barrier {
		// If TTarget is arithmetic
		//   - Character types must not be mixed with other arithmetic types.
		//   - Any arithmetic type (except character types) can be assigned to floating point.
		//   - Unsigned integral types can be assigned to any integral type if the upper bound is large enough.
		//   - Signed integral types can be assigned to signed integral types if upper and lower bounds are large enough.
		template<typename TSource, typename TTarget, typename=void>
		struct is_safely_convertible_to_arithmetic_value
			: std::false_type
		{};

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_to_arithmetic_value<TSource, TTarget, std::enable_if_t<std::is_class<std::remove_reference_t<TSource>>::value>>
			: std::true_type
		{
			static_assert(std::is_arithmetic<TTarget>::value, "");
		};

#pragma warning(push)
#pragma warning(disable: 4018) // signed/unsigned mismatch
#pragma	warning(disable: 4804) // '<=': unsafe use of type 'bool' in operation

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_between_arithmetic_values
			: std::integral_constant<bool,
				std::is_same<std::remove_cv_t<TSource>, std::remove_cv_t<TTarget>>::value
				||
					(std::is_floating_point<TSource>::value || tc::is_actual_integer<TSource>::value)
					&& (
						std::is_floating_point<TTarget>::value // TODO: disable subranged conversions (such as int to float, or double to float) ?
						||
						tc::is_actual_integer<TTarget>::value
						&&(	std::is_signed<TSource>::value
							?	std::is_signed<TTarget>::value
								&& std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
								&& std::numeric_limits<TTarget>::lowest() <= std::numeric_limits<TSource>::lowest()
							:	// conversion to unsigned (Warning 4018) is ok here:
								std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
						)
					)
			>
		{
			static_assert(std::is_convertible<TSource, TTarget>::value, "");
		};

#pragma warning(pop)

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_to_arithmetic_value<TSource, TTarget, std::enable_if_t<std::is_arithmetic<std::remove_reference_t<TSource>>::value>>
			: is_safely_convertible_between_arithmetic_values<std::remove_reference_t<TSource>, TTarget>
		{
			static_assert(std::is_arithmetic<TTarget>::value, "");
		};
	
		// dispatch requried because logical operators are not lazy during template instantiation
		template <typename TSource, typename TTarget, typename=void>
		struct is_safely_convertible_to_value : std::false_type {};

		template <typename TSource, typename TTarget>
		struct is_safely_convertible_to_value<TSource, TTarget, std::enable_if_t<std::is_class<TTarget>::value>>
			// std::is_convertible already checks for existing conversion operator/ctor
			// in addition, prevent accidental slicing
			: std::integral_constant<
				bool,
				std::is_same<std::remove_cv_t<TTarget>, tc::remove_cvref_t<TSource>>::value || !tc::is_base_of<TTarget, std::remove_reference_t<TSource>>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value, "");
		};


		template <typename TSource, typename TTarget>
		struct is_safely_convertible_to_value<TSource, TTarget, std::enable_if_t<std::is_arithmetic<TTarget>::value>>
			// disable unwanted arithmetic conversions
			: std::integral_constant<
				bool,
				is_safely_convertible_to_arithmetic_value<TSource, TTarget>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value, "");
		};


		template <typename TSource, typename TTarget>
		struct is_safely_convertible_to_value<TSource, TTarget, std::enable_if_t<std::is_enum<TTarget>::value || std::is_pointer<TTarget>::value >>
			// std::is_convertible does the right thing for enums
			: std::true_type
		{
			static_assert(!std::is_reference<TTarget>::value, "");
		};
	}

	// Disable unwanted conversions despite true==std::is_convertible<TSource, TTarget>::value
	// See some static_assert in type_traits.cpp.
	template<typename TSource, typename TTarget>
	struct is_safely_convertible : 
		std::integral_constant<
			bool,	
			std::is_convertible<TSource, TTarget>::value
			&& (
				std::is_reference<TTarget>::value
				? tc::creates_no_reference_to_temporary<TSource, TTarget>::value
				: is_safely_convertible_adl_barrier::is_safely_convertible_to_value<TSource, TTarget>::value
			)
		>
	{};

	template<typename TTarget, typename TSource>
	struct is_safely_assignable final
		: std::integral_constant<
			bool,
			std::is_assignable<TTarget, TSource>::value
			&& is_safely_convertible_adl_barrier::is_safely_convertible_to_value<TSource, std::remove_reference_t<TTarget>>::value
		>
	{};
}


////////////////////////////////////////////////
// initialization of TTarget member/element by TSource

constexpr int forbidden_initialization = 0; // would initialize a reference to a temporary
constexpr int explicit_initialization = 1;
constexpr int implicit_initialization = 3; // we want to bit-and these values in order to find the minimum

template<typename TTarget, typename TSource>
struct initialization_restrictiveness : std::integral_constant<
	int,
	tc::is_safely_convertible<TSource, TTarget>::value
		? implicit_initialization
		: tc::creates_no_reference_to_temporary< TSource, TTarget >::value
			? explicit_initialization
			: forbidden_initialization
> {
	static_assert(!std::is_rvalue_reference<TTarget>::value, "");
};

// WORKAROUND until compiler supports C++17 fold expressions
#define FOLD_EXPRESSION(op, type, neutral, name) \
template <type ...args> \
struct fold_expression_ ## name; \
\
template <> \
struct fold_expression_ ## name<> \
	: std::integral_constant<type, neutral> \
{}; \
\
template <type head, type ...tail> \
struct fold_expression_ ## name<head, tail...> \
	: std::integral_constant<type, head op fold_expression_ ## name <tail...>::value > \
{};

FOLD_EXPRESSION(&, int, -1, bitwise_and) // equivalent to ( args && ... ) , currently only for int

// initialize N elements of TTarget by forwarding one arg per element
template <typename TTarget, typename ...Args>
struct accumulated_initialization_restrictiveness final
	: fold_expression_bitwise_and<initialization_restrictiveness<TTarget, Args>::value...>
{};

FOLD_EXPRESSION(|, int, 0, bitwise_or);