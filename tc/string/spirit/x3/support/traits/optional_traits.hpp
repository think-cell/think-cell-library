/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman
	Copyright (c) 2001-2011 Hartmut Kaiser
	http://spirit.sourceforge.net/

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_OPTIONAL_TRAITS_FEBRUARY_06_2007_1001AM)
#define BOOST_SPIRIT_X3_OPTIONAL_TRAITS_FEBRUARY_06_2007_1001AM

#include "../unused.hpp"
#include <boost/mpl/identity.hpp>
#include <optional>

namespace boost { namespace spirit { namespace x3 { namespace traits
{
	///////////////////////////////////////////////////////////////////////////
	template <typename T, typename Enable = void>
	struct is_optional
	  : mpl::false_
	{};

	template <typename T>
	struct is_optional<std::optional<T>>
	  : mpl::true_
	{};

	///////////////////////////////////////////////////////////////////////////
	// build_optional
	//
	// Build a std::optional from T. Return unused_type if T is unused_type.
	///////////////////////////////////////////////////////////////////////////
	template <typename T>
	struct build_optional
	{
		typedef std::optional<T> type;
	};

	template <typename T>
	struct build_optional<std::optional<T> >
	{
		typedef std::optional<T> type;
	};

	template <>
	struct build_optional<unused_type>
	{
		typedef unused_type type;
	};

	///////////////////////////////////////////////////////////////////////////
	// optional_value
	//
	// Get the optional's value_type. Handles unused_type as well.
	///////////////////////////////////////////////////////////////////////////
	template <typename T>
	struct optional_value : mpl::identity<T> {};

	template <typename T>
	struct optional_value<std::optional<T> >
	  : mpl::identity<T> {};

	template <>
	struct optional_value<unused_type>
	  : mpl::identity<unused_type> {};

	template <>
	struct optional_value<unused_type const>
	  : mpl::identity<unused_type> {};

}}}}

#endif
