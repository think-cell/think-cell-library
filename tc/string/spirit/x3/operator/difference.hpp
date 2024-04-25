/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_DIFFERENCE_FEBRUARY_11_2007_1250PM)
#define BOOST_SPIRIT_X3_DIFFERENCE_FEBRUARY_11_2007_1250PM

#include "../support/traits/attribute_of.hpp"
#include "../support/traits/has_attribute.hpp"
#include "../core/parser.hpp"
#include "../directive/expect.hpp"

namespace boost { namespace spirit { namespace x3
{
	template <typename Left, typename Right>
	struct difference : binary_parser<Left, Right, difference<Left, Right>>
	{
		typedef binary_parser<Left, Right, difference<Left, Right>> base_type;
		static bool const handles_container = Left::handles_container;

		constexpr difference(Left const& left, Right const& right)
		  : base_type(left, right) {}

		template <typename Iterator, typename Context
		  , typename RContext, typename Attribute>
		bool parse(Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, Attribute& attr) const
		{
			// Try Right first
			Iterator start = first;
			if (this->right.parse(first, last, context, rcontext, unused))
			{
				// Right succeeds, we fail.
				first = start;
				return false;
			} else if(has_expectation_failure(context)) {
				return false;
			}
			// Right fails, now try Left
			return this->left.parse(first, last, context, rcontext, attr);
		}

		template <typename Left_, typename Right_>
		constexpr difference<Left_, Right_>
		make(Left_ const& left, Right_ const& right) const
		{
			return { left, right };
		}
	};

	template <spirit_parser Left, spirit_parser Right>
	constexpr difference<
		Left
	  , Right>
	operator-(Left const& left, Right const& right)
	{
		return { left, right };
	}
}}}

namespace boost { namespace spirit { namespace x3 { namespace traits
{
	template <typename Left, typename Right, typename Context>
	struct attribute_of<x3::difference<Left, Right>, Context>
		: attribute_of<Left, Context> {};

	template <typename Left, typename Right, typename Context>
	struct has_attribute<x3::difference<Left, Right>, Context>
		: has_attribute<Left, Context> {};
}}}}

#endif
