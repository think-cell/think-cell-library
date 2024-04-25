/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_ALTERNATIVE_JAN_07_2013_1131AM)
#define BOOST_SPIRIT_X3_ALTERNATIVE_JAN_07_2013_1131AM

#include "../support/traits/attribute_of_binary.hpp"
#include "../core/parser.hpp"
#include "detail/alternative.hpp"
#include "../directive/expect.hpp"

#include <boost/variant/variant_fwd.hpp>

namespace boost { namespace spirit { namespace x3
{
	template <typename Left, typename Right>
	struct alternative : binary_parser<Left, Right, alternative<Left, Right>>
	{
		typedef binary_parser<Left, Right, alternative<Left, Right>> base_type;

		constexpr alternative(Left const& left, Right const& right)
			: base_type(left, right) {}

		template <typename Iterator, typename Context, typename RContext>
		bool parse(
			Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, unused_type) const
		{
			return this->left.parse(first, last, context, rcontext, unused) ||
				(
					!has_expectation_failure(context) &&
					this->right.parse(first, last, context, rcontext, unused)
				);
		}

		template <typename Iterator, typename Context
		  , typename RContext, typename Attribute>
		bool parse(
			Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, Attribute& attr) const
		{
			return detail::parse_alternative(this->left, first, last, context, rcontext, attr) ||
				(
					!has_expectation_failure(context) &&
					detail::parse_alternative(this->right, first, last, context, rcontext, attr)
				);
		}
	};

	template <spirit_parser Left, spirit_parser Right>
	constexpr alternative<
		Left
	  , Right>
	operator|(Left const& left, Right const& right)
	{
		return { left, right };
	}
}}}

namespace boost { namespace spirit { namespace x3 { namespace traits
{
	template <typename Left, typename Right, typename Context>
	struct attribute_of<x3::alternative<Left, Right>, Context>
		: x3::detail::attribute_of_binary<boost::variant, x3::alternative, Left, Right, Context> {};
}}}}

#endif
