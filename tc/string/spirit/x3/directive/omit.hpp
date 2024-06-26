/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_OMIT_MARCH_24_2007_0802AM)
#define BOOST_SPIRIT_X3_OMIT_MARCH_24_2007_0802AM

#include "../support/unused.hpp"
#include "../core/parser.hpp"

namespace boost { namespace spirit { namespace x3
{
	///////////////////////////////////////////////////////////////////////////
	// omit_directive forces the attribute of subject parser
	// to be unused_type
	///////////////////////////////////////////////////////////////////////////
	template <typename Subject>
	struct omit_directive : unary_parser<Subject, omit_directive<Subject>>
	{
		typedef unary_parser<Subject, omit_directive<Subject> > base_type;
		typedef unused_type attribute_type;
		static bool const has_attribute = false;

		typedef Subject subject_type;
		constexpr omit_directive(Subject const& subject)
		  : base_type(subject) {}

		template <typename Iterator, typename Context, typename RContext>
		bool parse(Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, unused_type) const
		{
			return this->subject.parse(first, last, context, rcontext, unused);
		}
	};

	struct omit_gen
	{
		template <spirit_parser Subject>
		constexpr omit_directive<Subject>
		operator[](Subject const& subject) const
		{
			return { subject };
		}
	};

	constexpr auto omit = omit_gen{};
}}}

#endif
