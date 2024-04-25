/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman
	Copyright (c) 2001-2011 Hartmut Kaiser

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_KLEENE_JANUARY_07_2007_0818AM)
#define BOOST_SPIRIT_X3_KLEENE_JANUARY_07_2007_0818AM

#include "../core/parser.hpp"
#include "../support/traits/container_traits.hpp"
#include "../support/traits/attribute_of.hpp"
#include "../core/detail/parse_into_container.hpp"
#include "../directive/expect.hpp"

namespace boost { namespace spirit { namespace x3
{
	template <typename Subject>
	struct kleene : unary_parser<Subject, kleene<Subject>>
	{
		typedef unary_parser<Subject, kleene<Subject>> base_type;
		static bool const handles_container = true;

		constexpr kleene(Subject const& subject)
		  : base_type(subject) {}

		template <typename Iterator, typename Context
		  , typename RContext, typename Attribute>
		bool parse(Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, Attribute& attr) const
		{
			while (detail::parse_into_container(
				this->subject, first, last, context, rcontext, attr))
				;
			return !has_expectation_failure(context);
		}
	};

	template <spirit_parser Subject>
	constexpr kleene<Subject>
	operator*(Subject const& subject)
	{
		return { subject };
	}
}}}

namespace boost { namespace spirit { namespace x3 { namespace traits
{
	template <typename Subject, typename Context>
	struct attribute_of<x3::kleene<Subject>, Context>
		: build_container<
			typename attribute_of<Subject, Context>::type> {};
}}}}

#endif
