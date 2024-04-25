/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman
	Copyright (c) 2013 Agustin Berge

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_SKIP_JANUARY_26_2008_0422PM)
#define BOOST_SPIRIT_X3_SKIP_JANUARY_26_2008_0422PM

#include "../support/context.hpp"
#include "../support/unused.hpp"
#include "../core/skip_over.hpp"
#include "../core/parser.hpp"
#include <boost/utility/enable_if.hpp>

namespace boost { namespace spirit { namespace x3
{
	template <typename Subject>
	struct reskip_directive : unary_parser<Subject, reskip_directive<Subject>>
	{
		typedef unary_parser<Subject, reskip_directive<Subject>> base_type;
		static bool const is_pass_through_unary = true;
		static bool const handles_container = Subject::handles_container;

		constexpr reskip_directive(Subject const& subject)
		  : base_type(subject) {}

		template <typename Iterator, typename Context
		  , typename RContext, typename Attribute>
		typename disable_if<has_skipper<Context>, bool>::type
		parse(Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, Attribute& attr) const
		{
			auto const& skipper =
				detail::get_unused_skipper(x3::get<skipper_tag>(context));

			return this->subject.parse(
				first, last
			  , make_context<skipper_tag>(skipper, context)
			  , rcontext
			  , attr);
		}
		template <typename Iterator, typename Context
		  , typename RContext, typename Attribute>
		typename enable_if<has_skipper<Context>, bool>::type
		parse(Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, Attribute& attr) const
		{
			return this->subject.parse(
				first, last
			  , context
			  , rcontext
			  , attr);
		}
	};

	template <typename Subject, typename Skipper>
	struct skip_directive : unary_parser<Subject, skip_directive<Subject, Skipper>>
	{
		typedef unary_parser<Subject, skip_directive<Subject, Skipper>> base_type;
		static bool const is_pass_through_unary = true;
		static bool const handles_container = Subject::handles_container;

		constexpr skip_directive(Subject const& subject, Skipper const& skipper)
		  : base_type(subject)
		  , skipper(skipper)
		{}

		template <typename Iterator, typename Context
		  , typename RContext, typename Attribute>
		bool parse(Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, Attribute& attr) const
		{
			return this->subject.parse(
				first, last
			  , make_context<skipper_tag>(skipper, context)
			  , rcontext
			  , attr);
		}

		Skipper const skipper;
	};

	struct reskip_gen
	{
		template <typename Skipper>
		struct skip_gen
		{
			constexpr skip_gen(Skipper const& skipper)
			  : skipper_(skipper) {}

			template <spirit_parser Subject>
			constexpr skip_directive<Subject, Skipper>
			operator[](Subject const& subject) const
			{
				return { subject, skipper_ };
			}

			Skipper skipper_;
		};

		template <typename Skipper>
		constexpr skip_gen<Skipper> const operator()(Skipper const& skipper) const
		{
			return { skipper };
		}

		template <spirit_parser Subject>
		constexpr reskip_directive<Subject>
		operator[](Subject const& subject) const
		{
			return { subject };
		}
	};

	constexpr auto skip = reskip_gen{};
}}}

#endif
