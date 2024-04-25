/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_EXPECT_MARCH_16_2012_1024PM)
#define BOOST_SPIRIT_X3_EXPECT_MARCH_16_2012_1024PM

#include "../support/context.hpp"
#include "../core/parser.hpp"
#include "../core/detail/parse_into_container.hpp"

#include <boost/config.hpp> // for BOOST_SYMBOL_VISIBLE
#include <boost/throw_exception.hpp>
#include <stdexcept>

namespace boost { namespace spirit { namespace x3
{
	namespace detail {
		inline bool has_expectation_failure_impl(bool& failure) {
			return failure;
		}

		template <typename Iterator>
		bool has_expectation_failure_impl(
			std::optional<expectation_failure<Iterator>>& failure
		) {
			return static_cast<bool>(failure);
		}

		template <typename Iterator, typename Subject>
		void set_expectation_failure_impl(
			Iterator const& where,
			Subject const& subject,
			bool& failure
		) {
			failure = true;
		}

		template <typename Iterator, typename Subject>
		void set_expectation_failure_impl(
			Iterator const& where,
			Subject const& subject,
			std::optional<expectation_failure<Iterator>>& failure
		) {
			failure = expectation_failure<Iterator>{
				where,
				what(subject)
			};
		}

		template <typename Iterator>
		expectation_failure<Iterator> get_expectation_failure_impl(
			Iterator const& where, bool& failure
		) {
			return {where, {}};
		}

		template <typename Iterator>
		auto const& get_expectation_failure_impl(
			Iterator const&,
			std::optional<expectation_failure<Iterator>>& failure
		) {
			return *failure;
		}

		template <typename Iterator>
		void reset_expectation_failure_impl(bool& failure) {
			failure = false;
		}

		template <typename Iterator>
		void reset_expectation_failure_impl(std::optional<expectation_failure<Iterator>>& failure) {
			failure = std::nullopt;
		}
	}

	template <typename Context> 
	bool has_expectation_failure(Context const& context) {
		return detail::has_expectation_failure_impl(
			x3::get<expectation_failure_tag>(context));
	}

	template <typename Iterator, typename Subject, typename Context>
	void set_expectation_failure(
		Iterator const& where,
		Subject const& subject,
		Context const& context
	) {
		detail::set_expectation_failure_impl(
			where,
			subject,
			x3::get<expectation_failure_tag>(context)
		);
	}

	template <typename Iterator, typename Context>
	decltype(auto) get_expectation_failure(
		Iterator const& where,
		Context const& context
	) {
		return detail::get_expectation_failure_impl(
			where, x3::get<expectation_failure_tag>(context));
	}

	template <typename Context>
	void reset_expectation_failure(Context const& context) {
		detail::reset_expectation_failure_impl(
			x3::get<expectation_failure_tag>(context));
	}

	template <typename Subject>
	struct expect_directive : unary_parser<Subject, expect_directive<Subject>>
	{
		typedef unary_parser<Subject, expect_directive<Subject> > base_type;
		static bool const is_pass_through_unary = true;

		constexpr expect_directive(Subject const& subject)
		  : base_type(subject) {}

		template <typename Iterator, typename Context
		  , typename RContext, typename Attribute>
		bool parse(Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, Attribute& attr) const
		{
			bool r = this->subject.parse(first, last, context, rcontext, attr);

			if (!r)
			{
				if(!has_expectation_failure(context))
				{
					set_expectation_failure(first, this->subject, context);
				}
			}
			return r;
		}
	};

	struct expect_gen
	{
		template <spirit_parser Subject>
		constexpr expect_directive<Subject>
		operator[](Subject const& subject) const
		{
			return { subject };
		}
	};

	constexpr auto expect = expect_gen{};
}}}

namespace boost { namespace spirit { namespace x3 { namespace detail
{
	// Special case handling for expect expressions.
	template <typename Subject, typename Context, typename RContext>
	struct parse_into_container_impl<expect_directive<Subject>, Context, RContext>
	{
		template <typename Iterator, typename Attribute>
		static bool call(
			expect_directive<Subject> const& parser
		  , Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, Attribute& attr)
		{
			bool r = parse_into_container(
				parser.subject, first, last, context, rcontext, attr);

			if (!r)
			{
				if(!has_expectation_failure(context))
				{
					set_expectation_failure(first, parser.subject, context);
				}
			}
			return r;
		}
	};
}}}}

#endif
