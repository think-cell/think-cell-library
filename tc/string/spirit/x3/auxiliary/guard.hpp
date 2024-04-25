/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_GUARD_FERBRUARY_02_2013_0649PM)
#define BOOST_SPIRIT_X3_GUARD_FERBRUARY_02_2013_0649PM

#include "../support/context.hpp"
#include "../directive/expect.hpp"

namespace boost { namespace spirit { namespace x3
{
	enum class error_handler_result
	{
		fail
	  , retry
	  , accept
	  , rethrow
	};

	template <typename Subject, typename Handler>
	struct guard : unary_parser<Subject, guard<Subject, Handler>>
	{
		typedef unary_parser<Subject, guard<Subject, Handler>> base_type;
		static bool const is_pass_through_unary = true;

		constexpr guard(Subject const& subject, Handler handler)
		  : base_type(subject), handler(handler) {}

		template <typename Iterator, typename Context
		  , typename RuleContext, typename Attribute>
		bool parse(Iterator& first, Iterator const& last
		  , Context const& context, RuleContext& rcontext, Attribute& attr) const
		{
			for (;;)
			{
				Iterator i = first;
				if (this->subject.parse(i, last, context, rcontext, attr))
				{
					first = i;
					return true;
				} else if (has_expectation_failure(context)) {
					switch (handler(first, last, get_expectation_failure(first, context), context))
					{
						case error_handler_result::fail:
							reset_expectation_failure(context);
							return false;
						case error_handler_result::retry:
							reset_expectation_failure(context);
							continue;
						case error_handler_result::accept:
							reset_expectation_failure(context);
							return true;
						case error_handler_result::rethrow:
							return false;
					}
				}
				return false;
			}
			return false;
		}

		Handler handler;
	};
}}}

#endif
