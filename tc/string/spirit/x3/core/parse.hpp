/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_PARSE_APRIL_16_2006_0442PM)
#define BOOST_SPIRIT_X3_PARSE_APRIL_16_2006_0442PM

#include "../support/context.hpp"
#include "parser.hpp"
#include "skip_over.hpp"
#include <boost/iterator/iterator_concepts.hpp>

namespace boost { namespace spirit { namespace x3
{
	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, spirit_parser Parser, typename Attribute>
	inline bool
	parse_main(
		Iterator& first
	  , Iterator last
	  , Parser const& p
	  , Attribute& attr)
	{
		// Make sure the iterator is at least a readable forward traversal iterator.
		// If you got a compilation error here, then you are using a weaker iterator
		// while calling this function, you need to supply a readable forward traversal
		// iterator instead.
		BOOST_CONCEPT_ASSERT((boost_concepts::ReadableIteratorConcept<Iterator>));
		BOOST_CONCEPT_ASSERT((boost_concepts::ForwardTraversalConcept<Iterator>));

		std::optional<expectation_failure<Iterator>> failure;
		return p.parse(first, last, make_context<expectation_failure_tag>(failure), unused, attr);
	}

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, typename Parser, typename Attribute>
	inline bool
	parse(
		Iterator& first
	  , Iterator last
	  , Parser const& p
	  , Attribute& attr)
	{
		return parse_main(first, last, p, attr);
	}

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, typename Parser, typename Attribute>
	inline bool
	parse(
		Iterator const& first_
	  , Iterator last
	  , Parser const& p
	  , Attribute& attr)
	{
		Iterator first = first_;
		return parse_main(first, last, p, attr);
	}

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, typename Parser>
	inline bool
	parse(
		Iterator& first
	  , Iterator last
	  , Parser const& p)
	{
		return parse_main(first, last, p, unused);
	}

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, typename Parser>
	inline bool
	parse(
		Iterator const& first_
	  , Iterator last
	  , Parser const& p)
	{
		Iterator first = first_;
		return parse_main(first, last, p, unused);
	}

	///////////////////////////////////////////////////////////////////////////
	enum class skip_flag
	{
		post_skip,      // force post-skipping in phrase_parse()
		dont_post_skip  // inhibit post-skipping in phrase_parse()
	};

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, spirit_parser Parser, spirit_parser Skipper, typename Attribute>
	inline bool
	phrase_parse_main(
		Iterator& first
	  , Iterator last
	  , Parser const& p
	  , Skipper const& s
	  , Attribute& attr
	  , skip_flag post_skip = skip_flag::post_skip)
	{
		// Make sure the iterator is at least a readable forward traversal iterator.
		// If you got a compilation error here, then you are using a weaker iterator
		// while calling this function, you need to supply a readable forward traversal
		// iterator instead.
		BOOST_CONCEPT_ASSERT((boost_concepts::ReadableIteratorConcept<Iterator>));
		BOOST_CONCEPT_ASSERT((boost_concepts::ForwardTraversalConcept<Iterator>));

		static_assert(!std::is_same<Skipper, unused_type>::value,
			"Error! Skipper cannot be unused_type.");

		auto skipper_ctx = make_context<skipper_tag>(s);
		std::optional<expectation_failure<Iterator>> failure;
		bool r = p.parse(first, last, make_context<expectation_failure_tag>(failure, skipper_ctx), unused, attr);
		if (post_skip == skip_flag::post_skip)
			x3::skip_over(first, last, skipper_ctx);
		return r;
	}

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, typename Parser, typename Skipper, typename Attribute>
	inline bool
	phrase_parse(
		Iterator& first
	  , Iterator last
	  , Parser const& p
	  , Skipper const& s
	  , Attribute& attr
	  , skip_flag post_skip = skip_flag::post_skip)
	{
		return phrase_parse_main(first, last, p, s, attr, post_skip);
	}

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, typename Parser, typename Skipper, typename Attribute>
	inline bool
	phrase_parse(
		Iterator const& first_
	  , Iterator last
	  , Parser const& p
	  , Skipper const& s
	  , Attribute& attr
	  , skip_flag post_skip = skip_flag::post_skip)
	{
		Iterator first = first_;
		return phrase_parse_main(first, last, p, s, attr, post_skip);
	}

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, typename Parser, typename Skipper>
	inline bool
	phrase_parse(
		Iterator& first
	  , Iterator last
	  , Parser const& p
	  , Skipper const& s
	  , skip_flag post_skip = skip_flag::post_skip)
	{
		return phrase_parse_main(first, last, p, s, unused, post_skip);
	}

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, typename Parser, typename Skipper>
	inline bool
	phrase_parse(
		Iterator const& first_
	  , Iterator last
	  , Parser const& p
	  , Skipper const& s
	  , skip_flag post_skip = skip_flag::post_skip)
	{
		Iterator first = first_;
		return phrase_parse_main(first, last, p, s, unused, post_skip);
	}

	///////////////////////////////////////////////////////////////////////////
	template <typename Iterator, typename Skipper>
	struct phrase_parse_context
	{
		typedef decltype(
			make_context<expectation_failure_tag>(
				std::declval<
					std::optional<expectation_failure<Iterator>>&
				>(),
				make_context<skipper_tag>(std::declval<Skipper const&>())
			)
		)
		type;
	};
}}}

#endif
