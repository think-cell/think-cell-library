/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman
	http://spirit.sourceforge.net/

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_X3_CONTEXT_JAN_4_2012_1215PM)
#define BOOST_SPIRIT_X3_CONTEXT_JAN_4_2012_1215PM

#include "unused.hpp"
#include <boost/mpl/identity.hpp>
#include <type_traits>

namespace boost { namespace spirit { namespace x3
{
	template <typename ID, typename T, typename Next = unused_type>
	struct context
	{
		T& val;
		Next next;
	};

	template <typename ID, typename T, typename Next>
	struct context<ID, T, Next const&>
	{
		T& val;
		Next const& next;
	};

	namespace detail {
		template <typename ID, typename T, typename Next>
		T& get(context<ID, T, Next> const& ctx, mpl::identity<ID>)
		{
			return ctx.val;
		}

		template <typename ID>
		unused_type get(unused_type, mpl::identity<ID>)
		{
			return {};
		}

		template <typename ID, typename T, typename Next, typename DifferentID>
		decltype(auto) get(context<ID, T, Next> const& ctx, mpl::identity<DifferentID> id)
		{
			return get(ctx.next, id);
		}
	}

	template <typename Tag, typename Context>
	inline decltype(auto) get(Context const& ctx)
	{
		return detail::get(ctx, mpl::identity<Tag>());
	}

	namespace detail {
		template <typename ID, typename T, typename Next>
		auto const& remove(context<ID, T, Next> const& ctx, mpl::identity<ID>)
		{
			return ctx.next;
		}

		template <typename ID, typename T, typename Next, typename DifferentID>
		auto remove(context<ID, T, Next> const& ctx, mpl::identity<DifferentID> id)
		{
			return context<ID, T, decltype(remove(ctx.next, id))>{ ctx.val, remove(ctx.next, id) };
		}

		template <typename ID, typename FoundVal, typename T, typename Next, std::enable_if_t<!std::is_same<FoundVal, unused_type>::value>* =nullptr>
		inline auto make_context(T& val, Next const& next) -> context<ID, T, decltype(remove(next, mpl::identity<ID>()))>
		{
			return { val, remove(next, mpl::identity<ID>()) };
		}

		// optimization: don't rebuild the context when ID could not be found
		template <typename ID, typename FoundVal, typename T, typename Next, std::enable_if_t<std::is_same<FoundVal, unused_type>::value>* =nullptr>
		inline context<ID, T, Next const&> make_context(T& val, Next const& next)
		{
			return { val, next };
		}
	}

	template <typename ID, typename T, typename Next>
	inline decltype(auto) make_context(T& val, Next const& next)
	{
		return detail::make_context<ID, decltype(x3::get<ID>(next))>(val, next);
	}

	template <typename ID, typename T>
	inline context<ID, T> make_context(T& val)
	{
		return { val };
	}

	namespace detail
	{
		template <typename ID, typename T, typename Next, typename FoundVal>
		inline Next const&
		make_unique_context(T& /* val */, Next const& next, FoundVal&)
		{
			return next;
		}

		template <typename ID, typename T, typename Next>
		inline context<ID, T, Next const&>
		make_unique_context(T& val, Next const& next, unused_type)
		{
			return { val, next };
		}
	}

	template <typename ID, typename T, typename Next>
	inline auto
	make_unique_context(T& val, Next const& next)
	{
		return detail::make_unique_context<ID>(val, next, x3::get<ID>(next));
	}
}}}

#endif
