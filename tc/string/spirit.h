
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/as_lvalue.h"
#include "../base/tag_type.h"
#include "../range/subrange.h"
#include "../range/subrange.h"
#include "../algorithm/find.h"
#include "../algorithm/append.h"
#include "../algorithm/quantifier.h"
#include "../algorithm/equal.h"
#include "../static_vector.h"

#include "ascii.h"
#include "char.h"

#include <boost/version.hpp>

#ifndef __clang__
MODIFY_WARNINGS_BEGIN(
	((disable)(4127)) // conditional expression is constant
	((disable)(4459)) // declaration hides global declaration
)
#else
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wsign-compare"
#pragma clang diagnostic ignored "-Wunknown-warning-option"
#pragma clang diagnostic ignored "-Wdeprecated-copy"
#endif
#include <boost/spirit/home/x3.hpp>
#ifndef __clang__
MODIFY_WARNINGS_END
#else
#pragma clang diagnostic pop
#endif
#include <boost/fusion/adapted/std_tuple.hpp>

// We prefer x3::rule to simple parser. Because:
//   1. parsers are copied around in spirit.
//   2. x3::rule is a special parser reference which does not store the actual parser inside.
// The standard way to define a namespace scope x3::rule is like below:
//   x3::rule<struct SIdXXX(, Attribute)> const ruleXXX;
//   auto const ruleXXX_def = actual_parser;
//   BOOST_SPIRIT_DEFINE(ruleXXX)
// Notes:
//   1. We don't have to wrap actual_parser with tc::attr_is or x3::omit.
//   2. The attribute can be propagated exactly as in tc::attr_is.

namespace x3 = boost::spirit::x3;
namespace tc {
	template< typename Rng, typename Expr, typename... Attr>
	bool parse( Rng const& rng, Expr const& expr, Attr&... attr ) MAYTHROW {
		if constexpr( 2 <= sizeof...(Attr) ) {
			return tc::parse(rng, expr, tc::as_lvalue(std::tie(attr...)));
		} else {
			try {
				return x3::parse( tc::begin(rng), tc::end(rng), expr, attr... ); // MAYTHROW
			} catch (x3::expectation_failure<tc::decay_t<decltype(tc::begin(rng))>> const&) {
				return false;
			}
		}
	}
	template< typename Iterator, typename Expr, typename... Attr>
	bool parse_iterator( Iterator& itBegin, Iterator const& itEnd, Expr const& expr, Attr&... attr ) MAYTHROW {
		if constexpr( 2 <= sizeof...(Attr) ) {
			return tc::parse_iterator(itBegin, itEnd, expr, tc::as_lvalue(std::tie(attr...))); // MAYTHROW
		} else {
			auto itBegin2=itBegin;
			try {
				if(x3::parse( itBegin, itEnd, expr, attr... )) return true; // MAYTHROW
			} catch (x3::expectation_failure<Iterator> const&) {}
			itBegin=itBegin2;
			return false;
		}
	}
	template< typename Rng, typename Expr, typename... Attr>
	bool parse_consume( Rng & rng, Expr const& expr, Attr&... attr ) MAYTHROW {
		if constexpr( 2 <= sizeof...(Attr) ) {
			return tc::parse_consume(rng, expr, tc::as_lvalue(std::tie(attr...))); // MAYTHROW
		} else {
			try {
				auto itBegin = tc::begin(rng);
				if(x3::parse( itBegin, tc::end(rng), expr, attr... )) { // MAYTHROW
					tc::drop_inplace( rng, itBegin );
					return true;
				}
			} catch (x3::expectation_failure<tc::decay_t<decltype(tc::begin(rng))>> const&) {}
			return false;
		}
	}
	template< typename Rng, typename Expr, typename Skipper, typename... Attr>
	bool phrase_parse( Rng const& rng, Expr const& expr, Skipper const& skipper, Attr&... attr ) MAYTHROW {
		if constexpr( 2 <= sizeof...(Attr) ) {
			return tc::phrase_parse(rng, expr, skipper, tc::as_lvalue(std::tie(attr...)));
		} else {
			try {
				return x3::phrase_parse( tc::begin(rng), tc::end(rng), expr, skipper, attr... ); // MAYTHROW
			} catch (x3::expectation_failure<tc::decay_t<decltype(tc::begin(rng))>> const&) {
				return false;
			}
		}
	}

	namespace no_adl {
		template <typename Expr>
		struct x3parser : Expr {
			constexpr x3parser(Expr expr) : Expr(tc_move(expr)) {}

			template <typename Rng>
			[[nodiscard]] constexpr auto operator()(Rng const& rng) const MAYTHROW {
				std::optional<typename Expr::attribute_type> oresult(std::in_place);
				if (!tc::parse(rng, *this > x3::eoi, *oresult)) {
					oresult=std::nullopt;
				}
				return oresult;
			}
		};
	}
	template <typename Expr>
	constexpr auto x3parser(Expr&& expr) return_ctor_MAYTHROW(no_adl::x3parser<std::remove_cvref_t<Expr>>, (tc_move_if_owned(expr)))

	namespace no_adl {
		struct any_one_impl final : x3::char_parser<any_one_impl> {
			static bool const has_attribute = false;
			using attribute_type = x3::unused_type;

			constexpr bool test(tc::unused /*char*/, tc::unused /*context*/) const& noexcept {
				return true;
			}
		};
	}
	inline constexpr auto any_one = no_adl::any_one_impl();

	namespace no_adl {
		template<typename T>
		struct one_impl final : x3::char_parser<one_impl<T>> {
			static bool const has_attribute = true;
			using attribute_type = T;

			template<typename U>
			bool test(U const u, tc::unused /*context*/) const& noexcept {
				return parse_match(u);
			}

			template<typename U>
			bool parse_match(U const u) const& noexcept {
				if constexpr (requires { T::constructable_from(std::declval<T>()); }) {
					return T::constructable_from(u);
				} else {
					static_assert(tc::safely_constructible_from<T, U const&>);
					return true;
				}
			}
		};
	}
	template<typename T>
	constexpr auto one = no_adl::one_impl<T>();

	namespace no_adl {
		template<typename Rng>
		struct char_set final: x3::char_parser<char_set<Rng>> {
			static bool const has_attribute = true;
			using attribute_type = tc::range_value_t<Rng>;

			constexpr char_set(Rng&& rng) noexcept
				: m_rng(tc::aggregate_tag, tc_move_if_owned(rng)) {}

			template <typename CharType, typename Context>
			bool test(CharType const& ch, Context& context) const& noexcept {
				return tc::any_of(*m_rng, [&](auto const& chAllowed) noexcept {
					return tc::equal_to_or_parse_match(ch, chAllowed);
				});
			}
		private:
			tc::reference_or_value<Rng> m_rng;
		};
	}
	template<typename Rng>
	constexpr no_adl::char_set<Rng> static_char_class(Rng&& rng) noexcept {
		return tc_move_if_owned(rng);
	}

	namespace no_adl {
		template<typename String>
		struct lit_impl final: x3::parser<lit_impl<String>> {
			static bool const has_attribute = false;
			using attribute_type = x3::unused_type;

			explicit constexpr lit_impl(String&& str) noexcept: m_str(tc::aggregate_tag, tc_move_if_owned(str)) {}

			template<typename Iterator, typename Context, typename Attribute>
			bool parse(Iterator& first, Iterator const& last, Context const& context, x3::unused_type, Attribute& attr) const& {
				x3::skip_over(first, last, context);
				if(auto const first_=tc::starts_with<tc::return_border_or_null>(tc::make_iterator_range(first, last), *m_str)) {
					first=first_;
					return true;
				}
				return false;
			}
		private:
			tc::reference_or_value<String> m_str;
		};
	}
	template<typename String>
	constexpr auto lit(String&& str) noexcept {
		return no_adl::lit_impl<String>(tc_move_if_owned(str));
	}

	namespace no_adl {
		// tc::attr_is<T>[parser] directive creates an x3::rule_definition (an x3::parser with a customized attribute type).
		// When specifying an attribute, in some cases the attribute can be filled by the parser without any additional code:
		//   auto const parser = tc::attr_is<tc::string<char>>[+x3::alpha];
		// In other cases, you may have to write a semantic action and access the attribute via x3::_val(ctx) to fill it:
		//   auto const parser = tc::attr_is<tc::string<char>>[+x3::alpha[([](auto const& ctx) noexcept {
		//     tc::append(
		//       x3::_val(ctx)/*attribute of innermost x3::rule_definition, which is what is specified in tc::attr_is<>*/,
		//       tc::single(x3::_attr(ctx))/*automatic attribute of the parser attached; in this case x3::alpha is producing char*/
		//     );
		//   })]];
		template<typename T>
		struct attr_is_type {
			template<typename Expr>
			constexpr auto operator[](Expr&& expr) const& noexcept {
				// returns a x3::rule_definition with attr_is_id, Attribute type of T and rhs parser of x3::as_parser(expr)
				return x3::rule<struct attr_is_id, T>{"attr_is"} = x3::as_parser(std::forward<Expr>(expr));
			}
		};
	}
	template<typename T>
	inline constexpr no_adl::attr_is_type<T> attr_is = {};

	namespace no_adl {
		struct asciiblank final: x3::char_parser<asciiblank> {
			static bool const has_attribute = false;
			using attribute_type = x3::unused_type;

			template <typename CharType>
			static bool test(CharType const& ch, tc::unused /*context*/) noexcept {
				return tc::isasciiblank(ch);
			}
		};

		struct asciispace final: x3::char_parser<asciispace> {
			static bool const has_attribute = false;
			using attribute_type = x3::unused_type;

			template <typename CharType>
			static bool test(CharType const& ch, tc::unused /*context*/) noexcept {
				return tc::isasciispace(ch);
			}
		};
	}
	inline constexpr auto asciiblank = no_adl::asciiblank{};
	inline constexpr auto asciispace = no_adl::asciispace{};

	template <bool bSigned>
	struct xml_percentage_policies final : std::conditional_t<bSigned, x3::real_policies<double>, x3::ureal_policies<double>> {
		static constexpr bool allow_leading_dot = false;
		static constexpr bool allow_trailing_dot = false;
		// do not parse exponents
		template<typename Iterator> static bool parse_exp(Iterator& first, Iterator const& last) noexcept { return false; }
		// do not parse NANs
		template<typename Iterator, typename Attribute> static bool parse_nan(Iterator& first, Iterator const& last, Attribute& attr) noexcept { return false; }
		// do not parse INFs
		template<typename Iterator, typename Attribute> static bool parse_inf(Iterator& first, Iterator const& last, Attribute& attr) noexcept { return false; }
	};

	inline constexpr auto asciixdigit = tc::one<tc::char_asciidigit> | tc::one<tc::restricted_enum<char, 'A', 'F'>> | tc::one<tc::restricted_enum<char, 'a', 'f'>>;

	namespace no_adl {
		template<tc::char_type T>
		struct char_encoding final {
			using char_type = T;

			template<typename Char> requires std::same_as<Char, T>
			static constexpr bool ischar(Char ch) noexcept {
				return true;
			}
			static ::boost::uint32_t toucs4(int const ch) noexcept { // for debug only
				return ch;
			}
		};

		template<tc::char_type T, T tFirst, T tLast>
		struct char_encoding<tc::restricted_enum<T, tFirst, tLast>> final {
			using char_type = tc::restricted_enum<T, tFirst, tLast>;

			template<tc::char_like Char>
			static constexpr bool ischar(Char ch) noexcept {
				return char_type(tFirst) <= ch && ch <= char_type(tLast); // We reuse the SFINAE from the comparison operators of restricted_enum.
			}
			static ::boost::uint32_t toucs4(int const ch) noexcept { // for debug only
				return ch;
			}
		};
	}
	using no_adl::char_encoding;

	template<typename Char, typename T>
	using symbols = x3::symbols_parser<tc::char_encoding<Char>, T>;

	namespace no_adl {
		struct move_attr_to_val_impl final {
			template<typename Context>
			void operator()(Context& ctx) noexcept {
				x3::_val(ctx)=tc_move_always(x3::_attr(ctx));
			}
		};
		template<typename T>
		struct move_attr_to_ext_val_impl final {
			constexpr move_attr_to_ext_val_impl(T& t) noexcept: m_t(t) {}
			template<typename Context>
			void operator()(Context& ctx) noexcept {
				m_t=tc_move_always(x3::_attr(ctx));
			}
		private:
			T& m_t;
		};
	}

	template<typename T>
	constexpr auto move_attr_to_val(T& t) noexcept {
		return no_adl::move_attr_to_ext_val_impl<T>(t);
	}

	constexpr auto move_attr_to_val() noexcept {
		return no_adl::move_attr_to_val_impl();
	}
}

///////////////////////////////
// x3::with_val

namespace boost::spirit::x3 {
	template<typename Subject, typename ID>
	struct with_val_directive : unary_parser<Subject, with_val_directive<Subject, ID>>
	{
		using base_type = typename with_val_directive::unary_parser;
		static bool const is_pass_through_unary = true;
		static bool const handles_container = Subject::handles_container;
		using subject_type = Subject;

		constexpr with_val_directive(Subject const& subject): base_type(subject) {}

		template <typename Iterator, typename Context, typename RContext, typename Attribute>
		bool parse(Iterator& first, Iterator const& last, Context const& context, RContext& rcontext, Attribute& attr) const
		{
			return this->subject.parse(
				first, last
			  , make_context<ID>(rcontext, context)
			  , rcontext
			  , attr);
		}
	};

	template <typename ID>
	struct with_val_gen
	{	
		template <typename Subject>
		constexpr with_val_directive<typename extension::as_parser<Subject>::value_type, ID>
		operator[](Subject const& subject) const
		{
			return { as_parser(subject) };
		}
	};

	template<typename ID>
	inline constexpr auto with_val = with_val_gen<ID>{};
}

///////////////////////////////
// x3::lazy

namespace boost::spirit::x3
{
	template <typename context_tag>
	struct lazy_parser : parser<lazy_parser<context_tag>>
	{
		using base_type = typename lazy_parser::parser;

		template <typename Iterator, typename Context, typename RContext, typename Attribute>
		bool parse(Iterator& first, Iterator const& last, Context const& context, RContext& rcontext, Attribute& attr) const
		{
			return x3::get<context_tag>(context).parse(first, last, context, rcontext, attr);
		}
	};

	template <typename context_tag>
	inline constexpr auto lazy = lazy_parser<context_tag>{};
}

namespace boost::spirit::x3::traits
{
	template <typename context_tag, typename Context>
	struct attribute_of<x3::lazy_parser<context_tag>, Context>
		: attribute_of<
			typename remove_cv<typename remove_reference<
				decltype(x3::get<context_tag>(std::declval<Context>()))
			>::type>::type,
			Context
		> {};

	template <typename context_tag, typename Context>
	struct has_attribute<x3::lazy_parser<context_tag>, Context>
		: has_attribute<
			typename remove_cv<typename remove_reference<
				decltype(x3::get<context_tag>(std::declval<Context>()))
			>::type>::type,
			Context
		> {};
}

///////////////////////////////
// static_vector support

namespace boost::spirit::x3::traits
{
	template <typename T, tc::static_vector_size_t N>
	struct push_back_container<tc::static_vector<T, N>, void>
	{
		template <typename Value>
		static bool call(tc::static_vector<T, N>& c, Value&& val)
		{
			tc::cont_emplace_back(c, tc_move_if_owned(val));
			return true;
		}
	};
	template <typename T, tc::static_vector_size_t N>
	struct append_container<tc::static_vector<T, N>, void>
	{
		template <typename Iterator>
		static bool call(tc::static_vector<T, N>& c, Iterator first, Iterator last)
		{
			tc::append(c, tc::make_iterator_range(first, last));
			return true;
		}
	};
	template <typename T, tc::static_vector_size_t N>
	struct is_empty_container<tc::static_vector<T, N>, void>
	{
		static bool call(tc::static_vector<T, N> const& c) noexcept
		{
			return tc::empty(c);
		}
	};
}
