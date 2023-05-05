
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
#include "value_restrictive.h"

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
		template<typename Char>
		struct char_encoding;

		template<tc::char_type Char>
		struct char_encoding<Char> final {
			using char_type = Char;

			static constexpr bool ischar(int) noexcept {
				return true;
			}
			static ::boost::uint32_t toucs4(int ch) noexcept { // for debug only
				return ch;
			}
		};

		template<tc::char_type T, T tFirst, T tLast>
		struct char_encoding<tc::value_restrictive<T, tFirst, tLast>> final {
			using char_type = tc::value_restrictive<T, tFirst, tLast>;

			template<tc::char_like Char>
			static constexpr bool ischar(Char ch) noexcept {
				return char_type(tFirst) <= ch && ch <= char_type(tLast); // We reuse the SFINAE from the comparison operators of value_restrictive.
			}
			static ::boost::uint32_t toucs4(int ch) noexcept { // for debug only
				return ch;
			}
		};
	}
	using no_adl::char_encoding;

	DEFINE_TAG_TYPE(unused_type)

	template<typename Char>
	inline constexpr auto one = tc::unused_type;

	template<>
	inline constexpr auto one<char> = x3::any_char<tc::char_encoding<char>>();

	template<>
	inline constexpr auto one<wchar_t> = x3::any_char<tc::char_encoding<wchar_t>>();

#ifdef __clang__
	template<>
	inline constexpr auto one<char16_t> = x3::any_char<tc::char_encoding<char16_t>>();
#endif

	template<typename Char>
	constexpr
	x3::literal_char<
		tc::char_encoding<Char>
	> single_char(Char ch) noexcept {
		return {ch};
	}

	namespace no_adl {
		template<typename Rng>
		struct char_set final: x3::char_parser<char_set<Rng>> {
			using attribute_type = tc::range_value_t<Rng>;
			static bool const has_attribute = true;

			constexpr char_set(Rng rng) noexcept
				: m_rng(tc_move_if_owned(rng)) {}

			template <typename CharType, typename Context>
			bool test(CharType const& ch, Context& context) const& noexcept {
				static_assert( std::is_same<CharType, attribute_type>::value );
				return tc::any_of(m_rng, [&](auto const& chAllowed) noexcept {
					return 0==x3::get_case_compare<tc::char_encoding<CharType>>(context)(ch, chAllowed);
				});
			}
		private:
			tc::decay_t<Rng> m_rng;
		};
	}

	template<typename Rng>
	constexpr no_adl::char_set<tc::decay_t<Rng>> static_char_class(Rng rng) noexcept {
		return tc_move_if_owned(rng);
	}

	template<tc::char_type Char>
	constexpr x3::literal_char<
		tc::char_encoding<Char>,
		x3::unused_type
	> lit(Char ch) noexcept {
		return {ch};
	}

	template<typename Char>
	using literal_string_type = x3::literal_string<
		tc::span<Char const>,
		tc::char_encoding<Char>,
		x3::unused_type
	>;

	template<typename Str> requires tc::safely_convertible_to<Str&&, tc::span<tc::range_value_t<Str> const>>
	constexpr auto lit(Str&& str) noexcept {
		static_assert(!tc::instance<Str, std::basic_string>, "tc::lit won't own string data. Use tc::as_lvalue or x3::lit.");
		return literal_string_type<tc::range_value_t<Str>>(std::forward<Str>(str));
	}
}

namespace boost::spirit::x3 {
	template< typename Char >
	struct get_info<tc::literal_string_type<Char>> {
		using result_type = std::basic_string<char>/*external interface*/;
		result_type operator()(tc::literal_string_type<Char> const& p) const& noexcept {
			return tc_modified(result_type(), tc::append(_, p.str));
		}
	};
}

namespace tc {
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
		template<typename T>
		struct value_restrictive_parser final : x3::char_parser<value_restrictive_parser<T>> {
			using attribute_type = T;
			static bool const has_attribute = true;

			template<typename U>
			bool test(U const u, tc::unused /*context*/) const& noexcept {
				return parse_match(u);
			}

			template<typename U>
			bool parse_match(U const u) const& noexcept {
				return T(T::c_tFirst) <= u && u <= T(T::c_tLast); // We reuse the SFINAE from the comparison operators of value_restrictive.
			}
		};
	}
	template<typename T, T tFirst, T tLast>
	inline constexpr auto one<value_restrictive<T, tFirst, tLast>> = no_adl::value_restrictive_parser<value_restrictive<T, tFirst, tLast>>();

	inline constexpr auto asciidigit = tc::one<tc::char_asciidigit>;
	inline constexpr auto asciilower = tc::one<tc::char_asciilower>;
	inline constexpr auto asciiupper = tc::one<tc::char_asciiupper>;

	namespace no_adl {
		struct asciilit_impl final: x3::parser<asciilit_impl> {
			static bool const has_attribute = false;
			using attribute_type = x3::unused_type;
			explicit constexpr asciilit_impl(tc::span<char const> str) noexcept: m_str(tc_move(str)) {
				_ASSERTE(tc::all_of(m_str, [](char const ch) constexpr noexcept { return '\0'<=ch && ch<='\x7f'; }));
			}

			template<typename Iterator, typename Context, typename Attribute>
			bool parse(Iterator& first, Iterator const& last, Context const& context, x3::unused_type, Attribute& attr) const& {
				x3::skip_over(first, last, context);
				if(auto const first_=tc::starts_with<tc::return_border_or_null>(tc::make_iterator_range(first, last), m_str, [](auto const chInput, char const chLit) noexcept {
					return chInput == tc::explicit_cast<decltype(chInput)>(chLit);
				})) {
					first=first_;
					return true;
				}
				return false;
			}
		private:
			tc::span<char const> m_str;
		};
	}

	template<std::size_t N>
	constexpr auto asciilit(char const (&str)[N]) noexcept {
		return tc::no_adl::asciilit_impl(str);
	}

	namespace no_adl {
		struct blank final: x3::char_parser<blank> {
			using attribute_type = x3::unused_type;
			static bool const has_attribute = false;

			template <typename CharType, typename Context>
			bool test(CharType const& ch, Context&) const& noexcept {
				// in C locale, it is:
				// ' '	(0x20) space (SPC)
				// '\t'	(0x09) horizontal tab (TAB)
				// TODO: support unicode?
				return tc::char_ascii(' ')==ch || tc::char_ascii('\t')==ch;
			}
		};

		struct space final: x3::char_parser<space> {
			using attribute_type = x3::unused_type;
			static bool const has_attribute = false;

			template <typename CharType, typename Context>
			bool test(CharType const& ch, Context&) const& noexcept {
				// in C locale, it is:
				// ' '	(0x20) space (SPC)
				// '\t'	(0x09) horizontal tab (TAB)
				// '\n'	(0x0a) newline (LF)
				// '\v'	(0x0b) vertical tab (VT)
				// '\f'	(0x0c) feed (FF)
				// '\r'	(0x0d) carriage return (CR)
				// TODO: support unicode?
				return tc::char_ascii(' ')==ch || (tc::char_ascii('\t')<=ch && ch<=tc::char_ascii('\r'));
			}
		};
	}

	inline constexpr auto blank = no_adl::blank{};
	inline constexpr auto space = no_adl::space{};

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

}

namespace tc {
	inline constexpr auto asciixdigit = asciidigit | tc::one<tc::value_restrictive<char, 'A', 'F'>> | tc::one<tc::value_restrictive<char, 'a', 'f'>>;

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
}
