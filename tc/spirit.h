
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "sub_range.h"
#include "explicit_cast.h"
#include "as_lvalue.h"
#include "find.h"
#include "ascii.h"
#include "append.h"
#include "tag_type.h"
#include <boost/version.hpp>

#ifndef __clang__
#pragma warning(push)
#pragma warning(disable:4127) // conditional expression is constant
#if BOOST_VERSION==105900
#pragma warning(disable:4244) // conversion from 'unsigned __int64' to 'double', possible loss of data, see ticket 11608
#endif
#pragma warning(disable:4459) // declaration hides global declaration
#else
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wsign-compare"
#endif
#include <boost/spirit/home/x3.hpp>
#include <boost/spirit/home/support/common_terminals.hpp>
#ifndef __clang__
#pragma warning(pop)
#else
#pragma clang diagnostic pop
#endif
#include <boost/fusion/adapted/std_tuple.hpp>

namespace x3 = boost::spirit::x3;
namespace tc {
	template< typename Rng, typename Expr, typename... Attr, std::enable_if_t< sizeof...(Attr)<2 >* = nullptr>
	bool parse( Rng const& rng, Expr const& expr, Attr&... attr ) MAYTHROW {
		try {
			return x3::parse( tc::begin(rng), tc::end(rng), expr, attr... ); // MAYTHROW
		} catch (x3::expectation_failure<tc::decay_t<decltype(tc::begin(rng))>> const&) {
			return false;
		}
	}
	template< typename Iterator, typename Expr, typename... Attr, std::enable_if_t< sizeof...(Attr)<2 >* = nullptr>
	bool parse_iterator( Iterator& itBegin, Iterator const& itEnd, Expr const& expr, Attr&... attr ) MAYTHROW {
		auto itBegin2=itBegin;
		try {
			if(x3::parse( itBegin, itEnd, expr, attr... )) return true; // MAYTHROW
		} catch (x3::expectation_failure<Iterator> const&) {}
		itBegin=itBegin2;
		return false;
	}
	template< typename Rng, typename Expr, typename... Attr, std::enable_if_t< sizeof...(Attr)<2 >* = nullptr>
	bool parse_consume( Rng & rng, Expr const& expr, Attr&... attr ) MAYTHROW {
		try {
			auto itBegin = tc::begin(rng);
			if(x3::parse( itBegin, tc::end(rng), expr, attr... )) { // MAYTHROW
				tc::drop_inplace( rng, itBegin );
				return true;
			}
		} catch (x3::expectation_failure<tc::decay_t<decltype(tc::begin(rng))>> const&) {}
		return false;
	}
	template< typename Rng, typename Expr, typename Skipper, typename... Attr, std::enable_if_t< sizeof...(Attr)<2 >* = nullptr>
	bool phrase_parse( Rng const& rng, Expr const& expr, Skipper const& skipper, Attr&... attr ) MAYTHROW {
		try {
			return x3::phrase_parse( tc::begin(rng), tc::end(rng), expr, skipper, attr... ); // MAYTHROW
		} catch (x3::expectation_failure<tc::decay_t<decltype(tc::begin(rng))>> const&) {
			return false;
		}
	}
	template< typename Rng, typename Expr, typename... Attr, std::enable_if_t< 2<=sizeof...(Attr) >* = nullptr>
	bool parse( Rng const& rng, Expr const& expr, Attr&... attr ) MAYTHROW {
		try {
			return x3::parse( tc::begin(rng), tc::end(rng), expr, tc::as_lvalue(std::tie(attr...)) ); // MAYTHROW
		} catch (x3::expectation_failure<tc::decay_t<decltype(tc::begin(rng))>> const&) {
			return false;
		}
	}
	template< typename Iterator, typename Expr, typename... Attr, std::enable_if_t< 2<=sizeof...(Attr) >* = nullptr>
	bool parse_iterator( Iterator& itBegin, Iterator const& itEnd, Expr const& expr, Attr&... attr ) MAYTHROW {
		auto itBegin2=itBegin;
		try {
			if(x3::parse( itBegin, itEnd, expr, tc::as_lvalue(std::tie(attr...)) )) return true; // MAYTHROW
		} catch (x3::expectation_failure<Iterator> const&) {}
		itBegin=itBegin2;
		return false;
	}
	template< typename Rng, typename Expr, typename... Attr, std::enable_if_t< 2<=sizeof...(Attr) >* = nullptr>
	bool parse_consume( Rng & rng, Expr const& expr, Attr&... attr ) MAYTHROW {
		try {
			auto itBegin = tc::begin(rng);
			if(x3::parse( itBegin, tc::end(rng), expr, tc::as_lvalue(std::tie(attr...)) )) { // MAYTHROW
				tc::drop_inplace( rng, itBegin );
				return true;
			}
		} catch (x3::expectation_failure<tc::decay_t<decltype(tc::begin(rng))>> const&) {}
		return false;
	}
	template< typename Rng, typename Expr, typename Skipper, typename... Attr, std::enable_if_t< 2<=sizeof...(Attr) >* = nullptr>
	bool phrase_parse( Rng const& rng, Expr const& expr, Skipper const& skipper, Attr&... attr ) MAYTHROW {
		try {
			return x3::phrase_parse( tc::begin(rng), tc::end(rng), expr, skipper, tc::as_lvalue(std::tie(attr...)) );  // MAYTHROW
		} catch (x3::expectation_failure<tc::decay_t<decltype(tc::begin(rng))>> const&) {
			return false;
		}
	}
}

namespace boost { namespace spirit { namespace traits {
	// prevent access violation when using ascii::alpha, ascii::digit and ascii::alnum on narrow char input containing non-ascii characters
	// http://boost.2283326.n4.nabble.com/BOOST-ASSERT-isascii-ch-is-triggered-by-char-classification-parsers-tp3475081p3475081.html

	template<>
	struct ischar<char, char_encoding::ascii, false> final {
		static bool call(char ch) noexcept {
			return char_encoding::ascii::isascii_(ch);
		}
	};
}}}

#ifdef __clang__

// add char16_t support to Spirit

namespace boost { namespace spirit { namespace x3 { namespace traits
	{
		///////////////////////////////////////////////////////////////////////////
		// Determine if T is a character type
		///////////////////////////////////////////////////////////////////////////
		template <>
		struct is_char<char16_t> : mpl::true_ {};

		///////////////////////////////////////////////////////////////////////////
		// Determine if T is a string
		///////////////////////////////////////////////////////////////////////////
		template <>
		struct is_string<char16_t const*> final : mpl::true_ {};

		template <>
		struct is_string<char16_t*> final : mpl::true_ {};

		template <std::size_t N>
		struct is_string<char16_t[N]> final : mpl::true_ {};

		template <std::size_t N>
		struct is_string<char16_t const[N]> final : mpl::true_ {};

		template <std::size_t N>
		struct is_string<char16_t(&)[N]> final : mpl::true_ {};

		template <std::size_t N>
		struct is_string<char16_t const(&)[N]> final : mpl::true_ {};

		///////////////////////////////////////////////////////////////////////////
		// Get the underlying char type of a string
		///////////////////////////////////////////////////////////////////////////
		template <>
		struct char_type_of<char16_t> final : mpl::identity<wchar_t> {};

		template <>
		struct char_type_of<char16_t const*> final : mpl::identity<wchar_t const> {};

		template <>
		struct char_type_of<char16_t*> final : mpl::identity<wchar_t> {};

		template <std::size_t N>
		struct char_type_of<char16_t[N]> final : mpl::identity<wchar_t> {};

		template <std::size_t N>
		struct char_type_of<char16_t const[N]> final : mpl::identity<wchar_t const> {};

		template <std::size_t N>
		struct char_type_of<char16_t(&)[N]> final : mpl::identity<wchar_t> {};

		template <std::size_t N>
		struct char_type_of<char16_t const(&)[N]> final : mpl::identity<wchar_t const> {};
	}}}}

namespace boost { namespace spirit { namespace char_encoding {
	///////////////////////////////////////////////////////////////////////////
	//  Test characters for specified conditions (using std wchar_t functions)
	///////////////////////////////////////////////////////////////////////////

	struct standard_16
	{
		using char_type = char16_t;

		template <typename Char>
		static typename std::char_traits<Char>::int_type
		to_int_type(Char ch) noexcept
		{
			return std::char_traits<Char>::to_int_type(ch);
		}

		template <typename Char>
		static Char
		to_char_type(typename std::char_traits<Char>::int_type ch) noexcept
		{
			return std::char_traits<Char>::to_char_type(ch);
		}

		static bool
		ischar(int ch) noexcept
		{
			// we have to watch out for sign extensions (casting is there to
			// silence certain compilers complaining about signed/unsigned
			// mismatch)
			return 0<=ch && ch<=0xffff;
		}

		static ::boost::uint32_t
		toucs4(int ch) noexcept
		{
			return ch;
		}
	};
}}}

namespace boost { namespace spirit { namespace traits
	{
		template <>
		struct char_encoding_from_char<char16_t> final
		: boost::mpl::identity<boost::spirit::char_encoding::standard_16>
		{};
	}}}


namespace boost { namespace spirit { namespace x3 {
	namespace standard_16 {
		using char_type = any_char<char_encoding::standard_16>;
        auto const char_ = char_type{};
	}
}}}

#endif

namespace tc {
	DEFINE_TAG_TYPE(unused_type)

	template<typename Char>
	constexpr auto char_ = tc::unused_type();

	template<>
	constexpr auto char_<char> = x3::any_char<typename boost::spirit::traits::char_encoding_from_char<char>::type>();

	template<>
	constexpr auto char_<wchar_t> = x3::any_char<typename boost::spirit::traits::char_encoding_from_char<wchar_t>::type>();

#ifdef __clang__
	template<>
	constexpr auto char_<char16_t> = x3::any_char<typename boost::spirit::traits::char_encoding_from_char<char16_t>::type>();
#endif

	template<typename Char>
	constexpr
	x3::literal_char<
		typename boost::spirit::traits::char_encoding_from_char<tc::decay_t<Char>>::type
	> single_char(Char ch) noexcept {
		return {ch};
	}

	namespace no_adl {
		template<typename Rng>
		struct char_set final: x3::char_parser<char_set<Rng>> {
			using attribute_type = tc::range_value_t<std::remove_reference_t<Rng>>;
			static bool const has_attribute = true;

			constexpr char_set(Rng rng) noexcept
				: m_rng(tc_move_if_owned(rng)) {}

			template <typename CharType, typename Context, std::enable_if_t<std::is_same<CharType, attribute_type>::value>* = nullptr>
			bool test(CharType const& ch, Context& context) const& noexcept {
				return tc::find_first_if<tc::return_bool>(m_rng, [&](auto const& chAllowed) noexcept {
					return 0==x3::get_case_compare<typename boost::spirit::traits::char_encoding_from_char<CharType>::type>(context)(ch, chAllowed);
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

	template<typename Char>
	x3::literal_char<
		typename boost::spirit::traits::char_encoding_from_char<tc::decay_t<Char>>::type,
		x3::unused_type
	> lit(Char ch) noexcept {
		return {ch};
	}

	template<typename Char>
	using literal_string_type = x3::literal_string<
		tc::ptr_range<Char const>,
		typename boost::spirit::traits::char_encoding_from_char<Char>::type,
		x3::unused_type
	>;

	template<typename RngChar>
	auto lit(RngChar&& str, std::enable_if_t<tc::is_safely_convertible<RngChar&&, tc::ptr_range<tc::range_value_t<RngChar> const>>::value>* = nullptr) noexcept {
		static_assert(!tc::is_instance<std::basic_string, RngChar>::value, "tc::lit won't own string data. Use tc::as_lvalue or x3::lit.");
		return literal_string_type<tc::range_value_t<RngChar>>(std::forward<RngChar>(str));
	}
}

namespace boost { namespace spirit { namespace x3 {
	template< typename Char >
	struct get_info<tc::literal_string_type<Char>> {
		using result_type = std::basic_string<char>;
		result_type operator()(tc::literal_string_type<Char> const& p) const& noexcept {
			return modified(result_type(), tc::append(_, p.str));
		}
	};
}}}

namespace tc {
	namespace attr_is_id_adl {
		struct attr_is_id;
		// tc::attr_is directive creates a rule_definition with an attribute type of T
		// it should be used to specify the attribute type of a sub-parser.
		//   auto const rule_def = x3::lit('-') >> tc::attr_is<std::string>[+x3::alpha];
		// it should NOT be used to specify the attribute type of another rule definition.
		//   Not recommended:
		//     x3::rule<struct xxx, std::string> const rule{""};
		//     auto const rule_def = tc::attr_is<std::string>[+x3::alpha];
		//     BOOST_SPIRIT_DEFINE(rule)
		//   Recommended (2nd line if attribute propagation is not automatic):
		//     auto const rule_def = rule = +x3::alpha;
		// it doesn't insert dummy values into the context

		// When we call the parse method of a x3::rule_definition, it will check if there is a specialization
		// of the parse_rule function on rule_definition's ID. Normally this specialization is provided by
		// BOOST_SPIRIT_DEFINE macro. Otherwise the parse method will inject a reference of rule_definition's
		// rhs parser (actual parser) into the context. So when the rule and rule_def are not connected by
		// BOOST_SPIRIT_DEFINE macro, for example when they are defined locally, the parse method of x3::rule can
		// still call the rule_def's parse by retrieving the injected rhs parser with its ID using the default
		// parse_rule function.
		//   x3::rule<struct xx, std::string> const rule{""};
		//   auto const rule_def = +x3::alpha | (tc::lit('[') >> rule >> tc::lit(']'));
		//   tc::parse(str, rule_def, attr); // rule and rule_def are not connected with BOOST_SPIRIT_DEFINE
		// tc::attr_is is a rule_definition, but it's never meant or able to be used it like this. So we don't
		// have to inject its rhs parser into the context. The injection may slow the parsing when it's recursive. 
		// BOOST_SPIRIT_DEFINE is for global rule(_def)s. So we need to declare the specialization of the parse_rule
		// function on attr_is_id. We don't have to implement it because it's only used in the decltype and is never
		// called (because x3::rule with attr_is_id is never exposed).
		template <typename Iterator, typename Context, typename Attribute, typename ActualAttribute>
		bool parse_rule(
		    x3::rule<attr_is_id, Attribute> const& rule_
		  , Iterator& first, Iterator const& last
		  , Context const& context, ActualAttribute& attr);
		// Attribute is the Attribute type the rule propagates
		// ActualAttribute is the Attribute variable type we give to tc::parse function
	}
	using attr_is_id_adl::attr_is_id;

	namespace no_adl {
		template<typename T>
		struct attr_is_type {
			template<typename Expr>
			auto operator[](Expr&& expr) const& noexcept {
				// returns a x3::rule_definition with attr_is_id, Attribute type of T and rhs parser of x3::as_parser(expr)
				return x3::rule<attr_is_id, T>{"attr_is"} = x3::as_parser(std::forward<Expr>(expr));
			}
		};
	}

	template<typename T>
	constexpr no_adl::attr_is_type<T> attr_is = {};

	namespace no_adl {
		template<typename Char>
		struct char_range_parser final: x3::char_parser<char_range_parser<Char>> {
			using attribute_type = Char;
			static bool const has_attribute = true;

			constexpr char_range_parser(Char chFirst, Char chLast) noexcept
				: m_chFirst(chFirst)
				, m_chLast(chLast)
			{}

			template<typename CharType, typename Context, std::enable_if_t<std::is_same<CharType, Char>::value>* = nullptr>
			bool test(CharType const& ch, Context& context) const& noexcept {
				auto_cref( compare, x3::get_case_compare<typename boost::spirit::traits::char_encoding_from_char<CharType>::type>(context) );
				return 0 <= compare(ch, m_chFirst) && compare(ch, m_chLast) <= 0;
			}
		private:
			Char m_chFirst;
			Char m_chLast; // inclusive
		};
	}

	template<typename Char>
	constexpr no_adl::char_range_parser<Char> char_range(Char chFirst, Char chLast) noexcept {
		return {chFirst, chLast};
	}
	template<typename Char>
	constexpr auto asciidigit = char_range(tc::explicit_cast<Char>('0'), tc::explicit_cast<Char>('9'));

	template<typename Char>
	constexpr auto asciilower = char_range(tc::explicit_cast<Char>('a'), tc::explicit_cast<Char>('z'));

	template<typename Char>
	constexpr auto asciiupper = char_range(tc::explicit_cast<Char>('A'), tc::explicit_cast<Char>('Z'));

#ifndef __clang__
	inline auto UnusedInlineFunctionToWorkaroundCompilerBug() noexcept {
		asciidigit<char>;
		asciidigit<wchar_t>;
		asciilower<char>;
		asciilower<wchar_t>;
		asciiupper<char>;
		asciiupper<wchar_t>;
	}
#endif

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
				return tc::explicit_cast<CharType>(' ')==ch || tc::explicit_cast<CharType>('\t')==ch;
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
				return tc::explicit_cast<CharType>(' ')==ch || (tc::explicit_cast<CharType>('\t')<=ch && ch<=tc::explicit_cast<CharType>('\r'));
			}
		};
	}

	constexpr auto blank = no_adl::blank{};
	constexpr auto space = no_adl::space{};
}

namespace boost { namespace spirit { namespace x3 {
	DEFINE_TAG_TYPE(ascii_no_case_tag)

	constexpr auto ascii_no_case_compare_ = ascii_no_case_tag{};

	template <typename Encoding>
	struct ascii_no_case_compare
	{
		template < template <typename> class basic_charset>
		typename Encoding::char_type
		in_set( typename Encoding::char_type const ch
		      , basic_charset<typename Encoding::char_type> const &set)
		{
			// case-insensitive set should be all uppercase
			return set.test(tc::toasciiupper(ch));
		}
		
		std::int32_t operator()(
			  typename Encoding::char_type const lc
			, typename Encoding::char_type const rc) const
		{
			return tc::toasciiupper(lc) - tc::toasciiupper(rc);
		}
		
		template <typename CharClassTag>
		CharClassTag get_char_class_tag(CharClassTag tag) const
		{
			return tag;
		}
		
		alpha_tag get_char_class_tag(lower_tag ) const
		{
			return {};
		}
		
		alpha_tag get_char_class_tag(upper_tag ) const
		{
			return {};
		}
	};

	template <typename Encoding>
	ascii_no_case_compare<Encoding> get_case_compare_impl(ascii_no_case_tag const&)
	{
		return {};
	}

	// propagate no_case information through the context
	template <typename Subject>
	struct ascii_no_case_directive : unary_parser<Subject, ascii_no_case_directive<Subject>>
	{
		typedef unary_parser<Subject, ascii_no_case_directive<Subject> > base_type;
		static bool const is_pass_through_unary = true;
		static bool const handles_container = Subject::handles_container;
		
		ascii_no_case_directive(Subject const& subject)
		  : base_type(subject) {}
		
		template <typename Iterator, typename Context
		  , typename RContext, typename Attribute>
		bool parse(Iterator& first, Iterator const& last
		  , Context const& context, RContext& rcontext, Attribute& attr) const
		{
			return this->subject.parse(
				first, last
			  , make_context<no_case_tag>(ascii_no_case_compare_, context)
			  , rcontext
			  , attr);
		}
	};

	struct ascii_no_case_gen
	{
		template <typename Subject>
		ascii_no_case_directive<typename extension::as_parser<Subject>::value_type>
		operator[](Subject const& subject) const
		{
			return { as_parser(subject) };
		}
	};
}}}

namespace tc {
    constexpr auto ascii_no_case = x3::ascii_no_case_gen{};

	template<typename Char, typename T>
	using symbols = x3::symbols_parser<typename boost::spirit::traits::char_encoding_from_char<tc::decay_t<Char>>::type, T>;

	namespace no_adl {
		struct quoted_symbols final: tc::symbols<char, char const> {
			quoted_symbols() noexcept {
				add ("\\a", '\a')("\\b", '\b')("\\f", '\f')("\\n", '\n')
					("\\r", '\r')("\\t", '\t')("\\v", '\v')
					("\\\\", '\\')("\\\'", '\'')("\\\"", '\"');
			}
		};
	}
	x3::rule<struct SIdNarrowQuotedSymbols, std::string> const narrow_quoted_symbols{"narrow_quoted_symbols"};
	auto const narrow_quoted_symbols_def = x3::rule<struct SIdNarrowQuotedSymbols,std::string>{} %=
		x3::confix("\"", "\"")[x3::no_skip[*(no_adl::quoted_symbols{} | tc::lit("\\x")[([](auto& ctx) noexcept {x3::_pass(ctx)=false;})] | (tc::char_<char> - '"'))]];
	BOOST_SPIRIT_DEFINE(narrow_quoted_symbols)

	namespace no_adl {
		struct move_attr_to_val_impl final {
			template<typename Context>
			void operator()(Context& ctx) noexcept {
				x3::_val(ctx)=tc_move_always(x3::_attr(ctx));
			}
		};
		template<typename T>
		struct move_attr_to_ext_val_impl final {
			move_attr_to_ext_val_impl(T& t) noexcept: m_t(t) {}
			template<typename Context>
			void operator()(Context& ctx) noexcept {
				m_t=tc_move_always(x3::_attr(ctx));
			}
		private:
			T& m_t;
		};
	}

	template<typename T>
	auto move_attr_to_val(T& t) noexcept {
		return no_adl::move_attr_to_ext_val_impl<T>(t);
	}

	constexpr auto move_attr_to_val() noexcept {
		return no_adl::move_attr_to_val_impl();
	}
}

///////////////////////////////
// x3::with_val

namespace boost { namespace spirit { namespace x3 {
	template<typename Subject, typename ID>
	struct with_val_directive : unary_parser<Subject, with_val_directive<Subject, ID>>
	{
		using base_type = unary_parser<Subject, with_val_directive<Subject, ID>>;
		static bool const is_pass_through_unary = true;
        static bool const handles_container = Subject::handles_container;
		using subject_type = Subject;

		with_val_directive(Subject const& subject): base_type(subject) {}

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
		with_val_directive<typename extension::as_parser<Subject>::value_type, ID>
		operator[](Subject const& subject) const
		{
			return { as_parser(subject) };
		}
	};

	template<typename ID>
	constexpr auto with_val = with_val_gen<ID>{};
}}}

///////////////////////////////
// x3::lazy

namespace boost { namespace spirit { namespace x3
{
	template <typename context_tag>
	struct lazy_parser : parser<lazy_parser<context_tag>>
	{
		using base_type = parser<lazy_parser<context_tag>>;
		       
		template <typename Iterator, typename Context, typename RContext, typename Attribute>
		bool parse(Iterator& first, Iterator const& last, Context const& context, RContext& rcontext, Attribute& attr) const
		{
			return x3::get<context_tag>(context).parse(first, last, context, rcontext, attr);
		}
	};

	template <typename context_tag>
	constexpr auto lazy = lazy_parser<context_tag>{};
}}}

namespace boost { namespace spirit { namespace x3 { namespace traits
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
}}}}
