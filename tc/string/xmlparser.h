
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "parserbase.h"
#include "spirit.h"
#include "../container/container.h"
#include "../container/cont_assign.h"
#include "../range/join_framed_adaptor.h"
#include "../range/join_framed_adaptor.h"

namespace tc::xml {
	namespace no_adl {
		struct parse_error final {};
	}
	using no_adl::parse_error;
	
	namespace no_adl {
		template<tc::decayed Func>
		struct [[nodiscard]] simple_error_handler {
			simple_error_handler()=default;
			constexpr explicit simple_error_handler(Func func) noexcept
				: m_func(tc_move(func))
			{}
			void semantic_error(auto const& strInput, tc::unused /*itch*/, auto&&...) const& MAYTHROW {
				m_func(strInput);
			}
			void parse_warning(auto const& strInput, tc::unused /*itch*/, tc::unused /*strTag*/) const& MAYTHROW {
				m_func(strInput);
			}
			void parse_warning(auto const& strInput, tc::unused /*itch*/, tc::unused /*strTag*/, tc::unused /*strAttributeName*/) const& MAYTHROW {
				m_func(strInput);
			}
			void parse_error(auto const& strInput, tc::unused /*itch*/, tc::unused /*strTag*/) const& MAYTHROW {
				m_func(strInput);
			}
			void parse_error(auto const& strInput, tc::unused /*itch*/, tc::unused /*strTag*/, tc::unused /*strAttributeName*/) const& MAYTHROW {
				m_func(strInput);
			}
			void end_unexpected(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void end_expected(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void root_expected(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void name_expected(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void invalid_namespace_prefix(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void attribute_name_expected(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void child_expected(auto const& strInput, tc::unused /*itch*/, tc::unused /*strName*/) const& MAYTHROW {
				m_func(strInput);
			}
			void char_expected(auto const& strInput, tc::unused /*itch*/, tc::unused /*ch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void characters_expected(auto const& strInput, tc::unused /*itch*/, tc::unused /*strTag*/) const& MAYTHROW {
				m_func(strInput);
			}
			void characters_unexpected(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void attribute_expected(auto const& strInput, tc::unused /*itch*/, tc::unused /*strTag*/, tc::unused /*strAttributeName*/) const& MAYTHROW {
				m_func(strInput);
			}
			void element_end_expected(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void quotation_marks_expected(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}
			void unsupported_declaration_found(auto const& strInput, tc::unused /*itch*/) const& MAYTHROW {
				m_func(strInput);
			}

		private:
			Func m_func;
		};

		template< typename Rng >
		struct [[nodiscard]] decode_adaptor
			: tc::index_range_adaptor<
				decode_adaptor<Rng>,
				Rng, tc::index_range_adaptor_flags::inherit_begin_end
			>
		{
		private:
			using this_type = decode_adaptor;
			using base_ = typename decode_adaptor::index_range_adaptor;
			using char_type=tc::range_value_t<Rng>;

		public:
			using typename base_::tc_index;

			void SkipOverSemicolon(tc_index& idx) const& MAYTHROW {
				auto& base=this->base_range();
				for(;;) {
					tc::increment_index(base, idx);
					if(tc::at_end_index(base, idx)) break;
					if(tc::char_ascii(';')==tc::dereference_index(base, idx)) {
						tc::increment_index(base, idx);
						break;
					}
				}
			}

			STATIC_FINAL(increment_index)(tc_index& idx) const& MAYTHROW -> void {
				auto& base=this->base_range();
				auto t=tc::dereference_index(base, idx); // we return characters, which we can always return by value
				if(tc::char_ascii('&')==t) {
					SkipOverSemicolon(idx);
				} else {
					tc::increment_index(base, idx);
				}
			}

			template<bool bIncrement>
			char_type ProcessEscaped(tc_index& idx) const& MAYTHROW {
				auto& base=this->base_range();
				tc::increment_index(base, idx);
				if(!tc::at_end_index(base, idx)) {
					switch(tc::dereference_index(base, idx)) {
					case tc::explicit_cast<char_type>('a'):
						tc::increment_index(base, idx);
						if(!tc::at_end_index(base, idx)) {
							switch(tc::dereference_index(base, idx)) {
							case tc::explicit_cast<char_type>('m'): // amp
								if constexpr(bIncrement) SkipOverSemicolon(idx);
								return tc::char_ascii('&');
							case tc::explicit_cast<char_type>('p'): // apos
								if constexpr(bIncrement) SkipOverSemicolon(idx);
								return tc::char_ascii('\'');
							case tc::explicit_cast<char_type>(';'):
								if constexpr(bIncrement) tc::increment_index(base, idx);
								break;
							default:
								if constexpr(bIncrement) SkipOverSemicolon(idx);
								break;
							}
						}
						break;
					case tc::explicit_cast<char_type>('l'): // lt
						if constexpr(bIncrement) SkipOverSemicolon(idx);
						return tc::char_ascii('<');
					case tc::explicit_cast<char_type>('g'): // gt
						if constexpr(bIncrement) SkipOverSemicolon(idx);
						return tc::char_ascii('>');
					case tc::explicit_cast<char_type>('q'): // quot
						if constexpr(bIncrement) SkipOverSemicolon(idx);
						return tc::char_ascii('"');
					case tc::explicit_cast<char_type>('#'): // numeric character reference
						tc::increment_index(base, idx);
						if(!tc::at_end_index(base, idx)) {
							auto const NumericCharacterReference=[&](auto N) MAYTHROW -> char_type {
								int n;
								auto str=tc::drop(base, idx);
								if(tc::parse_consume( str, x3::uint_parser<decltype(n), decltype(N)::value>(), n )) { // MAYTHROW
									idx=tc::begin_index(str);
									if(!tc::at_end_index(base, idx)) {
										if(tc::char_ascii(';')==tc::dereference_index(base, idx)) {
											if constexpr(bIncrement) tc::increment_index(base, idx);
											return static_cast<char_type>(VERIFYPRED(n, tc::char_in_range<char_type>(_)));
										} else {
											if constexpr(bIncrement) SkipOverSemicolon(idx);
										}
									}
								} else if constexpr(bIncrement) {
									if(tc::char_ascii(';')==tc::dereference_index(base, idx)) {
										tc::increment_index(base, idx);
									} else {
										SkipOverSemicolon(idx);
									}
								}
								return tc::char_ascii('&'); // replacement character would be nicer, but has multiple code units, which we do not yet support
							};
							if(tc::char_ascii('x')==tc::dereference_index(base, idx)) {
								tc::increment_index(base, idx);
								if(!tc::at_end_index(base, idx)) {
									return NumericCharacterReference(tc::constant<16>());
								}
							} else {
								return NumericCharacterReference(tc::constant<10>());
							}
						}
						break;
					case tc::explicit_cast<char_type>(';'):
						if constexpr(bIncrement) tc::increment_index(base, idx);
						break;
					default:
						if constexpr(bIncrement) SkipOverSemicolon(idx);
						break;
					}
				}
				return tc::char_ascii('&'); // replacement character would be nicer, but has multiple code units, which we do not yet support
			};

			STATIC_FINAL(dereference_index)(tc_index const& idx) const& MAYTHROW {
				auto& base=this->base_range();
				auto t=tc::dereference_index(base, idx); // we return characters, which we can always return by value
				if(tc::char_ascii('&')==t) {
					return ProcessEscaped<false>(tc::as_lvalue(tc::decay_copy(idx)));
				} else {
					return t;
				}
			}

		public:
			using base_::base_;

			constexpr decode_adaptor() = default;

			static decltype(auto) element_base_index(tc_index const& idx) noexcept {
				return idx;
			}
			static decltype(auto) element_base_index(tc_index&& idx) noexcept {
				return tc_move(idx);
			}

			template<typename Sink>
			auto operator()(Sink sink) const& MAYTHROW -> tc::common_type_t<
				decltype(tc::continue_if_not_break(sink, tc::dereference_index(this->base_range(), std::declval<tc_index const&>()))),
				tc::constant<tc::continue_>
			> {
				auto& base=this->base_range();
				auto idx=tc::begin_index(base);
				for(;;) {
					if(tc::at_end_index(base, idx)) return tc::constant<tc::continue_>();
					if(tc::char_ascii('&')!=tc::dereference_index(base, idx)) {
						auto idxBegin=idx;
						do {
							tc::increment_index(base, idx);
							if(tc::at_end_index(base, idx)) {
								tc_return_if_break( tc::for_each(tc::slice(base, idxBegin, idx), sink) );
								return tc::constant<tc::continue_>();
							}
						} while(tc::char_ascii('&')!=tc::dereference_index(base, idx));
						tc_return_if_break( tc::for_each(tc::slice(base, idxBegin, idx), sink) );
					}
					tc_yield(sink, ProcessEscaped<true>(idx));
				}
			}
		};

		template<typename WithString>
		struct with_iterator_range /* not final */ {
			WithString m_withstring;

			template<tc::range_with_iterators String>
			auto chunk(String&& str) const& MAYTHROW {
				m_withstring(tc_move_if_owned(str)); // MAYTHROW
				return tc::constant<tc::continue_>();
			}
		};
		template<typename WithString>
		with_iterator_range(WithString) -> with_iterator_range<WithString>; // CTAD not working for aggregates (clang)
	}

	template<typename Rng>
	constexpr auto decode(Rng&& rng)
		return_ctor_noexcept( no_adl::decode_adaptor<Rng>, (aggregate_tag, std::forward<Rng>(rng)) )

	template<typename String>
	auto split_qualified_name(String const& str) noexcept {
		tc_auto_cref(itch, tc::find_unique<tc::return_border_after_or_begin>(str, tc::explicit_cast<tc::range_value_t<String>>(':')));
		return std::make_pair(
			tc::begin(str)==itch ? tc::take(str, itch) : tc::take(str, tc_modified(itch, --_)),
			tc::drop(str, itch)
		);
	}

	namespace no_adl {
		template< typename T, typename = void >
		struct has_discard_until : std::false_type { };

		// specialization recognizes types that do have a nested ::type member:
		template< typename T >
		struct has_discard_until<T, std::void_t<decltype(&std::declval<T>().discard_until)> > : std::true_type {};

#pragma push_macro("case_whitespace")
#define case_whitespace case tc::explicit_cast<char_type>('\t'): case tc::explicit_cast<char_type>('\n'): case tc::explicit_cast<char_type>('\r'): case tc::explicit_cast<char_type>(' ')

		TC_DEFINE_ENUM(EXmlEntity, exmlentity, (START)(EMPTYELEMENT)(EMPTYELEMENTCLOSING)(OPENINGTAG)(CLOSINGTAG)(CHARACTERS)(CDATA));

		template<typename Char>
		struct namespace_info : tc::noncopyable {
		protected:
			using SetNamespace = tc::unordered_set<tc::string<Char>>;
			SetNamespace m_setstrns;
			
		public:
			// Namespace is a nullable type
			// Passing a nullptr as a namespace to any parser method implies
			// that the matched element has no namespace, i.e., it belongs to the default namespace
			// and the default namespace has no value because either
			// - there is no xmlns="..." declaration in scope
			// - or there is a xmlns="" declaration in scope
			//
			// Using a namespace prefix t in the scope of a declaration xmlns:t="" is explicitly forbidden
			// and causes an invalid_namespace_prefix parser error.
			// https://www.w3.org/TR/2006/REC-xml-names11-20060816/#nsc-NSDeclared
			using Namespace = tc::string<Char> const*; // pointer is stable, iterator is not
			
			template<typename Rng>
			Namespace register_namespace(Rng const& strURI) & noexcept {
				// An empty string is a valid URI but has special semantics in the XML namespace 1.1 spec.
				// It undefines a previously defined namespace prefix for its scope.
				// https://www.w3.org/TR/2006/REC-xml-names11-20060816/#scoping
				_ASSERT(!tc::empty(strURI));
				return std::addressof(*tc::cont_try_emplace(m_setstrns, tc::make_str(decode(strURI))).first);
			}
		};

		template<typename String, typename ErrorHandler>
		struct [[nodiscard]] parser : parser_base<String, ErrorHandler>, namespace_info<tc::range_value_t<String>> {
		private:
			using base_ = parser_base<String, ErrorHandler>;
		public:
			using typename base_::char_type;

			using SetNamespace=typename namespace_info<char_type>::SetNamespace;
			using Namespace=typename namespace_info<char_type>::Namespace;
			
			explicit parser(String&& strInput, ErrorHandler errorhandler) MAYTHROW
				: base_(tc_move_if_owned(strInput), tc_move(errorhandler))
				, m_strMain(tc::slice(this->input(), this->m_itchInput, this->m_itchInput))
				, m_bConsumed(false)
			{
				Next(); // MAYTHROW
				SkipWhitespaceCharacters(); // MAYTHROW
				switch_no_default(m_exmlentity) {
				case exmlentityCLOSINGTAG:
					this->template error_at<tc_mem_fn(.root_expected)>(m_itchEntityBegin); // MAYTHROW
				case exmlentityEMPTYELEMENT:
				case exmlentityOPENINGTAG:
					; // root element
				}
			}

			using base_::expect_end;

			[[nodiscard]] auto characters() & MAYTHROW {
				EnsureFresh(); // MAYTHROW
				switch(m_exmlentity) {
				case exmlentityCDATA:
				case exmlentityCHARACTERS:
					m_bConsumed=true;
					break;
				default:
					m_bConsumed=false;
					break;
				}
				return tc::generator_range_output<char_type>([this](auto sink) MAYTHROW -> tc::common_type_t<
					decltype(tc::for_each(m_strMain, tc_move(sink))),
					decltype(tc::for_each(xml::decode(m_strMain), tc_move(sink))),
					tc::constant<tc::continue_>
				> {
					switch(m_exmlentity) {
					case exmlentityCDATA:
						return tc::for_each(m_strMain, tc_move(sink)); // MAYTHROW
					case exmlentityCHARACTERS:
						return tc::for_each(xml::decode(m_strMain), tc_move(sink)); // MAYTHROW
					default:
						return tc::constant<tc::continue_>();
					}
				});
			}

			[[nodiscard]] auto parse_characters(auto const& parser) & MAYTHROW {
				decltype(parser(tc::make_empty_range<char_type>())) ot;
				WithCharacters([&](auto const& strCharacters) MAYTHROW {
					if (!tc::empty(strCharacters)) {
						if (ot = parser(strCharacters); !ot) {
							this->template warning_at<tc_mem_fn(.parse_warning)>(m_itchEntityBegin, m_strMain); // MAYTHROW
						}
					}
				}); // MAYTHROW
				return ot;
			}

			[[nodiscard]] auto expect_parse_characters(auto const& parser) & MAYTHROW {
				tc::decay_t<decltype(*parser(tc::make_empty_range<char_type>()))> t;
				WithCharacters([&](auto const& strCharacters) MAYTHROW {
					if (tc::empty(strCharacters)) {
						this->template error_at<tc_mem_fn(.characters_expected)>(m_itchEntityBegin, m_strMain); // MAYTHROW
					}
					if (auto ot = parser(strCharacters); !ot) {
						this->template error_at<tc_mem_fn(.parse_error)>(m_itchEntityBegin, m_strMain); // MAYTHROW
					} else {
						t = *ot;
					}
				}); // MAYTHROW
				return t;
			}

			template<typename T>
			void assign_expect_parse_characters(T& t, auto const& parser) & MAYTHROW {
				t = expect_parse_characters(parser); // MAYTHROW
			}

			template<typename T, template<typename> typename Parser>
			void assign_expect_parse_characters(T& t, Parser<tc::deduce_tag> const&) & MAYTHROW {
				t = expect_parse_characters(Parser<T>()); // MAYTHROW
			}

			bool child(Namespace ons, auto const& strName) & MAYTHROW {
				EnsureFreshSkipWhitespaceCharacters(); // MAYTHROW
				switch_no_default(m_exmlentity) {
				case exmlentityEMPTYELEMENT:
				case exmlentityOPENINGTAG:
					{
						auto const paironsstr = expanded_name(); // MAYTHROW
						if(ons==paironsstr.first && tc::equal(strName, paironsstr.second)) {
							m_bConsumed=true;
							return true;
						}
					}
					[[fallthrough]];
				case exmlentityEMPTYELEMENTCLOSING:
				case exmlentityCLOSINGTAG:
					m_bConsumed=false;
					return false;
				}
			}

			bool child(auto const& strName) & MAYTHROW {
				return child(/*ons*/ nullptr, strName); // MAYTHROW
			}

			// For <mso:control id="..."> return the range 'mso:control'
			auto qualified_tag_name() const& noexcept {
				_ASSERTANYOF(m_exmlentity, (exmlentityEMPTYELEMENT)(exmlentityEMPTYELEMENTCLOSING)(exmlentityOPENINGTAG)(exmlentityCLOSINGTAG));
				return m_strMain;
			}

			// For <mso:control id="..."> return the pair ['mso', 'control']
			// as a std::pair of subranges.
			// The namespace prefix may be an empty range.
			auto prefix_and_tag_name() const& noexcept {
				return split_qualified_name(qualified_tag_name());
			}

			// For <mso:control id="..."> return a std::pair<Namespace, String>
			// with the namespace for prefix 'mso' and local name 'control'.
			// Namespace may be nullptr if the current tag has no namespace.
			// https://www.w3.org/TR/2006/REC-xml-names11-20060816/#concepts
			auto expanded_name() & MAYTHROW {
				auto const pairstrstr = prefix_and_tag_name();
				auto const ons = namespace_for_prefix(pairstrstr.first);
				
				// Namespace prefixes must be declared
				// https://www.w3.org/TR/2006/REC-xml-names11-20060816/#nsc-NSDeclared
				if(!ons && !tc::empty(pairstrstr.first)) {
					this->template error_at<tc_mem_fn(.invalid_namespace_prefix)>(m_itchEntityBegin); // MAYTHROW
				}
				
				return std::make_pair(ons, pairstrstr.second);
			}

			bool child() & MAYTHROW {
				EnsureFreshSkipWhitespaceCharacters(); // MAYTHROW
				switch_no_default(m_exmlentity) {
				case exmlentityEMPTYELEMENT:
				case exmlentityOPENINGTAG:
					m_bConsumed=true;
					return true;
				case exmlentityEMPTYELEMENTCLOSING:
				case exmlentityCLOSINGTAG:
					m_bConsumed=false;
					return false;
				}
			}

			void expect_child(Namespace ons, auto const& strName) & MAYTHROW {
				if (!child(ons, strName)) {
					this->template error_at<tc_mem_fn(.child_expected)>(m_itchEntityBegin, strName); // MAYTHROW
				}
			}

			void expect_child(auto const& strName) & MAYTHROW {
				expect_child(/*ons*/ nullptr, strName); // MAYTHROW
			}

			bool skip_child(Namespace ons, auto const& strName) & MAYTHROW {
				if (child(ons, strName)) { // MAYTHROW
					skip_rest_of_element(); // MAYTHROW
					return true;
				}
				return false;
			}

			bool skip_child(auto const& strName) & MAYTHROW {
				return skip_child(/*ons*/ nullptr, strName); // MAYTHROW
			}

			void expect_skip_child(Namespace ons, auto const& strName) & MAYTHROW {
				if (!skip_child(ons, strName)) {
					this->template error_at<tc_mem_fn(.child_expected)>(m_itchEntityBegin, strName); // MAYTHROW
				}
			}

			void expect_skip_child(auto const& strName) & MAYTHROW {
				expect_skip_child(/*ons*/ nullptr, strName); // MAYTHROW
			}

			void skip_rest_of_element() & MAYTHROW {
				EnsureFresh(); // MAYTHROW
				m_bConsumed=true;
				if(exmlentityEMPTYELEMENTCLOSING!=m_exmlentity) {
					int nOpenElements=1;
					for(;;) {
						switch(m_exmlentity) {
						case exmlentityOPENINGTAG:
							++nOpenElements;
							break;
						case exmlentityCLOSINGTAG:
							--nOpenElements;
							if(0==nOpenElements) return;
							break;
						default: ;
						}
						Next(); // MAYTHROW
					}
				}
			}
			
			void skip_child_or_characters() & MAYTHROW {
				EnsureFresh(); // MAYTHROW
				m_bConsumed=false;
				switch_no_default(m_exmlentity) {
				case exmlentityEMPTYELEMENT:
				case exmlentityCDATA:
				case exmlentityCHARACTERS:
					Next(); // MAYTHROW
					break;
				case exmlentityOPENINGTAG:
					Next(); // MAYTHROW
					skip_rest_of_element(); // MAYTHROW
					break;
				case exmlentityEMPTYELEMENTCLOSING:
				case exmlentityCLOSINGTAG:
					;
				}
			}

			bool element_end() & MAYTHROW {
				EnsureFreshSkipWhitespaceCharacters(); // MAYTHROW
				switch_no_default(m_exmlentity) {
				case exmlentityEMPTYELEMENTCLOSING:
				case exmlentityCLOSINGTAG:
					m_bConsumed=true;
					return true;
				case exmlentityEMPTYELEMENT:
				case exmlentityOPENINGTAG:
					m_bConsumed=false;
					return false;
				}
			}

			void expect_element_end() & MAYTHROW {
				if (!element_end()) {
					this->template error_at<tc_mem_fn(.element_end_expected)>(m_itchEntityBegin); // MAYTHROW
				}
			}

			[[nodiscard]] auto attribute(auto const& strName) & noexcept {
				// TODO: Attributes can have qualified names **but** the default namespace never applies to unqualified attributes:
				// An attribute with unqualified name always has no namespace value, not the default namespace.
				// See https://www.w3.org/TR/2006/REC-xml-names11-20060816/#scoping-defaulting
				_ASSERTANYOF(m_exmlentity, (exmlentityEMPTYELEMENT)(exmlentityOPENINGTAG));
				return tc::and_then(
					tc::find_first_if<tc::return_element_or_null>(m_vecpairstrAttributes, [&](auto const& pairstrstr) noexcept {
						return tc::equal(pairstrstr.first, strName);
					}),
					[&](auto const& pairstrstr) noexcept {
						return std::optional(xml::decode(pairstrstr.second));
					}
				);
			}

			[[nodiscard]] auto attributes() const& noexcept {
				return tc::transform(
					m_vecpairstrAttributes,
					[&](auto const& pairstrstr) noexcept {
						// FIXME: Support attribute without value
						return std::make_pair(pairstrstr.first, xml::decode(pairstrstr.second));
					}
				);
			}

			[[nodiscard]] auto expect_attribute(auto& strName) & MAYTHROW {
				auto ostr = attribute(strName);
				if (!ostr) {
					this->template error_at<tc_mem_fn(.attribute_expected)>(m_itchEntityBegin, m_strMain, strName); // MAYTHROW
				}
				return *tc_move(ostr);
			}

			[[nodiscard]] auto parse_attribute(auto const& parser, auto const& strName) & MAYTHROW {
				return tc::and_then(attribute(strName), [&](auto const& strAttribute) MAYTHROW {
					auto ot = parser(strAttribute);
					if (!ot) {
						this->template warning_at<tc_mem_fn(.parse_warning)>(m_itchEntityBegin, m_strMain, strName); // MAYTHROW
					}
					return ot;
				});
			}

			[[nodiscard]] auto expect_parse_attribute(auto const& parser, auto const& strName) & MAYTHROW {
				auto ot = parser(expect_attribute(strName)); // MAYTHROW
				if (!ot) {
					this->template error_at<tc_mem_fn(.parse_error)>(m_itchEntityBegin, m_strMain, strName); // MAYTHROW
				}
				return *tc_move(ot);
			}

			template<typename T, template<typename> typename Parser>
			void assign_expect_parse_attribute(T& t, Parser<tc::deduce_tag> const&, auto const& strName) & MAYTHROW {
				t = expect_parse_attribute(Parser<T>(), strName); // MAYTHROW
			}

			template<typename Str>
			Namespace /* may be nullptr */ namespace_for_prefix(Str const& strNsPrefix) const& noexcept {
				if(auto const opairstrns = tc::find_last_if<tc::return_element_or_null>(
					m_stkpairstrns,
					[&](auto const& pairstrns) noexcept {
						return c_nsStackSeparator!=pairstrns.second
							&& tc::equal(pairstrns.first, strNsPrefix);
					}
				)) {
					return opairstrns->second;
				} else {
					return nullptr;
				}
			}
			
		protected:
			tc::make_subrange_result_t< String const& > m_strMain;
			EXmlEntity m_exmlentity=exmlentitySTART;
			bool m_bConsumed;
				
#ifdef _MSC_VER
			static constexpr Namespace c_nsStackSeparator = std::addressof(tc_as_constexpr(tc::string<char_type>{}));
#else
# if defined(__clang__)
			static_assert(__clang_major__ == 14, "Is this workaround needed in Xcode 15?");
# endif
			static tc::string<char_type> const c_strDummy;
			static Namespace const c_nsStackSeparator;
#endif
			tc::vector<std::pair<tc::string< char_type >, Namespace>> m_stkpairstrns; // must not store sub_range in case input string is discarded

		private:
			tc::iterator_t<String const> m_itchEntityBegin;
			tc::vector<std::pair<tc::make_subrange_result_t< String const& >, tc::make_subrange_result_t< String const& >>> m_vecpairstrAttributes;
			
			void WithCharacters(auto func) & MAYTHROW {
				IF_TC_CHECKS(bool bOnce = false;)
				tc::for_each(
					characters(), // MAYTHROW
					no_adl::with_iterator_range{
						[&](auto const& strCharacters) MAYTHROW {
							_ASSERT(tc::change(bOnce, true));
							func(strCharacters);
						}
					}
				);
			}

			void SkipUntil(char_type const ch) & MAYTHROW {
				for(;;) {
					this->expect_not_end(); // MAYTHROW
					if(ch==*this->m_itchInput) break;
					++this->m_itchInput;
				}
			}

			void SkipOverGreaterThan() & MAYTHROW {
				SkipUntil(tc::char_ascii('>')); // MAYTHROW
				++this->m_itchInput;
			}

			void SkipOver(char_type const ch1, char_type const ch2) & MAYTHROW {
				bool bHave1=false;
				this->expect_not_end(); // MAYTHROW
				for(;;) {
					bHave1 = ch1==*this->m_itchInput;
					++this->m_itchInput;
					this->expect_not_end(); // MAYTHROW
					if(ch2==*this->m_itchInput && bHave1) break;
				}
				++this->m_itchInput;
			}

			void Next() & MAYTHROW {
			restart:
				if constexpr (has_discard_until<decltype((*this->m_strInput))>::value) {
					this->m_strInput->discard_until(tc::iterator2index<String>(this->m_itchInput));
				}

				this->expect_not_end(); // MAYTHROW
				m_itchEntityBegin=this->m_itchInput;
				switch(m_exmlentity) {
				case exmlentityCLOSINGTAG:
				case exmlentityEMPTYELEMENTCLOSING:
					tc::take_inplace(
						m_stkpairstrns,
						tc::find_last_if<tc::return_element>(m_stkpairstrns, [&](auto const& pairstrns) noexcept {
							return c_nsStackSeparator==pairstrns.second;
						})
					);
					break;
				default: ;
				};

				if(tc::char_ascii('<')==*this->m_itchInput) {
					++this->m_itchInput;
					this->expect_not_end(); // MAYTHROW
					switch(*this->m_itchInput) {
					case tc::explicit_cast<char_type>('>'):
						this->template error<tc_mem_fn(.name_expected)>(); // MAYTHROW
					case tc::explicit_cast<char_type>('?'): // processing instruction or prolog
						++this->m_itchInput;
						SkipOver(tc::char_ascii('?'), tc::char_ascii('>'));
						goto restart;
					case tc::explicit_cast<char_type>('/'): // closing tag
					{
						++this->m_itchInput;
						auto const itchBegin=this->m_itchInput;
						SkipOverGreaterThan();
						m_exmlentity=exmlentityCLOSINGTAG;
						m_strMain=tc::slice(this->input(), itchBegin, tc_modified(this->m_itchInput, --_));
						return;
					}
					case tc::explicit_cast<char_type>('!'):
						++this->m_itchInput;
						this->expect_not_end(); // MAYTHROW
						switch(*this->m_itchInput) {
						case tc::explicit_cast<char_type>('-'): // comment
							++this->m_itchInput;
							this->expect_literal("-"_tc);
							SkipOver(tc::char_ascii('-'), tc::char_ascii('-'));
							this->expect_literal(">"_tc);
							goto restart;
						case tc::explicit_cast<char_type>('['): // CDATA
							++this->m_itchInput;
							this->expect_literal("CDATA["_tc);

							{
								auto const itchBegin=this->m_itchInput;
								int nClosing;
								decltype(this->m_itchInput) aitchEnd[2];
								goto start;

								for(;;) {
									do {
										++this->m_itchInput;
									start:
										this->expect_not_end(); // MAYTHROW
									} while(tc::char_ascii(']')!=*this->m_itchInput);

									nClosing=0;
									do {
										aitchEnd[nClosing&1]=this->m_itchInput;
										++this->m_itchInput;
										this->expect_not_end(); // MAYTHROW
										++nClosing;
									} while(tc::char_ascii(']')==*this->m_itchInput);

									if(2<=nClosing && tc::char_ascii('>')==*this->m_itchInput) break;
								}
								m_strMain=tc::slice(this->input(), itchBegin, aitchEnd[nClosing&1]);
							}
							++this->m_itchInput;
							m_exmlentity=exmlentityCDATA;
							return;
						default: // DOCTYPE, ENTITY, etc.
							this->template error_at<tc_mem_fn(.unsupported_declaration_found)>(m_itchEntityBegin); // MAYTHROW
						}
					default: // opening tag or empty element
						tc::cont_assign(m_vecpairstrAttributes);
						tc::cont_emplace_back(m_stkpairstrns, tc::take(this->input(), tc::begin(this->input())), c_nsStackSeparator);

						auto itchBegin=this->m_itchInput;
						++this->m_itchInput;
						for(;;) {
							this->expect_not_end(); // MAYTHROW
							switch(*this->m_itchInput) {
							default:
								++this->m_itchInput;
								break;
							case tc::explicit_cast<char_type>('>'):
								m_strMain=tc::slice(this->input(), itchBegin, this->m_itchInput);
								++this->m_itchInput;
								m_exmlentity=exmlentityOPENINGTAG;
								return;
							case tc::explicit_cast<char_type>('/'):
								m_strMain=tc::slice(this->input(), itchBegin, this->m_itchInput);
								++this->m_itchInput;
								this->expect_literal(">"_tc);
								m_exmlentity=exmlentityEMPTYELEMENT;
								return;
							case_whitespace:
								m_strMain=tc::slice(this->input(), itchBegin, this->m_itchInput);
								++this->m_itchInput;
								for(;;) {
									this->skip_whitespace();
									switch(*this->m_itchInput) {
									case tc::explicit_cast<char_type>('>'):
										++this->m_itchInput;
										m_exmlentity=exmlentityOPENINGTAG;
										return;
									case tc::explicit_cast<char_type>('/'):
										++this->m_itchInput;
										this->expect_literal(">"_tc);
										m_exmlentity=exmlentityEMPTYELEMENT;
										return;
									default: ;
									}

									auto strAttribute=[&]() MAYTHROW {
										auto itchBegin = this->m_itchInput;
										for(;;) {
											switch(auto const ch = *this->m_itchInput) {
											case tc::explicit_cast<char_type>('='):
												{
													auto strAttribute=tc::slice(this->input(), itchBegin, this->m_itchInput);
													if (tc::empty(strAttribute)) {
														this->template error<tc_mem_fn(.attribute_name_expected)>(); // MAYTHROW
													}
													++this->m_itchInput;
													return strAttribute;
												}
											case_whitespace:
												{
													auto strAttribute=tc::slice(this->input(), itchBegin, this->m_itchInput);
													++this->m_itchInput;
													this->skip_whitespace();
													this->expect_literal("="_tc);
													return strAttribute;
												}
											default:
												++this->m_itchInput;
												this->expect_not_end(); // MAYTHROW
											}
										}
									}();
									this->skip_whitespace();

									switch(auto const ch=*this->m_itchInput) {
									default:
										this->template error<tc_mem_fn(.quotation_marks_expected)>(); // MAYTHROW
									case tc::explicit_cast<char_type>('"'): case tc::explicit_cast<char_type>('\''):
										++this->m_itchInput;
										itchBegin=this->m_itchInput;
										SkipUntil(ch);

										auto strValue = tc::slice(this->input(), itchBegin, this->m_itchInput);
										if(auto ostrPrefix = [&]() MAYTHROW -> std::optional<tc::make_subrange_result_t<String const&>> {
											if(auto ostr = tc::starts_with<tc::return_drop_or_none>(strAttribute, tc_ascii("xmlns"))) {
												if(tc::starts_with<tc::return_bool>(*ostr, tc_ascii(":"))) {
													tc::drop_first_inplace(*ostr);
													if(tc::empty(*ostr)) {
														this->template error_at<tc_mem_fn(.parse_error)>(m_itchEntityBegin, m_strMain, strAttribute); // MAYTHROW
													}
													return ostr;
												} else if(tc::empty(*ostr)) {
													return ostr;
												}
											}
											return std::nullopt;
										}()) {
											tc::cont_emplace_back(
												m_stkpairstrns,
												tc_move_always(*ostrPrefix),
												tc::empty(strValue) ? nullptr : this->register_namespace(strValue)
											);
										} else {
											tc::cont_emplace_back(m_vecpairstrAttributes, tc_move(strAttribute), strValue);
										}
										++this->m_itchInput;
									}
								}
							}
						}
					}
				} else {
					++this->m_itchInput;
					SkipUntil(tc::char_ascii('<'));
					m_strMain=tc::slice(this->input(), m_itchEntityBegin, this->m_itchInput);
					m_exmlentity=exmlentityCHARACTERS;
				}
			}

			void SkipWhitespaceCharacters() & MAYTHROW {
				for(;;) {
					switch(m_exmlentity) {
					default:
						return;
					case exmlentityCDATA:
						this->template error_at<tc_mem_fn(.unsupported_declaration_found)>(m_itchEntityBegin); // MAYTHROW
					case exmlentityCHARACTERS:
						tc_auto_cref(itEnd, tc::end(m_strMain));
						for (auto it = tc::begin(m_strMain); itEnd != it; ++it) {
							switch(*it) {
							default:
								this->template error_at<tc_mem_fn(.characters_unexpected)>(it); // MAYTHROW
							case_whitespace:
								;
							}
						};
						Next(); // MAYTHROW
					}
				}
			}

			void EnsureFresh() & MAYTHROW {
				if(m_bConsumed) {
					if(exmlentityEMPTYELEMENT==m_exmlentity) {
						m_exmlentity=exmlentityEMPTYELEMENTCLOSING;
					} else {
						Next();
					}
				}

			}

			void EnsureFreshSkipWhitespaceCharacters() & MAYTHROW {
				EnsureFresh(); // MAYTHROW
				SkipWhitespaceCharacters(); // MAYTHROW
			}
		};
#pragma pop_macro("case_whitespace")

#ifndef _MSC_VER
		template<typename String, typename ErrorHandler> tc::string<typename parser<String, ErrorHandler>::char_type> const parser<String, ErrorHandler>::c_strDummy{};
		template<typename String, typename ErrorHandler> typename parser<String, ErrorHandler>::Namespace const parser<String, ErrorHandler>::c_nsStackSeparator = std::addressof(c_strDummy);
#endif
	}
	using no_adl::simple_error_handler;
	using no_adl::namespace_info;
	using no_adl::parser;

	inline auto constexpr throw_parse_error = simple_error_handler([](tc::unused /*strInput*/) THROW(tc::xml::parse_error) {
		throw tc::xml::parse_error();
	});

	template<typename String, typename ErrorHandler, typename... Args>
	[[nodiscard]] auto make_parser(String&& str, ErrorHandler&& errorhandler, Args&&... args) MAYTHROW {
		return no_adl::parser<String, tc::decay_t<ErrorHandler>>(tc_move_if_owned(str), tc_move_if_owned(errorhandler), tc_move_if_owned(args)...); // MAYTHROW
	}
	
	void for_each_descendant( auto& parser, auto funcOpen, auto funcClose) MAYTHROW {
		int nOpenElements = 1;
		for(;;) {
			if(parser.child()) { // MAYTHROW
				++nOpenElements;
				funcOpen();
			} else {
				--nOpenElements;
				if(0==nOpenElements) break;

				parser.expect_element_end(); // MAYTHROW
				funcClose();
			}
		}
	}

	namespace no_adl {
		inline struct {
			[[nodiscard]] std::optional<bool> operator()(auto const& str) const& noexcept {
				if (tc::equal(str, tc_ascii("0")) || tc::equal(str, tc_ascii("false"))) {
					return false;
				} else if (tc::equal(str, tc_ascii("1")) || tc::equal(str, tc_ascii("true"))) {
					return true;
				} else {
					return std::nullopt;
				}
			}
		} constexpr boolean;

		template<typename T>
		struct IntegerParser final {
			[[nodiscard]] std::optional<T> operator()(auto const& str) const& noexcept requires tc::actual_integer<T> && std::is_signed<T>::value {
				return tc::x3parser(x3::int_parser<T>())(str);
			}
		};
		template<typename T=tc::deduce_tag>
		constexpr IntegerParser<T> integer = IntegerParser<T>();

		template<typename T>
		struct NonNegativeIntegerParser final {
			[[nodiscard]] std::optional<T> operator()(auto const& str) const& noexcept requires tc::actual_integer<T> {
				return tc::x3parser(x3::uint_parser<T>())(str);
			}
		};
		template<typename T=tc::deduce_tag>
		constexpr NonNegativeIntegerParser<T> nonNegativeInteger = NonNegativeIntegerParser<T>();

		template<typename T>
		struct RealParser final {
			[[nodiscard]] std::optional<T> operator()(auto const& str) const& noexcept requires std::floating_point<T> {
				return tc::x3parser(x3::real_parser<T>())(str);
			}
		};
		template<typename T=tc::deduce_tag>
		constexpr RealParser<T> real = RealParser<T>();
	}
	using no_adl::boolean;
	using no_adl::integer;
	using no_adl::nonNegativeInteger;
	using no_adl::real;
}

namespace tc {
	template<typename Rng>
	constexpr auto enable_stable_index_on_move<xml::no_adl::decode_adaptor<Rng>> = tc::stable_index_on_move<Rng>;
}

