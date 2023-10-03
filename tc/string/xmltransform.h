
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "char.h"
#include "xmlparser.h"
#include "../range/conditional_range.h"

namespace tc::xml {
	[[nodiscard]] auto format_attributes(auto&& rngattr) noexcept {
		return tc::join_with_separator(
			tc_ascii(" "),
			tc::transform(
				tc_move_if_owned(rngattr),
				[&](auto const& strKey, auto const& strValue) noexcept {
					return tc::concat(strKey, tc_ascii("=\""), strValue, tc_ascii("\""));
				}
			) 
		);
	}

	[[nodiscard]] auto opening_element(auto&& strPrefix, auto&& strElement, auto&& strAttributes) noexcept {
		auto const bEmptyPrefix = tc::empty(strPrefix);
		return tc::concat(
			tc_ascii("<"),
			tc_move_if_owned(strPrefix),
			tc_conditional_range(!bEmptyPrefix, tc_ascii(":")),
			tc_move_if_owned(strElement),
			tc_move_if_owned(strAttributes),
			tc_ascii(">")
		);
	}

	[[nodiscard]] auto opening_element(auto&& strPrefix, auto&& strElement) noexcept {
		return opening_element(tc_move_if_owned(strPrefix), tc_move_if_owned(strElement), tc::empty_range());
	}

	[[nodiscard]] auto opening_element(auto&& strElement) noexcept {
		return opening_element(tc::empty_range(), tc_move_if_owned(strElement), tc::empty_range());
	}

	[[nodiscard]] auto closing_element(auto&& strPrefix, auto&& strElement) noexcept {
		auto const bEmptyPrefix = tc::empty(strPrefix);
		return tc::concat(
			tc_ascii("</"),
			tc_move_if_owned(strPrefix),
			tc_conditional_range(!bEmptyPrefix, tc_ascii(":")),
			tc_move_if_owned(strElement),
			tc_ascii(">")
		);
	}

	[[nodiscard]] auto closing_element(auto&& strElement) noexcept {
		return closing_element(tc::empty_range(), tc_move_if_owned(strElement));
	}

	[[nodiscard]] auto empty_element(auto&& strPrefix, auto&& strElement, auto&& strAttributes) noexcept {
		auto const bEmptyPrefix = tc::empty(strPrefix);
		return tc::concat(
			tc_ascii("<"),
			tc_move_if_owned(strPrefix),
			tc_conditional_range(!bEmptyPrefix, tc_ascii(":")),
			tc_move_if_owned(strElement),
			tc_move_if_owned(strAttributes),
			tc_ascii("/>")
		);
	}

	namespace no_adl {
		template<typename String, typename ErrorHandler, typename Output>
		struct [[nodiscard]] transform : private parser<String, ErrorHandler>, tc::inside_unwinding {
		private:
			using base_ = parser<String, ErrorHandler>;
		public:
			using typename base_::char_type;
			
			using SetNamespace=typename base_::SetNamespace;
			using Namespace=typename base_::Namespace;

			static_assert(!has_discard_until<decltype((*transform<String, ErrorHandler, Output>::m_strInput))>::value);

			explicit transform(String&& strInput, ErrorHandler errorhandler, Output& output) MAYTHROW
				: base_(std::forward<String>(strInput), tc_move(errorhandler)), m_output(output)
				, m_itchOut(tc::begin(*(this->m_strInput)))
			{}
			
			~transform() {
				if(!inside_stack_unwinding()) {
					if(OutputEmptyElementCloseTag()) { // MAYTHROW
						// Skip the original empty element tag
						_ASSERT(exmlentityEMPTYELEMENT==this->m_exmlentity || exmlentityEMPTYELEMENTCLOSING==this->m_exmlentity);
						_ASSERT(exmlentityEMPTYELEMENTCLOSING==this->m_exmlentity || this->m_bConsumed);
						if(exmlentityEMPTYELEMENT==this->m_exmlentity || !this->m_bConsumed) { 
							base_::expect_element_end(); // MAYTHROW
						}
						_ASSERTEQUAL(this->m_exmlentity, exmlentityEMPTYELEMENTCLOSING);
						_ASSERT(this->m_bConsumed);
						m_itchOut = this->m_itchInput;
					}
					tc::append(
						m_output,
						tc::slice(*this->m_strInput, m_itchOut, tc::end(*this->m_strInput))
					); // MAYTHROW
				}
			}
			
			template<typename ...Attributes>
			void modify_element_attributes(Attributes&& ...attr) & MAYTHROW {
				_ASSERT(this->m_bConsumed);
				_ASSERTANYOF(this->m_exmlentity, (exmlentityEMPTYELEMENT)(exmlentityOPENINGTAG));
				OutputBeforeElement(); // MAYTHROW

				tc::append(
					m_output,
					tc_ascii("<"), this->qualified_tag_name(),
					tc_ascii(" "),
					tc_move_if_owned(attr)...,
					tc_conditional_range(
						exmlentityEMPTYELEMENT==this->m_exmlentity,
						tc_ascii("/>"),
						tc_ascii(">")
					)
				); // MAYTHROW
				m_itchOut = this->m_itchInput;
			}

			[[nodiscard]] Output& insert_before() & noexcept {
				if(exmlentityEMPTYELEMENTCLOSING==this->m_exmlentity) {
					SplitEmptyElement(); // MAYTHROW
				} else {
					m_itchOut = OutputBeforeElement(); // MAYTHROW
				}
				return m_output;
			}

		private:
			// just call tc::append(transform, ...) 
			[[nodiscard]] Output& insert_after() & MAYTHROW {
				_ASSERT(this->m_bConsumed);
				_ASSERTANYOF(this->m_exmlentity, (exmlentityEMPTYELEMENT)(exmlentityEMPTYELEMENTCLOSING)(exmlentityOPENINGTAG)(exmlentityCLOSINGTAG));
				if(exmlentityEMPTYELEMENT==this->m_exmlentity) {
					SplitEmptyElement(); // MAYTHROW
				} else {
					tc::append(m_output, tc::slice(*this->m_strInput, m_itchOut, this->m_itchInput)); // MAYTHROW
					m_itchOut = this->m_itchInput;
				}
				return m_output;
			}

		public:
			friend auto appender_impl(transform& self) noexcept {
				return tc::appender(
					self.m_bConsumed
					? self.insert_after()
					: self.insert_before()
				);
			}

			[[nodiscard]] auto insert_elements_before(auto&& strOpen, auto&& strClose) MAYTHROW {
				// If inserting before a closing tag: What do you need the RAII return value for?
				_ASSERTANYOF(this->m_exmlentity, (exmlentityEMPTYELEMENT)(exmlentityOPENINGTAG));
				tc::append(insert_before(), tc_move_if_owned(strOpen)); // MAYTHROW
				return tc::make_append_on_dtor(*this, tc_move_if_owned(strClose));
			}

			// insert_elements_after allows parsing and outputting further elements after
			// appending strOpen and before appending strClose
			[[nodiscard]] auto insert_elements_after(auto&& strOpen, auto&& strClose) MAYTHROW {
				tc::append(insert_after(), tc_move_if_owned(strOpen)); // MAYTHROW
				return tc::make_append_on_dtor(*this, tc_move_if_owned(strClose));
			}

		private:
			[[nodiscard]] auto closing_element_on_dtor(auto&& strPrefix, auto&& strTag) noexcept {
				return tc::make_append_on_dtor(
					*this,
					tc::xml::closing_element(tc_move_if_owned(strPrefix),tc_move_if_owned(strTag))
				);
			}

		public:
			template<typename Str>
			[[nodiscard]] auto match_or_insert_child(Namespace ons, Str&& strTag) & MAYTHROW
				-> std::optional<decltype(
				closing_element_on_dtor(std::declval<tc::string<char_type>&&>(), std::forward<Str>(strTag))
			)> {
				auto strPrefix = prefix_for_namespace(ons);
				if(this->child(ons, strTag)) { // MAYTHROW
					return std::nullopt;
				} else {
					_ASSERT(!this->m_bConsumed);
					tc::append(
						insert_before(),
						tc::xml::opening_element(strPrefix, strTag)
					);
					return closing_element_on_dtor(tc_move(strPrefix), tc_move_if_owned(strTag));
				}
			}

			[[nodiscard]] auto match_or_insert_child(auto&& strTag) & MAYTHROW {
				return match_or_insert_child(/*namespace*/ nullptr, tc_move_if_owned(strTag));
			}

			[[nodiscard]] auto drop_element_parse_children() & MAYTHROW {
				_ASSERT(this->m_bConsumed);
				_ASSERTANYOF(this->m_exmlentity, (exmlentityEMPTYELEMENT)(exmlentityOPENINGTAG));
				OutputBeforeElement(); // MAYTHROW
				m_itchOut = this->m_itchInput;

				struct drop_closing_tag final : tc::noncopyable, tc::inside_unwinding {
					explicit drop_closing_tag(transform& transform) noexcept : m_transform(transform) {}
					drop_closing_tag(drop_closing_tag&&)=default;
					~drop_closing_tag() MAYTHROW {
						if(!inside_stack_unwinding()) {
							m_transform.expect_element_end(); // MAYTHROW
							if(exmlentityCLOSINGTAG==m_transform.m_exmlentity) {
								m_transform.OutputBeforeElement(); // MAYTHROW
								m_transform.m_itchOut = m_transform.m_itchInput;
							} else {
								_ASSERTEQUAL(m_transform.m_exmlentity, exmlentityEMPTYELEMENTCLOSING);
								_ASSERTEQUAL(m_transform.m_itchOut, m_transform.m_itchInput);
							}
						}
					}
				private:
					transform& m_transform;
				};
				return drop_closing_tag(*this);
			}

			void drop_element() & MAYTHROW {
				_ASSERT(this->m_bConsumed);
				_ASSERTANYOF(this->m_exmlentity, (exmlentityEMPTYELEMENT)(exmlentityOPENINGTAG));
				OutputBeforeElement(); // MAYTHROW
				this->skip_rest_of_element(); // MAYTHROW
				m_itchOut = this->m_itchInput;
			}
			
			// Methods inherited unchanged from tc::xml::parser
			using base_::register_namespace;
			using base_::namespace_for_prefix;
			
			tc::string<char_type> prefix_for_namespace(Namespace ons) const& noexcept {
				if(ons) {
					if(auto const opairstrns = tc::find_last_if<tc::return_element_or_null>(
						this->m_stkpairstrns,
						[&](auto const& pairstrns) noexcept {
							return ons==pairstrns.second;
						}
					)) {
						return opairstrns->first;
					} else {
						_ASSERTFALSE; // FIXME: Throw?
						return tc::string<char_type>();
					}
				} else {
					if(auto const opairstrns = tc::find_last_if<tc::return_element_or_null>(
						this->m_stkpairstrns,
						[&](auto const& pairstrns) noexcept {
							return tc::empty(pairstrns.first)
								&& pairstrns.second
								&& this->c_nsStackSeparator!=pairstrns.second;
						}
					)) {
						// Default namespace has been redefined
						_ASSERTFALSE; // FIXME: Throw? 
						return tc::string<char_type>();
					} else {
						return tc::string<char_type>();
					}
				}
			}

			using base_::semantic_error;

			using base_::attribute;
			using base_::attributes;
			using base_::expect_attribute;

			using base_::expanded_name;
			using base_::qualified_tag_name;

			using base_::child;
			using base_::expect_child;

			using base_::skip_child;
			using base_::expect_skip_child;

			// Output the injected close tag for an empty element when the empty element close tag is parsed
			void skip_rest_of_element() & MAYTHROW { OutputEmptyElementCloseTag(); base_::skip_rest_of_element(); }
			bool element_end() & MAYTHROW { OutputEmptyElementCloseTag(); return base_::element_end(); }
			void expect_element_end() & MAYTHROW { OutputEmptyElementCloseTag(); base_::expect_element_end(); }

		private:
			Output& m_output;
			static_assert(tc::appendable<tc::span<char_type const>, Output&>);
			
			decltype(tc::begin(*transform<String, ErrorHandler, Output>::m_strInput)) m_itchOut;
			tc::string<char_type> m_strCloseTag;

			auto OutputBeforeElement() & MAYTHROW {
				auto itch = tc::begin(this->m_strMain); // MAYTHROW
				switch_no_default(this->m_exmlentity) {
					case exmlentityCLOSINGTAG: // </
						--itch; // MAYTHROW
						[[fallthrough]];
					case exmlentityOPENINGTAG: // <
					case exmlentityEMPTYELEMENT:
					case exmlentityEMPTYELEMENTCLOSING:
						--itch; // MAYTHROW
				}
				tc::append(m_output, tc::slice(*this->m_strInput, m_itchOut, itch)); // MAYTHROW
				return itch;
			}

			void SplitEmptyElement() & MAYTHROW {
				_ASSERTANYOF(this->m_exmlentity, (exmlentityEMPTYELEMENT)(exmlentityEMPTYELEMENTCLOSING));
				if(tc::empty(m_strCloseTag)) {
					m_itchOut = OutputBeforeElement(); // MAYTHROW
					auto const str = tc::slice(*this->m_strInput, m_itchOut, this->m_itchInput);
					tc::append(
						m_output,
						tc::ends_with<tc::return_take>(str, tc_ascii("/>")),
						tc_ascii(">")
					); // MAYTHROW
					
					tc::cont_assign(m_strCloseTag, this->qualified_tag_name());
				} else {
					_ASSERT(tc::equal(m_strCloseTag, this->qualified_tag_name()));
				}
			}

			bool OutputEmptyElementCloseTag() & MAYTHROW {
				if(!tc::empty(m_strCloseTag)) {
					_ASSERTANYOF(this->m_exmlentity, (exmlentityEMPTYELEMENT)(exmlentityEMPTYELEMENTCLOSING));
					tc::append(m_output, tc_ascii("</"), m_strCloseTag, tc_ascii(">")); // MAYTHROW
					tc::cont_assign(m_strCloseTag);
					return true;
				} else {
					return false;
				}
			}
		};
	}
	using no_adl::transform;
	
	template<typename String, typename ErrorHandler, typename Output, typename... Args> requires tc::appendable<tc::span<tc::range_value_t<String>>, Output&>
	[[nodiscard]] auto make_transform(String&& str, ErrorHandler&& errorhandler, Output& output, Args&&... args) MAYTHROW {
		return no_adl::transform<String, tc::decay_t<ErrorHandler>, Output>(tc_move_if_owned(str), tc_move_if_owned(errorhandler), output, tc_move_if_owned(args)...); // MAYTHROW
	}
}
