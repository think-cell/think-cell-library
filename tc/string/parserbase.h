
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/reference_or_value.h"
#include "../range/meta.h"
#include "char.h"
#include "ascii.h"

namespace tc {
	namespace no_adl {
		// Naming convention for parser functions:
		// * `optional<T> foo()`: Try to consume `foo` and return its value. If `T == void`, return `bool`.
		// * `T expect_foo()`: Consume `foo` and return its value. It is an error if `foo` isn't next.
		// * `void skip_foo()`: Consume `foo`. It is an error if `foo` isn't next.

		template<typename String, typename ErrorHandler>
		struct parser_base {
			using char_type=tc::range_value_t<String>;
			using iterator = tc::iterator_t<String const>;

			decltype(auto) input() const& noexcept {
				return *m_strInput;
			}

			explicit operator bool() const& noexcept {
				return !m_bError;
			}

			iterator position() const& noexcept {
				_ASSERTDEBUG(*this);
				return m_itchInput;
			}

			template<typename... Args>
			[[noreturn]] void semantic_error(Args&&... args) & MAYTHROW {
				this->error<tc_mem_fn(.semantic_error)>(tc_move_if_owned(args)...);
			}

		protected:
			explicit parser_base(String&& strInput, ErrorHandler errorhandler) noexcept
				: m_strInput(tc::aggregate_tag, tc_move_if_owned(strInput))
				, m_itchInput(tc::begin(*m_strInput))
				, m_end(tc::end(*m_strInput))
				, m_errorhandler(tc_move(errorhandler))
				, m_bError(false)
			{}
			parser_base(const parser_base&) = default;
			parser_base& operator=(const parser_base&) = default;
			~parser_base() = default;

		protected:
			template<auto memfn, typename... Args>
			void warning_at(iterator it, Args&&... args) & MAYTHROW {
				_ASSERTDEBUG(*this);
				memfn(m_errorhandler, *m_strInput, it, tc_move_if_owned(args)...); // MAYTHROW
			}
			template<auto memfn, typename... Args>
			void warning(Args&&... args) & MAYTHROW {
				warning_at<memfn>(this->m_itchInput, tc_move_if_owned(args)...); // MAYTHROW
			}

			template<auto memfn, typename... Args>
			[[noreturn]] void error_at(iterator it, Args&&... args) & MAYTHROW {
				_ASSERTDEBUG(*this);
				m_bError = true;
				memfn(m_errorhandler, *m_strInput, it, tc_move_if_owned(args)...); // MAYTHROW
				_ASSERTNORETURNFALSE;
			}
			template<auto memfn, typename... Args>
			[[noreturn]] void error(Args&&... args) & MAYTHROW {
				error_at<memfn>(this->m_itchInput, tc_move_if_owned(args)...); // MAYTHROW
			}

		protected:
			char_type unchecked_peek() & noexcept {
				_ASSERTDEBUG(*this);
				_ASSERTDEBUG(!end());
				return *m_itchInput;
			}

			void unchecked_increment() & noexcept {
				_ASSERTDEBUG(*this);
				_ASSERTDEBUG(!end());
				++m_itchInput;
			}

			tc::sentinel_t<String const> end_position() const& noexcept {
				return m_end;
			}

			void set_position(iterator pos) & noexcept {
				_ASSERTDEBUG(*this);
				if constexpr (std::random_access_iterator<iterator> && tc::common_range<String>) {
					// If pos is outside the range, the comparison is UB, but so is calling set_position() in the first place.
					// So we assert it to hope that it causes a nicer error.	
					_ASSERT(tc::begin(input()) <= pos && pos <= m_end);
				}
				m_itchInput = pos;
			}

		protected:
			bool end() const& noexcept {
				_ASSERTDEBUG(*this);
				return m_itchInput == m_end;
			}
			void expect_end() & MAYTHROW {
				_ASSERTDEBUG(*this);
				if (m_itchInput != m_end) {
					error<tc_mem_fn(.end_expected)>(); // MAYTHROW
				}
			}
			void expect_not_end() & MAYTHROW {
				_ASSERTDEBUG(*this);
				if (m_itchInput == m_end) {
					error<tc_mem_fn(.end_unexpected)>(); // MAYTHROW
				}
			}

			[[nodiscard]] std::optional<char_type> any_char() & noexcept {
				_ASSERTDEBUG(*this);
				if (m_itchInput == m_end) {
					return std::nullopt;
				}
				return *m_itchInput++;
			}
			[[nodiscard]] char_type expect_any_char() & MAYTHROW {
				_ASSERTDEBUG(*this);
				expect_not_end();
				return *m_itchInput++;
			}

			template <auto C>
			[[nodiscard]] bool literal(tc::literal_range<tc::char_ascii, C>) & noexcept {
				_ASSERTDEBUG(*this);
				if (m_itchInput == m_end || C != *m_itchInput) {
					return false;
				}
				++m_itchInput;
				return true;
			}
			template <auto C>
			void expect_literal(tc::literal_range<tc::char_ascii, C>) & MAYTHROW {
				_ASSERTDEBUG(*this);
				expect_not_end();
				if (C != *m_itchInput) {
					error<tc_mem_fn(.char_expected)>(tc::char_ascii(C)); // MAYTHROW
				}
				++m_itchInput;
			}

			template <auto ... Cs>
			void expect_literal(tc::literal_range<tc::char_ascii, Cs...>) & MAYTHROW {
				(expect_literal(tc::literal_range<tc::char_ascii, Cs>()), ...);
			}

			template <typename Fn>
			[[nodiscard]] std::optional<char_type> char_class(Fn const predicate) & noexcept {
				_ASSERTDEBUG(*this);
				if (m_itchInput == m_end || !predicate(*m_itchInput)) {
					return std::nullopt;
				}

				return *m_itchInput++;
			}
			template <auto ... Cs>
			[[nodiscard]] std::optional<char_type> one_of(tc::literal_range<tc::char_ascii, Cs...>) & noexcept {
				_ASSERTDEBUG(*this);
				if (m_itchInput == m_end) return std::nullopt;

				auto ch = *m_itchInput;
				if (((Cs == ch) || ...)) {
					++m_itchInput;
					return ch;
				}

				return std::nullopt;
			}

		protected:
			void skip_whitespace() & MAYTHROW {
				_ASSERTDEBUG(*this);
				for (;;) {
					expect_not_end(); // MAYTHROW
					switch(*m_itchInput) {
					default:
						return;
					case '\t':
					case '\n':
					case '\r':
					case ' ':
						++m_itchInput;
						break;
					}
				}
			}
			void skip_whitespace_maybe_end() & noexcept {
				_ASSERTDEBUG(*this);
				while (m_itchInput != m_end) {
					switch(*m_itchInput) {
					default:
						return;
					case '\t':
					case '\n':
					case '\r':
					case ' ':
						++m_itchInput;
						break;
					}
				}
			}

			// Precondition: We've already consumed one character and want to skip the rest of the code point.
			void skip_utf8_code_point(char_type const ch0) & MAYTHROW requires (sizeof(char_type) == 1) {
				_ASSERTDEBUG(*this);
				auto const n0 = tc::to_underlying(ch0); // MAYTHROW
				if(0 == (n0 & 0x80)) { // ASCII
					return;
				}

				auto const n1 = tc::to_underlying(expect_any_char()); // MAYTHROW
				switch( (n0 >> 4) & 0x7 ) {
					case 0b100:
					case 0b101:
						{	// ([\xC2-\xDF][\x80-\xBF])
							if( (n0 < 0xC2) | (0x80 != (0xC0 & n1)) ) goto InvalidEncoding; // No short-circuit-evaluation
							break;
						}

					case 0b110:
						{	// ((([\xE0][\xA0-\xBF])|([\xE1-\xEC\xEE-\xEF][\x80-\xBF])|([\xED][\x80-\x9F]))[\x80-\xBF])
							auto const n2 = tc::to_underlying(expect_any_char()); // MAYTHROW
							if( (0x80 != (0xC0 & (n1 | n2))) | // not continuation bytes
								(0xE0 == n0) & (n1 < 0xA0) | // non-shortest form
								(0xED == n0) & (0xA0 <= n1) // range 0xd800-0xdfff
							) goto InvalidEncoding; // No short-circuit-evaluation
							break;
						}

					case 0b111:
						{	// ((([\xF0][\x90-\xBF])|([\xF1-\xF3][\x80-\xBF])|([\xF4][\x80-\x8F]))[\x80-\xBF][\x80-\xBF])
							auto const n2 = tc::to_underlying(expect_any_char()); // MAYTHROW
							auto const n3 = tc::to_underlying(expect_any_char()); // MAYTHROW
							if( (n0 < 0xF5) & // value less than U+10FFFF
								((0xF0 != n0) | (0x90 <= n1)) & // non-shortest form
								((0xF4 != n0) | (n1 < 0x90)) & // value less than U+10FFFF
								(0x80 == (0xC0 & (n1 | n2 | n3))) // continuation bytes
							) {  // No short-circuit-evaluation
								break;
							}
						}
						[[fallthrough]];

					default:
					InvalidEncoding:
						this->template error<tc_mem_fn(.invalid_encoding)>(); // MAYTHROW
				}
			}

		protected: // TODO: refactor XML parser
			tc::reference_or_value<String> m_strInput;
			iterator m_itchInput;
			tc::sentinel_t<String const> m_end; // cached for performance
			ErrorHandler m_errorhandler;
			bool m_bError;
		};
	}
	using no_adl::parser_base;
}
