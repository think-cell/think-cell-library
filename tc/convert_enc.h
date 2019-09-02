
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "casts.h"
#include "range_adaptor.h"

namespace tc {
	inline bool is_continuation_unit(char ch) noexcept {
		return 0x80 == (0xc0 & ch);
	}

	inline bool is_continuation_unit(tc::char16 ch) noexcept {
		return 0xdc00 <= ch && ch < 0xe000;
	}

	namespace convert_enc_impl {
		inline bool IsLeadingSurrogate( unsigned int n ) noexcept {
			return 0xd800 <= n && n < 0xdc00;
		}

		inline bool AddTrailingSurrogate( unsigned int& n, tc::char16 ch ) noexcept {
			if(!IsLeadingSurrogate(n)) return false;
			if(!tc::is_continuation_unit(ch)) return false;
			n <<= 10;
			n += ch;
			n += 0x10000u - (0xD800u << 10u) - 0xDC00u;
			return true;
		}

		char32_t const c_chReplacementCharacter = 0xfffd; // U+FFFD REPLACEMENT CHARACTER

		inline int Utf8Bytes (unsigned int n) {
			// We will replace invalid characters above 0x10ffff with c_chReplacementCharacter
			if (!VERIFYNOTIFY(n < 0x110000)) {
				n = tc::underlying_cast(c_chReplacementCharacter);
			}

			if (n < 0x80) {
				return 1;
			} else if (n < 0x800) {
				return 2;
			} else if (n < 0x10000) {
				return 3;
			} else {
				return 4;
			}
		}

		template <typename Dst, typename Rng, typename Src=tc::range_value_t<Rng>>
		struct SStringConversionRange;

		template <typename T, typename Rng>
		using enable_if_range_of_t = std::enable_if_t<std::is_same<T, tc::range_value_t<Rng>>::value, T>;

		// Lazily convert UTF-16 strings to UTF-32
		template<typename Rng>
		struct SStringConversionRange<char32_t, Rng, enable_if_range_of_t<tc::char16, Rng>>
		: tc::range_iterator_generator_from_index<
			SStringConversionRange<char32_t, Rng, tc::char16>,
			tc::index_t<std::remove_reference_t<Rng>>,
			typename boost::range_detail::demote_iterator_traversal_tag<
				boost::iterators::bidirectional_traversal_tag,
				traversal_t<Rng>
			>::type
		> {
			// Indexes pointing to the second UTF-16 code unit of a surrogate pair are invalid.
			// If the underlying range contains lone surrogates, we will convert them to REPLACEMENT CHARACTER
			using index = typename SStringConversionRange::index;

			explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
			: m_baserng(aggregate_tag, std::forward<Rng>(rng))
			{}
		private:
			using this_type = SStringConversionRange<char32_t, Rng, tc::char16>;
			reference_or_value< Rng > m_baserng;

		public:
			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return tc::begin_index(m_baserng);
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return tc::end_index(m_baserng);
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::at_end_index(*m_baserng,idx);
			}

			STATIC_FINAL(dereference_index)(index idx) const& noexcept -> char32_t {
				unsigned int n = tc::dereference_index(*m_baserng,idx);
				if (IsLeadingSurrogate(n)) {
					tc::increment_index(*m_baserng,idx);
					if (!(VERIFYNOTIFY(!tc::at_end_index(*m_baserng,idx)) && AddTrailingSurrogate(n, tc::dereference_index(*m_baserng,idx)) && n < 0x110000)) {
						return c_chReplacementCharacter;
					}
				} else if (!VERIFYNOTIFY(!tc::is_continuation_unit(static_cast<tc::char16>(n)))) {
					return c_chReplacementCharacter;
				}
				return static_cast<char32_t>(n);
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return tc::equal_index(*m_baserng,idxLhs, idxRhs);
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				_ASSERT(!tc::at_end_index(*m_baserng,idx));
				bool bLeadingSurrogate = IsLeadingSurrogate(tc::dereference_index(*m_baserng,idx));
				tc::increment_index(*m_baserng,idx);
				if (bLeadingSurrogate && VERIFYNOTIFY(!tc::at_end_index(*m_baserng,idx)) && VERIFYNOTIFY(tc::is_continuation_unit(tc::dereference_index(*m_baserng,idx)))) {
					tc::increment_index(*m_baserng,idx);
				}
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				tc::decrement_index(*m_baserng,idx);
				if (tc::is_continuation_unit(tc::dereference_index(*m_baserng,idx)) && !tc::equal_index(*m_baserng,idx, tc::begin_index(m_baserng))) {
					tc::decrement_index(*m_baserng,idx);
					if (!VERIFYNOTIFY(IsLeadingSurrogate(tc::dereference_index(*m_baserng,idx)))) {
						tc::increment_index(*m_baserng,idx);
					}
				}
			}

			auto border_base_index(index const& idx) const& noexcept {
				return idx;
			}

			constexpr decltype(auto) base_range() & noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() const& noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() && noexcept {
				return *std::move(m_baserng);
			}
			constexpr decltype(auto) base_range() const&& noexcept {
				return *std::move(m_baserng);
			}
		};
	
		// Lazily convert UTF-8 strings to UTF-32
		template<typename Rng>
		struct SStringConversionRange<char32_t, Rng, enable_if_range_of_t<char, Rng>>
		: tc::range_iterator_generator_from_index<
			SStringConversionRange<char32_t, Rng, char>,
			tc::index_t<std::remove_reference_t<Rng>>,
			typename boost::range_detail::demote_iterator_traversal_tag<
				boost::iterators::bidirectional_traversal_tag,
				traversal_t<Rng>
			>::type
		> {
			// Valid indexes are those which point to:
			// a) a single-byte (0xxxxxxxx) character,
			// b) the starting byte (11xxxxxx) of a possibly truncated character, or
			// c) the first byte in a run of continuation bytes (10xxxxxx).
			// This ensures that each run of illegally encoded characters is converted to at least one REPLACEMENT CHARACTER
			using index = typename SStringConversionRange::index;

			explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
			: m_baserng(aggregate_tag, std::forward<Rng>(rng))
			{}
		private:
			using this_type = SStringConversionRange<char32_t, Rng, char>;
			reference_or_value< Rng > m_baserng;

		public:
			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return tc::begin_index(m_baserng);
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return tc::end_index(m_baserng);
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::at_end_index(*m_baserng,idx);
			}

			STATIC_FINAL(dereference_index)(index idx) const& noexcept -> char32_t {
				unsigned int n;
				auto Continuation = [&](int i) noexcept {
					tc::increment_index(*m_baserng,idx);
					do {
						if (!VERIFYNOTIFY(!tc::at_end_index(*m_baserng,idx))) return false;
						auto const ch = tc::dereference_index(*m_baserng,idx);
						if (!VERIFYNOTIFY(tc::is_continuation_unit(ch))) return false;
						n = (n << 6) | (tc::underlying_cast(ch) & 0x3fu);
						tc::increment_index(*m_baserng,idx);
					} while (--i != 0);
					return true;
				};

				auto ch = tc::underlying_cast(tc::dereference_index(*m_baserng,idx));
				if (0==(ch & 0x80u)) {
					return static_cast<char32_t>(ch);
				} else if (0xc0==(ch & 0xe0u)) {
					n = ch & 0x1fu;
					// overlong sequences are forbidden (n < 0x80u can be encoded with a shorter sequence)
					if (Continuation(1) && VERIFYNOTIFY(0x80u <= n)) {
						_ASSERT(n < 0x800u);
						return static_cast<char32_t>(n);
					}
				} else if (0xe0==(ch & 0xf0u)) {
					n = ch & 0xfu;
					// overlong sequences are forbidden (n < 0x800u can be encoded with a shorter sequence)
					// UTF-16 uses U+D800 to U+DFFF for encoding low / high surrogates, this interval is also forbidden for UTF-8 by RFC 3629
					if (Continuation(2) && VERIFYNOTIFY(0x800u <= n) && VERIFYNOTIFY(!(0xD800u <= n && n <= 0xDFFFu))) {
						_ASSERT(n < 0x10000u);
						return static_cast<char32_t>(n);
					}
				} else if (0xf0==(ch & 0xf8u)) {
					n = ch & 0x7u;
					// overlong sequences are forbidden (n < 0x10000u can be encoded with a shorter sequence)
					// UTF-16 maximum code point is U+10FFFF, larger values are also forbidden for UTF-8 by RFC 3629
					if (Continuation(3) && VERIFYNOTIFY(0x10000u <= n) && VERIFYNOTIFY(n <= 0x10FFFFu)) {
						return static_cast<char32_t>(n);
					}
				}

				_ASSERTNOTIFYFALSE;
				return c_chReplacementCharacter;
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return tc::equal_index(*m_baserng,idxLhs, idxRhs);
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				auto Continuation = [&](auto i) noexcept {
					// Skip over starting byte and i continuation bytes
					++i;
					do {
						tc::increment_index(*m_baserng,idx);
					} while (--i != 0 && VERIFYNOTIFY(!tc::at_end_index(*m_baserng,idx)) && VERIFYNOTIFY(tc::is_continuation_unit(tc::dereference_index(*m_baserng,idx))));
				};
				auto ch = tc::underlying_cast(tc::dereference_index(*m_baserng,idx));
				if (0==(ch & 0x80u)) {
					Continuation(0);
				} else if (0xc0==(ch & 0xe0u)) {
					Continuation(1);
				} else if (0xe0==(ch & 0xf0u)) {
					Continuation(2);
				} else if (0xf0==(ch & 0xf8u)) {
					Continuation(3);
				} else {
					// This is an continuation byte without a corresponding starting byte, skip over subsequent continuation bytes
					_ASSERTNOTIFYFALSE;
					do {
						tc::increment_index(*m_baserng,idx);
					} while (!tc::at_end_index(*m_baserng,idx) && tc::is_continuation_unit(tc::dereference_index(*m_baserng,idx)));
				}
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				int nContinuationBytes = -1;
				do {
					tc::decrement_index(*m_baserng,idx);
					++nContinuationBytes;
				} while (tc::is_continuation_unit(tc::dereference_index(*m_baserng,idx)) && VERIFYNOTIFY(!tc::equal_index(*m_baserng,idx, tc::begin_index(m_baserng))));

				auto Continuation = [&](auto n) noexcept {
					// If we skipped over excess continuation bytes, return index to the first excess continuation byte
					if (n < nContinuationBytes) {
						this->increment_index(idx);
					}
				};

				auto ch = tc::dereference_index(*m_baserng,idx);
				if (0==(ch & 0x80u)) {
					Continuation(0);
				} else if (0xc0==(ch & 0xe0u)) {
					Continuation(1);
				} else if (0xe0==(ch & 0xf0u)) {
					Continuation(2);
				} else if (0xf0==(ch & 0xf8u)) {
					Continuation(3);
				} else {
					// This is a continuation byte, so if we stopped at it it had better be the first byte in the string.
					_ASSERT(tc::equal_index(*m_baserng,idx, tc::begin_index(m_baserng)));
					_ASSERTNOTIFYFALSE;
				}
			}

			auto border_base_index(index const& idx) const& noexcept {
				return idx;
			}

			constexpr decltype(auto) base_range() & noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() const& noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() && noexcept {
				return *std::move(m_baserng);
			}
			constexpr decltype(auto) base_range() const&& noexcept {
				return *std::move(m_baserng);
			}
		};
		
		template <typename Index>
		struct SUtf16Index final {
			Index m_idx;
			bool m_bTrailingSurrogate;
		};

		// Lazily convert UTF-32 strings to UTF-16
		template<typename Rng>
		struct SStringConversionRange<tc::char16, Rng, enable_if_range_of_t<char32_t, Rng>>
		: tc::range_iterator_generator_from_index<
			SStringConversionRange<tc::char16, Rng, char32_t>,
			SUtf16Index<tc::index_t<std::remove_reference_t<Rng>>>,
			typename boost::range_detail::demote_iterator_traversal_tag<
				boost::iterators::bidirectional_traversal_tag,
				traversal_t<Rng>
			>::type
		> {
			using index = typename SStringConversionRange::index;

			explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
			: m_baserng(aggregate_tag, std::forward<Rng>(rng))
			{}
		private:
			using this_type = SStringConversionRange<tc::char16, Rng, char32_t>;
			reference_or_value< Rng > m_baserng;

		public:
			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return {tc::begin_index(m_baserng), false};
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return {tc::end_index(m_baserng), false};
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::at_end_index(*m_baserng,idx.m_idx) && VERIFY(!idx.m_bTrailingSurrogate);
			}

			STATIC_FINAL(dereference_index)(index idx) const& noexcept -> tc::char16 {
				unsigned int n = tc::underlying_cast(tc::dereference_index(*m_baserng,idx.m_idx));
				if (0x10000 <= n) {
					if (VERIFYNOTIFY(n < 0x110000)) {
						if (idx.m_bTrailingSurrogate) {
							return tc::explicit_cast<uint16_t>((n - 0x10000u & 0x3FFu) + 0xDC00u);
						} else {
							return tc::explicit_cast<uint16_t>(((n - 0x10000u) >> 10u) + 0xD800u);
						}
					} else {
						return tc::explicit_cast<tc::char16>(c_chReplacementCharacter);
					}
				} else {
					_ASSERT(!idx.m_bTrailingSurrogate);
					_ASSERTNOTIFY(n < 0xD800 || 0xE000 <= n);
					return tc::explicit_cast<uint16_t>(n);
				}
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return idxLhs.m_bTrailingSurrogate == idxRhs.m_bTrailingSurrogate && tc::equal_index(*m_baserng,idxLhs.m_idx, idxRhs.m_idx);
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				if (!idx.m_bTrailingSurrogate && 0x10000 <= tc::underlying_cast(tc::dereference_index(*m_baserng,idx.m_idx))) {
					idx.m_bTrailingSurrogate = true;
				} else {
					idx.m_bTrailingSurrogate = false;
					tc::increment_index(*m_baserng,idx.m_idx);
				}
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				if (idx.m_bTrailingSurrogate) {
					idx.m_bTrailingSurrogate = false;
				} else {
					tc::decrement_index(*m_baserng,idx.m_idx);
					if (0x10000 <= tc::underlying_cast(tc::dereference_index(*m_baserng,idx.m_idx))) {
						idx.m_bTrailingSurrogate = true;
					}
				}
			}

			auto border_base_index(index const& idx) const& noexcept {
				_ASSERT(!idx.m_bTrailingSurrogate);
				return idx.m_idx;
			}

			constexpr decltype(auto) base_range() & noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() const& noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() && noexcept {
				return *std::move(m_baserng);
			}
			constexpr decltype(auto) base_range() const&& noexcept {
				return *std::move(m_baserng);
			}
		};

		template <typename Index>
		struct SUtf8Index final {
			Index m_idx;
			int m_nByte;
		};

		// Lazily convert UTF-32 strings to UTF-8
		template<typename Rng>
		struct SStringConversionRange<char, Rng, enable_if_range_of_t<char32_t, Rng>>
		: tc::range_iterator_generator_from_index<
			SStringConversionRange<char, Rng, char32_t>,
			SUtf8Index<tc::index_t<std::remove_reference_t<Rng>>>,
			typename boost::range_detail::demote_iterator_traversal_tag<
				boost::iterators::bidirectional_traversal_tag,
				traversal_t<Rng>
			>::type
		> {
			using index = typename SStringConversionRange::index;

			explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
			: m_baserng(aggregate_tag, std::forward<Rng>(rng))
			{}
		private:
			using this_type = SStringConversionRange<char, Rng, char32_t>;
			reference_or_value< Rng > m_baserng;

		public:
			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return {tc::begin_index(m_baserng), 0};
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return {tc::end_index(m_baserng), 0};
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::at_end_index(*m_baserng,idx.m_idx) && VERIFY(idx.m_nByte == 0);
			}

			STATIC_FINAL(dereference_index)(index idx) const& noexcept -> char {
				unsigned int n = tc::underlying_cast(tc::dereference_index(*m_baserng,idx.m_idx));
				if (n < 0x80) {
					_ASSERTEQUAL(idx.m_nByte, 0);
					return static_cast<char>(n);
				} else {
					if (!VERIFYNOTIFY(n < 0x110000) || !VERIFYNOTIFY(!(0xd800 <= n && n < 0xe000))) {
						n = tc::underlying_cast(c_chReplacementCharacter);
					}

					if (idx.m_nByte == 0) {
						int nBytes = Utf8Bytes(n);
						return static_cast<char>(n >> 6*(nBytes-1) | ((1 << (nBytes+1)) - 1) << (8 - nBytes));
					} else {
						return static_cast<char>((n >> 6*(Utf8Bytes(n)-idx.m_nByte-1) & 0x3fu) | 0x80);
					}
				}
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return idxLhs.m_nByte == idxRhs.m_nByte && tc::equal_index(*m_baserng,idxLhs.m_idx, idxRhs.m_idx);
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				++idx.m_nByte;
				if (Utf8Bytes(tc::dereference_index(*m_baserng,idx.m_idx)) <= idx.m_nByte) {
					idx.m_nByte = 0;
					tc::increment_index(*m_baserng,idx.m_idx);
				}
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				if (idx.m_nByte == 0) {
					tc::decrement_index(*m_baserng,idx.m_idx);
					idx.m_nByte = Utf8Bytes(tc::dereference_index(*m_baserng,idx.m_idx)) - 1;
				} else {
					--idx.m_nByte;
				}
			}

			auto border_base_index(index const& idx) const& noexcept {
				_ASSERTEQUAL(idx.m_nByte, 0);
				return idx.m_idx;
			}

			constexpr decltype(auto) base_range() & noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() const& noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() && noexcept {
				return *std::move(m_baserng);
			}
			constexpr decltype(auto) base_range() const&& noexcept {
				return *std::move(m_baserng);
			}
		};

		template<typename Rng>
		struct SStringConversionRange<char, Rng, enable_if_range_of_t<tc::char16, Rng>> final : SStringConversionRange<char, SStringConversionRange<char32_t, Rng>> {
			using index = typename SStringConversionRange::index;
		private:
			using base_ = SStringConversionRange<char, SStringConversionRange<char32_t, Rng>>;
		public:
			explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
			: base_(aggregate_tag, SStringConversionRange<char32_t, Rng>(aggregate_tag, std::forward<Rng>(rng)))
			{}

			auto border_base_index(index const& idx) const& noexcept {
				return base_::base_range().border_base_index(base_::border_base_index(idx));
			}

			constexpr decltype(auto) base_range() & noexcept {
				return base_::base_range().base_range();
			}
			constexpr decltype(auto) base_range() const& noexcept {
				return base_::base_range().base_range();
			}
			constexpr decltype(auto) base_range() && noexcept {
				return std::move(*this).base_::base_range().base_range();
			}
			constexpr decltype(auto) base_range() const&& noexcept {
				return std::move(*this).base_::base_range().base_range();
			}
		};

		template<typename Rng>
		struct SStringConversionRange<tc::char16, Rng, enable_if_range_of_t<char, Rng>> final : SStringConversionRange<tc::char16, SStringConversionRange<char32_t, Rng>> {
			using index = typename SStringConversionRange::index;
		private:
			using base_ = SStringConversionRange<tc::char16, SStringConversionRange<char32_t, Rng>>;
		public:
			explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
			: base_(aggregate_tag, SStringConversionRange<char32_t, Rng>(aggregate_tag, std::forward<Rng>(rng)))
			{}

			auto border_base_index(index const& idx) const& noexcept {
				return base_::base_range().border_base_index(base_::border_base_index(idx));
			}

			auto base_range() & noexcept return_decltype(
				base_::base_range().base_range()
			)

			auto base_range() const& noexcept return_decltype(
				base_::base_range().base_range()
			)
		};
	

		template< typename Dst, typename Src, typename Enable=void >
		struct choose_convert_enc;

		template< typename Dst, typename Src >
		struct choose_convert_enc<Dst, Src, std::enable_if_t<!tc::is_char<std::remove_reference_t<Src>>::value> > {
			static constexpr int value=!std::is_same< Dst, tc::range_value_t< Src > >::value ? 0 : 1;
		};
	} // namespace convert_enc_impl

	//--------------------------------------------------------------------------------------------------------------------------
	// must_convert_enc
	// Converts range of char type to a lazy range of a different char type

	template< typename Dst, typename Src, std::enable_if_t<tc::is_char<Dst>::value>* = nullptr>
	auto must_convert_enc(Src&& src) noexcept return_ctor(
		convert_enc_impl::SStringConversionRange<Dst BOOST_PP_COMMA() Src>,
		(aggregate_tag, std::forward<Src>(src))
	)

	//--------------------------------------------------------------------------------------------------------------------------
	// may_convert_enc
	// Either forwards its argument (if it is a range of Dst), or converts it to a lazy range of Dst (if it is a range of another char type).

	template< typename Dst, typename Src, std::enable_if_t<!std::is_same<tc::range_value_t<Src>, Dst>::value >* = nullptr>
	auto may_convert_enc( Src&& src ) noexcept return_decltype(
		tc::must_convert_enc<Dst>(std::forward<Src>(src))
	)

	template< typename Dst, typename Src, std::enable_if_t<std::is_same<tc::range_value_t<Src>, Dst>::value >* = nullptr>
	Src&& may_convert_enc( Src&& src ) noexcept {
		return std::forward<Src>( src );
	}
}

