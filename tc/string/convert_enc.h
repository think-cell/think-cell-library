
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/bitfield.h"
#include "../base/rvalue_property.h"
#include "../base/bit_cast.h"
#include "../algorithm/empty.h"
#include "../algorithm/compare.h"
#include "../range/range_adaptor.h"

#include "char.h"

namespace tc {
	// Code unit predicates
	//
	// These names are based on the terminology used in the Unicode Standard: https://www.unicode.org/versions/Unicode13.0.0/UnicodeStandard-13.0.pdf
	// "(...) reserved for the first, or leading, element of a UTF-8 code unit sequences, (...) reserved for the subsequent, or trailing, elements of such sequences;"
	// "(...) for the lead, trail, or single code units in any of those encoding forms overlap."
	[[nodiscard]] constexpr bool is_leading_codeunit(char const ch) noexcept {
		return 0xc0 == (ch & 0xc0); // matches 11xxxxxx
	}
	[[nodiscard]] constexpr bool is_leading_codeunit(tc::char16 const ch) noexcept {
		return 0xd800u == (ch & 0xfc00u); // high surrogate
	}
	template <tc::char_type Char, Char chFirst, Char chLast>
	[[nodiscard]] constexpr auto is_leading_codeunit(tc::restricted_enum<Char, chFirst, chLast> const ch) noexcept {
		if constexpr(chLast < 0x80) {
			return tc::constant<false>{};
		} else {
			return is_leading_codeunit(tc::implicit_cast<Char>(ch));
		}
	}

	[[nodiscard]] constexpr bool is_trailing_codeunit(char const ch) noexcept {
		return 0x80 == (ch & 0xc0); // matches 10xxxxxx
	}
	[[nodiscard]] constexpr bool is_trailing_codeunit(tc::char16 const ch) noexcept {
		return 0xdc00u == (ch & 0xfc00u); // low surrogate
	}
	template <tc::char_type Char, Char chFirst, Char chLast>
	[[nodiscard]] constexpr auto is_trailing_codeunit(tc::restricted_enum<Char, chFirst, chLast> const ch) noexcept {
		if constexpr(chLast < 0x80) {
			return tc::constant<false>{};
		} else {
			return is_trailing_codeunit(tc::implicit_cast<Char>(ch));
		}
	}

	template<typename T>
	[[nodiscard]] constexpr bool is_single_codeunit(T const ch) noexcept {
		return char_in_range<T>(tc::to_underlying(ch));
	}

	namespace codeunit_sequence_size_detail {
		[[nodiscard]] constexpr inline std::optional<int> codeunit_sequence_size_raw(char const ch) noexcept {
			if( auto const n=0xffu & ~tc::to_underlying(ch); 0!=n ) { // code unit 0xff is invalid
				switch(tc::index_of_most_significant_bit(n)) {
					case 7: return 1;
					case 5: return 2;
					case 4: return 3;
					case 3: return 4;
					default: break; // 6=continuation, otherwise invalid
				}
			}
			return std::nullopt;
		}

		// tc::make_interval not used to avoid dependency cycle
		[[nodiscard]] constexpr std::optional<int> codeunit_sequence_size_raw(tc::char16 const ch) noexcept {
			if( auto const n=tc::to_underlying(ch); n<0xd800u || 0xdfffu<n) return 1;
			else if( n<0xdc00u ) return 2; // high-surrogate
			else return std::nullopt; // low-surrogate, continuation
		}

		[[nodiscard]] constexpr std::optional<int> codeunit_sequence_size_raw(tc::char_ascii const ch) noexcept {
			return 1;
		}

		using osize_t = std::optional<tc::size_proxy<int>>;

		template<typename Char>
		[[nodiscard]] constexpr inline osize_t codeunit_sequence_size(Char ch) noexcept { 
			return osize_t(codeunit_sequence_size_raw(ch));
		}
	}
	using codeunit_sequence_size_detail::codeunit_sequence_size_raw;
	using codeunit_sequence_size_detail::codeunit_sequence_size;

	TC_DEFINE_ENUM(ECodeunitSequenceType, ecodeunitseqtyp,
		(TRUNCATED) // The sequence lacks continuation code unit(s). In UTF-16, an unpaired high-surrogate.
		// Embedded in any code unit sequence, end == increment(decrement(end)) for sequence types below.
		(VALID) // A structurally well formed sequence. In UTF-8, the sequence may still encode surrogate code points or be an overlong encoding. In utf-16, such a sequence is always a valid encoding.
		// Embedded in any code unit sequence, begin == decrement(increment(begin)) for sequence types above.
		(CONTINUATION) // The sequence is a single continuation code unit. In UTF-16, an unpaired low-surrogate.
	)

	template<typename Rng, typename Index>
	constexpr ECodeunitSequenceType codepoint_increment_index(Rng const& rng, Index& idx) MAYTHROW {
		auto onSequenceSize = tc::codeunit_sequence_size_raw(tc::dereference_index(rng, idx) /*MAYTHROW*/);
		tc::increment_index(rng, idx); // MAYTHROW
		if( onSequenceSize ) {
			for( ; 0 != --*onSequenceSize; tc::increment_index(rng, idx) /* MAYTHROW*/ ) {
				if( tc::at_end_index(rng, idx) || !tc::is_trailing_codeunit(tc::dereference_index(rng, idx) /*MAYTHROW*/) ) TC_UNLIKELY {
					return ecodeunitseqtypTRUNCATED;
				}
			}
			return ecodeunitseqtypVALID;
		} else {
			// Not [[unlikely]], if idx is an arbitrary index.
			return ecodeunitseqtypCONTINUATION;
		}
	}

	template<tc::has_decrement_index Rng, typename Index> requires
		tc::is_equality_comparable<Index>::value
	constexpr ECodeunitSequenceType codepoint_decrement_index(Rng const& rng, Index& idx) MAYTHROW {
		tc::decrement_index(rng, idx); // MAYTHROW
		int nCodeUnits = 1;
		static_assert( 1 < tc::char_limits<tc::range_value_t<Rng const&>>::c_nMaxCodeUnitsPerCodePoint );

		for (auto idxPeek = idx;; ++nCodeUnits, tc::decrement_index(rng, idxPeek) /* MAYTHROW*/) {
			if( auto const onSequenceSize = tc::codeunit_sequence_size_raw(tc::dereference_index(rng, idxPeek) /*MAYTHROW*/) ) {
				if( nCodeUnits <= *onSequenceSize ) {
					idx = tc_move(idxPeek);
					if( nCodeUnits == *onSequenceSize ) {
						return ecodeunitseqtypVALID;
					} else {
						// We decrement arbitrary indices to establish code-point-alignment in codepoint_range.
						// Hence, this branch is not [[unlikely]].
						return ecodeunitseqtypTRUNCATED;
					}
				} else TC_UNLIKELY {
					return ecodeunitseqtypCONTINUATION;
				}
			} else if( tc::explicit_cast<int>(tc::char_limits<tc::range_value_t<Rng const&>>::c_nMaxCodeUnitsPerCodePoint) <= nCodeUnits
				|| tc::begin_index(rng) == idxPeek //MAYTHROW
			) TC_UNLIKELY {
				return ecodeunitseqtypCONTINUATION;
			}
		}
	}

	constexpr char32_t surrogate_pair_value(tc::char16 const chLeading, tc::char16 const chTrailing) noexcept {
		auto n = tc::to_underlying(chLeading) - 0xd800u;
		_ASSERTDEBUG(n<0x400u);
		n<<=10;

		unsigned int n2=tc::to_underlying(chTrailing)-0xdc00u;
		_ASSERTDEBUG(n2<0x400u);
		n+=n2+0x10000u;
		_ASSERTDEBUG(n<0x110000u);
		return static_cast<char32_t>(n);
	}

	template<typename Rng>
	constexpr std::optional<char32_t> codepoint_value_impl(Rng const& rng, tc::index_t<Rng const> idx) MAYTHROW {
		static_assert(
			std::is_same<tc::char16, tc::range_value_t<Rng const&>>::value ||
			std::is_same<char, tc::range_value_t<Rng const&>>::value ||
			std::is_same<tc::char_ascii, tc::range_value_t<Rng const&>>::value
		);
		if(	auto const ch=tc::dereference_index(rng, idx); // MAYTHROW
			auto const onSequenceSize=tc::codeunit_sequence_size_raw(ch)
		) {
			unsigned int n=tc::to_underlying(ch);
			tc::increment_index(rng, idx); // MAYTHROW
			if( 1==*onSequenceSize || (!tc::at_end_index(rng, idx) && [&]() MAYTHROW {
				if constexpr( std::is_same<tc::char16, tc::range_value_t<Rng const&>>::value ) {
					auto const ch2=tc::dereference_index(rng, idx); // MAYTHROW
					_ASSERTDEBUGEQUAL(*onSequenceSize, 2);
					if(tc::is_trailing_codeunit(ch2)) {
						n=tc::to_underlying(tc::surrogate_pair_value(ch, ch2));
						return true;
					}
				} else if constexpr(std::is_same<char, tc::range_value_t<Rng const&>>::value) {
					_ASSERTDEBUGANYOF(*onSequenceSize, (2)(3)(4));
					n &= 0x7fu >> *onSequenceSize; // 2 -> 0x1f, 3 -> 0xf, 4 -> 0x7
					int i=*onSequenceSize;
					do {
						auto const ch2=tc::dereference_index(rng, idx); // MAYTHROW
						if(!tc::is_trailing_codeunit(ch2)) TC_UNLIKELY return false;

						n<<=6;
						n|=tc::to_underlying(ch2) & 0x3fu;
						if(1 == --i) {
							switch_no_default(*onSequenceSize) {
								case 2: return 0x80u<=n;
								case 3: return (0x800u<=n && n<0xd800u) || 0xdfffu<n;
								case 4: return 0x10000u<=n && n<0x110000u;
							}
						}
						tc::increment_index(rng, idx); // MAYTHROW
					} while(!tc::at_end_index(rng, idx));
				} else {
					_ASSERTE(false); // *onSequenceSize is always 1 for tc::char_ascii
				}
				TC_UNLIKELY return false;
			}()/*MAYTHROW*/) ) {
				return tc::bit_cast<char32_t>(n);
			}
		}
		TC_UNLIKELY return std::nullopt;
	}

	template<typename Rng>
	constexpr decltype(auto) codepoint_value(Rng const& rng) MAYTHROW {
		_ASSERTE( !tc::empty(rng) );
		return tc::codepoint_value_impl(rng, tc::begin_index(rng)); // MAYTHROW
	}

	template<typename Char>
	constexpr int codepoint_codeunit_count(unsigned int nCodePoint) noexcept {
		if constexpr( std::same_as<char, Char> ) {
			return 
				nCodePoint<0x80u ? 1
				: nCodePoint<0x800u	? 2
				: nCodePoint<0x10000u ? 3 : 4;
		} else if constexpr( std::same_as<tc::char16, Char> ) {
			return nCodePoint<0x10000u ? 1 : 2;
		} else {
			return 1;
		}
	}

	template<typename Char>
	constexpr Char codepoint_codeunit_at(unsigned int nCodePoint, int const nCodeUnitIndex) noexcept {
		_ASSERTDEBUG(nCodePoint<0x110000u);
		auto const nLastIndex=tc::codepoint_codeunit_count<Char>(nCodePoint) - 1;
		if constexpr( std::same_as<char, Char> ) {
			return tc::bit_cast<char>(tc::explicit_cast<std::uint8_t>([&]() noexcept {
				switch_no_default(nCodeUnitIndex) {
					case 0:
						return 0==nLastIndex
							? nCodePoint
							: nCodePoint>>nLastIndex*6 | ((1 << (nLastIndex+1))-1)<<(7-nLastIndex);
					case 1:
					case 2:
					case 3:
						_ASSERTDEBUG(nCodeUnitIndex<=nLastIndex);
						return (nCodePoint>>(nLastIndex-nCodeUnitIndex)*6 & 0x3fu) | 0x80u;
				}
			}()));
		} else if constexpr( std::same_as<tc::char16, Char> ) {
			return tc::bit_cast<tc::char16>(tc::explicit_cast<std::uint16_t>([&]() noexcept {
				switch_no_default(nCodeUnitIndex) {
					case 0:
						switch_no_default(nLastIndex) {
							case 0: return nCodePoint;
							case 1: return ((nCodePoint-0x10000u) >> 10)+0xd800u;
						}
					case 1:
						_ASSERTDEBUG(1==nLastIndex);
						return (nCodePoint-0x10000u & 0x3ffu)+0xdc00u;
				}
			}()));
		} else {
			tc_return_cast(nCodePoint);
		}
	}

	namespace convert_enc_impl {
		template <typename Dst, typename Rng, typename Src=tc::range_value_t<Rng>>
		struct SStringConversionRange;

		// Lazily convert UTF-8/UTF-16 strings to UTF-32
		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<char32_t, Rng>
			: tc::index_range_adaptor<
				SStringConversionRange<char32_t, Rng>,
				Rng,
				tc::index_range_adaptor_flags::inherit_begin_end
			>
		{
		private:
			using base_ = typename SStringConversionRange::index_range_adaptor;
			static_assert( std::is_same<char, tc::range_value_t<Rng>>::value || std::is_same<tc::char16, tc::range_value_t<Rng>>::value );

		public:
			using base_::base_;
			using typename base_::tc_index;

			static constexpr bool c_bHasStashingIndex=false;

		private:
			using this_type = SStringConversionRange;

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& MAYTHROW {
				VERIFYNOTIFYEQUAL(tc::codepoint_increment_index(this->base_range(), idx), ecodeunitseqtypVALID);
			}

			STATIC_FINAL_MOD(
				template<ENABLE_SFINAE> constexpr,
				decrement_index
			)(SFINAE_TYPE(tc_index)& idx) const& return_decltype_MAYTHROW(
				void(VERIFYNOTIFYEQUAL( tc::codepoint_decrement_index(this->base_range(), idx) /*MAYTHROW*/, ecodeunitseqtypVALID ))
			)

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) const& MAYTHROW {
				if (auto och=VERIFYNOTIFY( tc::codepoint_value_impl(this->base_range(), idx) )) { // MAYTHROW
					return *och;
				} else {
					return U'\uFFFD'; // REPLACEMENT CHARACTER
				}
			}

		public:
			static constexpr auto border_base_index(tc_index const& idx) noexcept {
				return idx;
			}
		};

		template<typename Index>
		struct SCodeUnitIndex final {
			Index m_idx;
			int m_nCodeUnitIndex;

			friend bool operator==(SCodeUnitIndex const& lhs, SCodeUnitIndex const& rhs) noexcept = default;
		};

		template<typename Derived, typename Rng, typename Char>
		struct SStringConversionFromUtf32RangeBase
			: tc::range_iterator_from_index<
				Derived,
				SCodeUnitIndex<tc::index_t<std::remove_reference_t<Rng>>>
			>
			, tc::range_adaptor_base_range<Rng>
		{
		private:
			using this_type = SStringConversionFromUtf32RangeBase;
			using base_ = tc::range_adaptor_base_range<Rng>;

		public:
			using base_::base_;
			using typename this_type::range_iterator_from_index::tc_index;

			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng>>::value;

		private:
			// Indexes pointing to continuation code units are invalid. If the underlying range contains lone continuation units, they will be converted to a
			// REPLACEMENT CHARACTER
			template<ENABLE_SFINAE> // clang workaround
			constexpr auto codepoint_at(decltype(tc_index::m_idx) const& idxBaseRng) const& MAYTHROW {
				if(	auto const n=tc::to_underlying(tc::dereference_index(SFINAE_VALUE(this)->base_range(), idxBaseRng)); // MAYTHROW
					VERIFYNOTIFY(n<0x110000u) && VERIFYNOTIFY(n<0xd800u || 0xdfffu<n) 
				) {
					return n;
				} else {
					return tc::explicit_cast<decltype(n)>(0xfffd); // U+FFFD REPLACEMENT CHARACTER
				}
			}

			STATIC_OVERRIDE_MOD(constexpr, begin_index)() const& return_ctor_MAYTHROW(
				tc_index,
				{this->base_begin_index() /*MAYTHROW*/, 0}
			)

			STATIC_OVERRIDE_MOD(TC_FWD(
				template<
					ENABLE_SFINAE,
					std::enable_if_t<tc::has_mem_fn_end_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>>* = nullptr
				> constexpr),
				end_index
			)() const& return_ctor_MAYTHROW(
				tc_index,
				{tc::end_index(SFINAE_VALUE(this->base_range())) /*MAYTHROW*/, 0}
			)

			STATIC_OVERRIDE_MOD(constexpr, at_end_index)(tc_index const& idx) const& return_decltype_MAYTHROW(
				tc::at_end_index(this->base_range(), idx.m_idx) && (_ASSERTE(0==idx.m_nCodeUnitIndex), true)
			)

			STATIC_OVERRIDE_MOD(constexpr, dereference_index)(tc_index const& idx) const& return_MAYTHROW(
				tc::codepoint_codeunit_at<Char>(codepoint_at(idx.m_idx) /*MAYTHROW*/, idx.m_nCodeUnitIndex)
			)

			STATIC_OVERRIDE_MOD(constexpr, increment_index)(tc_index& idx) const& MAYTHROW {
				if(++idx.m_nCodeUnitIndex == tc::codepoint_codeunit_count<Char>(codepoint_at(idx.m_idx) /*MAYTHROW*/)) {
					idx.m_nCodeUnitIndex=0;
					tc::increment_index(this->base_range(), idx.m_idx); // MAYTHROW
				}
			}

			STATIC_OVERRIDE_MOD(constexpr, decrement_index)(tc_index& idx) const& MAYTHROW
				requires tc::has_decrement_index<std::remove_reference_t<Rng>>
			{
				if(0==idx.m_nCodeUnitIndex) {
					tc::decrement_index(this->base_range(), idx.m_idx); // MAYTHROW
					idx.m_nCodeUnitIndex=tc::codepoint_codeunit_count<Char>(codepoint_at(idx.m_idx) /*MAYTHROW*/) - 1;
				} else {
					--idx.m_nCodeUnitIndex;
				}
			}

		public:
			static constexpr auto border_base_index(tc_index const& idx) noexcept {
				_ASSERTE(0==idx.m_nCodeUnitIndex);
				return idx.m_idx;
			}
		};

		// Lazily convert UTF-32 strings to UTF-16
		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<tc::char16, Rng, char32_t>
			: SStringConversionFromUtf32RangeBase<SStringConversionRange<tc::char16, Rng>, Rng, tc::char16>
		{
		private:
			using this_type = SStringConversionRange<tc::char16, Rng>;
			using base_ = SStringConversionFromUtf32RangeBase<this_type, Rng, tc::char16>;

		public:
			using base_::base_;
		};

		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<char, Rng, char32_t>
			: SStringConversionFromUtf32RangeBase<SStringConversionRange<char, Rng>, Rng, char>
		{
		private:
			using this_type = SStringConversionRange<char, Rng>;
			using base_ = SStringConversionFromUtf32RangeBase<this_type, Rng, char>;

		public:
			using base_::base_;
		};

		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<char, Rng, tc::char16> final
			: SStringConversionRange<char, SStringConversionRange<char32_t, Rng>>
		{
		private:
			using base_ = SStringConversionRange<char, SStringConversionRange<char32_t, Rng>>;

			template<typename Self>
			static constexpr decltype(auto) base_range_(Self&& self) noexcept {
				return tc_move_if_owned(self).base_::base_range().base_range();
			}

		public:
			constexpr explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
				: base_(aggregate_tag, SStringConversionRange<char32_t, Rng>(aggregate_tag, tc_move_if_owned(rng)))
			{}
			using typename base_::tc_index;

			constexpr auto border_base_index(tc_index const& idx) const& return_decltype_noexcept(
				base_::base_range().border_base_index(base_::border_base_index(idx))
			)

			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(base_range)
		};

		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<tc::char16, Rng, char> final
			: SStringConversionRange<tc::char16, SStringConversionRange<char32_t, Rng>>
		{
		private:
			using base_ = SStringConversionRange<tc::char16, SStringConversionRange<char32_t, Rng>>;

			template<typename Self>
			static constexpr decltype(auto) base_range_(Self&& self) noexcept {
				return tc_move_if_owned(self).base_::base_range().base_range();
			}

		public:
			constexpr explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
				: base_(aggregate_tag, SStringConversionRange<char32_t, Rng>(aggregate_tag, tc_move_if_owned(rng)))
			{}
			using typename base_::tc_index;

			constexpr auto border_base_index(tc_index const& idx) const& return_decltype_noexcept(
				base_::base_range().border_base_index(base_::border_base_index(idx))
			)

			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(base_range)
		};
	} // namespace convert_enc_impl

	//--------------------------------------------------------------------------------------------------------------------------
	// convert_enc
	// Either forwards its argument (if it is a range of Dst), or converts it to a lazy range of Dst (if it is a range of another char type).

	namespace no_adl {
		template<typename Sink, tc::char_like Dst>
		struct convert_enc_sink;
	}

	namespace convert_enc_detail {
		template<tc::char_like Dst, typename Src>
		[[nodiscard]] decltype(auto) with_sink_impl(Src&& src) noexcept {
			return tc::generator_range_output<Dst>([src=tc::make_reference_or_value(tc_move_if_owned(src))](auto&& sink) MAYTHROW {
				return tc::for_each(*src, no_adl::convert_enc_sink<decltype(sink), Dst>(tc_move_if_owned(sink)));
			});
		}

		template <tc::char_like Dst, auto ... Cs>
		[[nodiscard]] constexpr auto convert_literal(tc::literal_range<tc::char_ascii, Cs...>) noexcept {
			// We cast the Cs to char, to keep a simple type as NTTP and because presumably every char_like type is constructible from char.
			return tc::literal_range<Dst, tc::implicit_cast<char>(Cs)...>{};
		}
	}

	template<tc::char_like Dst, typename Src>
	[[nodiscard]] decltype(auto) convert_enc(Src&& src) noexcept {
		if constexpr(tc::has_range_value<Src>) {
			static_assert(tc::char_like<tc::range_value_t<Src>>);
			if constexpr(tc::safely_convertible_to<tc::range_value_t<Src>, Dst>) {
				return tc_move_if_owned(src);
			} else if constexpr(tc::char_type<Dst> && tc::char_type<tc::range_value_t<Src>> && tc::range_with_iterators<Src>) {
				return convert_enc_impl::SStringConversionRange<Dst, Src>{aggregate_tag, tc_move_if_owned(src)};
			} else if constexpr(std::same_as<Dst, tc::char_ascii>) {
				return tc::transform(tc_move_if_owned(src), tc::fn_explicit_cast<tc::char_ascii>());
			} else {
				return convert_enc_detail::with_sink_impl<Dst>(tc_move_if_owned(src));
			}
		} else {
			return convert_enc_detail::with_sink_impl<Dst>(tc_move_if_owned(src));
		}
	}

	template<tc::char_like Dst, typename Src> requires tc::instance_or_derived<std::remove_reference_t<Src>, convert_enc_impl::SStringConversionRange>
	[[nodiscard]] auto convert_enc(Src&& src) noexcept {
		return tc::convert_enc<Dst>(tc_move_if_owned(src).base_range());
	}

	template<tc::char_like Dst, typename Src> requires tc::is_literal_range<Src> && std::same_as<tc::range_value_t<Src>, tc::char_ascii>
	[[nodiscard]] auto convert_enc(Src&& src) noexcept {
		return convert_enc_detail::convert_literal<Dst>(src);
	}

	namespace no_adl {
		template<typename Sink, tc::char_like Dst>
		struct convert_enc_sink /*final*/ {
		private:
			tc::decay_t<Sink> m_sink;

		public:
			explicit convert_enc_sink(Sink&& sink) noexcept
				: m_sink(tc_move_if_owned(sink)) 
			{}

			template<tc::char_like CharT, std::enable_if_t<tc::safely_convertible_to<CharT, Dst>>* = nullptr>
			auto operator()(CharT ch) const& return_decltype_MAYTHROW(
				m_sink(ch)
			)

			template<typename Rng, std::enable_if_t<tc::range_with_iterators<Rng> && tc::char_like<tc::range_value_t<Rng>>>* = nullptr> // terse syntax triggers VS17.1 ICE
			auto chunk(Rng&& rng) const& return_decltype_MAYTHROW(
				tc::for_each(tc::convert_enc<Dst>(tc_move_if_owned(rng)), m_sink)
			)
		};
	}
}

