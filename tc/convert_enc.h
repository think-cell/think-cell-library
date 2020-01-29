
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "casts.h"
#include "bit_cast.h"
#include "range_adaptor.h"
#include "trivial_functors.h"
#include "rvalue_property.h"

namespace tc {
	[[nodiscard]] constexpr bool is_continuation_codeunit(char ch) noexcept {
		return 0x80 == (0xc0 & ch);
	}

	// tc::make_interval not used to avoid dependency cycle
	[[nodiscard]] constexpr bool is_continuation_codeunit(tc::char16 ch) noexcept {
		auto const n=tc::underlying_cast(ch);
		return 0xdc00u<=n && n<0xe000u; // low-surrogate
	}

	namespace codeunit_sequence_size_detail {
		using osize_t = std::optional<tc::size_proxy<int>>;
		[[nodiscard]] /*not constexpr, relies on intrinsics*/ inline osize_t codeunit_sequence_size(char ch) noexcept { 
			if(	auto const n=/*bitwise-not triggers promotion of operand to int*/0xff & ~tc::underlying_cast(ch);
				0!=n // code unit 0xff is invalid
			) {
				switch(tc::index_of_most_significant_bit(n)) {
					case 7: return osize_t(1);
					case 5: return osize_t(2);
					case 4: return osize_t(3);
					case 3: return osize_t(4);
					default: break; // 6=continuation, otherwise invalid
				}
			}
			return std::nullopt;
		}

		// tc::make_interval not used to avoid dependency cycle
		[[nodiscard]] constexpr osize_t codeunit_sequence_size(tc::char16 ch) noexcept {
			auto const n=tc::underlying_cast(ch);
			return
				n<0xd800u || 0xdfffu<n ? osize_t(1)
				: n<0xdc00u ? osize_t(2) // high-surrogate
				: std::nullopt; // low-surrogate, continuation
		}
	}
	using codeunit_sequence_size_detail::codeunit_sequence_size;

	namespace convert_enc_impl {
		inline constexpr char32_t c_chReplacementCharacter = 0xfffd; // U+FFFD REPLACEMENT CHARACTER

		template <typename Dst, typename Rng, typename Src=tc::range_value_t<Rng>>
		struct SStringConversionRange;

		template <typename T, typename Rng>
		using enable_if_range_of_t = std::enable_if_t<std::is_same<T, tc::range_value_t<Rng>>::value, T>;

		template<typename Derived, typename Rng>
		struct SStringConversionToUtf32RangeBase
			: tc::range_iterator_generator_from_index<
				Derived,
				tc::index_t<std::remove_reference_t<Rng>>
			> 
		{
			constexpr explicit SStringConversionToUtf32RangeBase(tc::aggregate_tag_t, Rng&& rng) noexcept
				: m_baserng(tc::aggregate_tag, std::forward<Rng>(rng))
			{}
			using index = typename SStringConversionToUtf32RangeBase::index;

		protected:
			// Indexes pointing to continuation code units are invalid. If the underlying range contains lone continuation units, they will be converted to a
			// REPLACEMENT CHARACTER

			template<typename FuncAggregateSequence>
			constexpr auto char_at(index const& idx, FuncAggregateSequence funcAggregateSequence) const& MAYTHROW {
				if(	auto const ch=tc::dereference_index(*m_baserng, idx); // MAYTHROW
					auto const onSequenceSize=VERIFYNOTIFY(tc::codeunit_sequence_size(ch))
				) {
					unsigned int n=tc::underlying_cast(ch);
					if(1==*onSequenceSize || funcAggregateSequence(n, *onSequenceSize) /*MAYTHROW*/) {
						return tc::bit_cast<char32_t>(n);
					}
				}
				return c_chReplacementCharacter;
			}

			reference_or_value<Rng> m_baserng;

		private:
			using this_type = SStringConversionToUtf32RangeBase;

			STATIC_OVERRIDE_MOD(constexpr, begin_index)() const& return_decltype_MAYTHROW(
				tc::begin_index(m_baserng)
			)

			STATIC_OVERRIDE_MOD(
				template<ENABLE_SFINAE> constexpr,
				end_index
			)() const& return_decltype_MAYTHROW(
				tc::end_index(tc::base_cast<SFINAE_TYPE(this_type)>(this)->m_baserng)
			)

			STATIC_OVERRIDE_MOD(constexpr, at_end_index)(index const& idx) const& return_decltype_MAYTHROW(
				tc::at_end_index(*m_baserng, idx)
			)

			STATIC_OVERRIDE_MOD(
				template<ENABLE_SFINAE> constexpr,
				equal_index
			)(SFINAE_TYPE(index) const& idxLhs, index const& idxRhs) const& return_decltype_noexcept(
				tc::equal_index(*m_baserng, idxLhs, idxRhs)
			)

			STATIC_OVERRIDE_MOD(constexpr, increment_index)(index& idx) const& MAYTHROW {
				auto const onSequenceSize=VERIFYNOTIFY(tc::codeunit_sequence_size(tc::dereference_index(*m_baserng, idx))); // MAYTHROW
				int i=0;
				do {
					tc::increment_index(*m_baserng, idx); // MAYTHROW
				} while(
					(!onSequenceSize || ++i<*onSequenceSize) &&
					VERIFYNOTIFYPRED(!tc::at_end_index(*m_baserng, idx), !onSequenceSize || _) &&
					VERIFYNOTIFYPRED(tc::is_continuation_codeunit(tc::dereference_index(*m_baserng, idx) /*MAYTHROW*/), !onSequenceSize || _)
				);
			}

			STATIC_OVERRIDE_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<
						tc::has_decrement_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value &&
						tc::has_equal_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value
					>* = nullptr
				> constexpr,
				decrement_index
			)(index& idx) const& MAYTHROW {
				tc::range_value_t<Rng> ch;
				int nCodeUnits=0;
				do {
					tc::decrement_index(*m_baserng, idx); // MAYTHROW
					ch=tc::dereference_index(*m_baserng, idx); // MAYTHROW
					++nCodeUnits;
				} while(
					tc::is_continuation_codeunit(ch) &&
					!tc::equal_index(*m_baserng, idx, tc::begin_index(m_baserng) /*MAYTHROW*/)
				);

				if(auto const onSequenceSize=VERIFYNOTIFY(tc::codeunit_sequence_size(VERIFYINITIALIZED(ch)))) {
					if(int i=*onSequenceSize; i<nCodeUnits) {
						// moved backwards from an invalid sequence over into a valid one - avoid skipping the invalid sequence, move forward again
						_ASSERTNOTIFYFALSE;
						do {
							tc::increment_index(*m_baserng, idx); // MAYTHROW
						} while(0<--i);
					}
				}
			}

			template<typename Self>
			static constexpr decltype(auto) base_range_(Self&& self) noexcept {
				return *std::forward<Self>(self).m_baserng;
			}

		public:
			constexpr auto border_base_index(index const& idx) const& noexcept {
				return idx;
			}

			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(base_range)
		};

		// Lazily convert UTF-16 strings to UTF-32
		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<char32_t, Rng, enable_if_range_of_t<tc::char16, Rng>>
			: SStringConversionToUtf32RangeBase<SStringConversionRange<char32_t, Rng>, Rng>
		{
		private:
			using this_type = SStringConversionRange<char32_t, Rng>;
			using base_ = SStringConversionToUtf32RangeBase<this_type, Rng>;

		public:
			constexpr explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
				: base_(aggregate_tag, std::forward<Rng>(rng))
			{}
			using typename base_::index;

		private:
			STATIC_FINAL_MOD(constexpr, dereference_index)(index const& idx) const& MAYTHROW {
				return base_::char_at(
					idx,
					[&](unsigned int& n, int IF_TC_CHECKS(IF_TC_DEBUG(nSequenceSize))) MAYTHROW {
						_ASSERTDEBUG(2==nSequenceSize);
						auto const idx2=modified(idx, tc::increment_index(*this->m_baserng, _) /*MAYTHROW*/);
						if(VERIFYNOTIFY(!tc::at_end_index(*this->m_baserng, idx2))) {
							auto const ch2=tc::dereference_index(*this->m_baserng, idx2); // MAYTHROW
							if(VERIFYNOTIFY(tc::is_continuation_codeunit(ch2))) {
								n-=0xd800u;
								_ASSERTDEBUG(n<0x400u);
								n<<=10;

								unsigned int n2=tc::underlying_cast(ch2)-0xdc00u;
								_ASSERTDEBUG(n2<0x400u);
								n+=n2+0x10000u;
								_ASSERTDEBUG(n<0x110000u);

								return true;
							}
						}
						return false;
					}
				); // MAYTHROW
			}
		};
	
		// Lazily convert UTF-8 strings to UTF-32
		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<char32_t, Rng, enable_if_range_of_t<char, Rng>>
			: SStringConversionToUtf32RangeBase<SStringConversionRange<char32_t, Rng>, Rng>
		{
		private:
			using this_type = SStringConversionRange<char32_t, Rng>;
			using base_ = SStringConversionToUtf32RangeBase<this_type, Rng>;

		public:
			constexpr explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
				: base_(aggregate_tag, std::forward<Rng>(rng))
			{}
			using typename base_::index;

		private:
			STATIC_FINAL_MOD(constexpr, dereference_index)(index const& idx) const& MAYTHROW {
				return base_::char_at(
					idx,
					[&](unsigned int& n, int nSequenceSize) MAYTHROW {
						n&=[&]() noexcept {
							switch_no_default(nSequenceSize) {
								case 2: return 0x1fu;
								case 3: return 0xfu;
								case 4: return 0x7u;
							}
						}();

						auto idx2=idx;
						int i=nSequenceSize;
						do {
							tc::increment_index(*this->m_baserng, idx2); // MAYTHROW
							if(!VERIFYNOTIFY(!tc::at_end_index(*this->m_baserng, idx2))) return false;
							auto const ch2=tc::dereference_index(*this->m_baserng, idx2); // MAYTHROW
							if(!VERIFYNOTIFY(tc::is_continuation_codeunit(ch2))) return false;

							n<<=6;
							n|=tc::underlying_cast(ch2) & 0x3fu;
						} while(1<--i);

						switch_no_default(nSequenceSize) {
							case 2: return VERIFYNOTIFY(0x80u<=n);
							case 3: return VERIFYNOTIFY(0x800u<=n) && VERIFYNOTIFY(n<0xd800u || 0xdfffu<n);
							case 4: return VERIFYNOTIFY(0x10000u<=n) && VERIFYNOTIFY(n<0x110000u);
						}
					}
				); // MAYTHROW
			}
		};

		template<typename Index>
		struct SCodeUnitIndex final {
			Index m_idx;
			int m_nCodeUnitIndex;
		};

		template<typename Derived, typename Rng>
		struct SStringConversionFromUtf32RangeBase
			: tc::range_iterator_generator_from_index<
				Derived,
				SCodeUnitIndex<tc::index_t<std::remove_reference_t<Rng>>>
			> 
		{
			explicit SStringConversionFromUtf32RangeBase(tc::aggregate_tag_t, Rng&& rng) noexcept
				: m_baserng(tc::aggregate_tag, std::forward<Rng>(rng))
			{}
			using index = typename SStringConversionFromUtf32RangeBase::index;

		private:
			using this_type = SStringConversionFromUtf32RangeBase;

		protected:
			STATIC_VIRTUAL_CONSTEXPR(codeunit_at)
			STATIC_VIRTUAL_CONSTEXPR(codepoint_last_index)

		private:
			reference_or_value<Rng> m_baserng;

			constexpr auto codepoint_at(decltype(index::m_idx) const& idxBaseRng) const& MAYTHROW {
				if(	auto const n=tc::underlying_cast(tc::dereference_index(*m_baserng, idxBaseRng)); // MAYTHROW
					VERIFYNOTIFY(n<0x110000u) && VERIFYNOTIFY(n<0xd800u || 0xdfffu<n) 
				) {
					return n;
				} else {
					return tc::underlying_cast(c_chReplacementCharacter);
				}
			}

			// As of Visual Studio compiler 19.15.26726, using return_ctor_MAYTHROW below triggers a C1001: internal compiler error
			STATIC_OVERRIDE_MOD(constexpr, begin_index)() const& noexcept(noexcept(tc::begin_index(m_baserng))) {
				return index{tc::begin_index(m_baserng) /*MAYTHROW*/, 0};
			}

			// As of Visual Studio compiler 19.15.26726, using return_ctor_MAYTHROW below triggers a C1001: internal compiler error
			STATIC_OVERRIDE_MOD(
				template<ENABLE_SFINAE> constexpr,
				end_index
			)() const& noexcept(noexcept(tc::end_index(tc::base_cast<SFINAE_TYPE(this_type)>(this)->m_baserng))) {
				return index{tc::end_index(m_baserng) /*MAYTHROW*/, 0};
			}

			STATIC_OVERRIDE_MOD(constexpr, at_end_index)(index const& idx) const& return_decltype_MAYTHROW(
				tc::at_end_index(*m_baserng, idx.m_idx) && (_ASSERTE(0==idx.m_nCodeUnitIndex), true)
			)

			STATIC_OVERRIDE_MOD(
				// delay instantiation of dereference_index, so that codeunit_at can be mentioned in its declaration (return_decltype_MAYTHROW) before the
				// Derived type is complete.
				template<typename index_=index> constexpr,
				dereference_index
			)(index_ const& idx) const& return_decltype_MAYTHROW(
				codeunit_at(codepoint_at(idx.m_idx) /*MAYTHROW*/, idx.m_nCodeUnitIndex)
			)

			STATIC_OVERRIDE_MOD(
				template<ENABLE_SFINAE> constexpr,
				equal_index
			)(SFINAE_TYPE(index) const& idxLhs, index const& idxRhs) const& return_decltype_noexcept(
				tc::equal_index(*m_baserng, idxLhs.m_idx, idxRhs.m_idx) && idxLhs.m_nCodeUnitIndex==idxRhs.m_nCodeUnitIndex
			)

			STATIC_OVERRIDE_MOD(constexpr, increment_index)(index& idx) const& MAYTHROW {
				if(idx.m_nCodeUnitIndex<codepoint_last_index(codepoint_at(idx.m_idx) /*MAYTHROW*/)) {
					++idx.m_nCodeUnitIndex;
				} else {
					idx.m_nCodeUnitIndex=0;
					tc::increment_index(*m_baserng, idx.m_idx); // MAYTHROW
				}
			}

			STATIC_OVERRIDE_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<tc::has_decrement_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value>* = nullptr
				> constexpr,
				decrement_index
			)(index& idx) const& MAYTHROW {
				if(0==idx.m_nCodeUnitIndex) {
					tc::decrement_index(*m_baserng, idx.m_idx); // MAYTHROW
					idx.m_nCodeUnitIndex=codepoint_last_index(codepoint_at(idx.m_idx) /*MAYTHROW*/);
				} else {
					--idx.m_nCodeUnitIndex;
				}
			}

			template<typename Self>
			static constexpr decltype(auto) base_range_(Self&& self) noexcept {
				return *std::forward<Self>(self).m_baserng;
			}

		public:
			constexpr auto border_base_index(index const& idx) const& noexcept {
				_ASSERTE(0==idx.m_nCodeUnitIndex);
				return idx.m_idx;
			}

			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(base_range)
		};

		// Lazily convert UTF-32 strings to UTF-16
		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<tc::char16, Rng, enable_if_range_of_t<char32_t, Rng>>
			: SStringConversionFromUtf32RangeBase<SStringConversionRange<tc::char16, Rng>, Rng>
		{
		private:
			using this_type = SStringConversionRange<tc::char16, Rng>;
			using base_ = SStringConversionFromUtf32RangeBase<this_type, Rng>;

		public:
			constexpr explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
				: base_(aggregate_tag, std::forward<Rng>(rng))
			{}

		private:
			STATIC_FINAL_MOD(constexpr, codeunit_at)(unsigned int nCodePoint, int nCodeUnitIndex) const& noexcept {
				_ASSERTDEBUG(nCodePoint<0x110000u);
				return tc::bit_cast<tc::char16>(tc::explicit_cast<std::uint16_t>([&]() noexcept {
					auto const nLastIndex=base_::codepoint_last_index(nCodePoint);
					switch_no_default(nCodeUnitIndex) {
						case 0:
							switch_no_default(nLastIndex) {
								case 0: return nCodePoint;
								case 1: return (nCodePoint-0x10000u >> 10)+0xd800u;
							}
						case 1:
							_ASSERTDEBUG(1==nLastIndex);
							return (nCodePoint-0x10000u & 0x3ffu)+0xdc00u;
					}
				}()));
			}

			STATIC_FINAL_MOD(constexpr, codepoint_last_index)(unsigned int nCodePoint) const& noexcept {
				return nCodePoint<0x10000u ? 0 : 1;
			}
		};

		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<char, Rng, enable_if_range_of_t<char32_t, Rng>>
			: SStringConversionFromUtf32RangeBase<SStringConversionRange<char, Rng>, Rng>
		{
		private:
			using this_type = SStringConversionRange<char, Rng>;
			using base_ = SStringConversionFromUtf32RangeBase<this_type, Rng>;

		public:
			constexpr explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
				: base_(aggregate_tag, std::forward<Rng>(rng))
			{}

		private:
			STATIC_FINAL_MOD(constexpr, codeunit_at)(unsigned int nCodePoint, int nCodeUnitIndex) const& noexcept {
				_ASSERTDEBUG(nCodePoint<0x110000u);
				return tc::bit_cast<char>(tc::explicit_cast<std::uint8_t>([&]() noexcept {
					auto const nLastIndex=base_::codepoint_last_index(nCodePoint);
					switch_no_default(nCodeUnitIndex) {
						case 0:
							return 0==nLastIndex
								? nCodePoint
								: nCodePoint>>nLastIndex*6 | ((1 << nLastIndex+1)-1)<<(7-nLastIndex);
						case 1:
						case 2:
						case 3:
							_ASSERTDEBUG(nCodeUnitIndex<=nLastIndex);
							return (nCodePoint>>(nLastIndex-nCodeUnitIndex)*6 & 0x3fu) | 0x80u;
					}
				}()));
			}

			STATIC_FINAL_MOD(constexpr, codepoint_last_index)(unsigned int nCodePoint) const& noexcept {
				return 
					nCodePoint<0x80u ? 0
					: nCodePoint<0x800u	? 1
					: nCodePoint<0x10000u ? 2 : 3;
			}
		};

		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<char, Rng, enable_if_range_of_t<tc::char16, Rng>> final
			: SStringConversionRange<char, SStringConversionRange<char32_t, Rng>>
		{
		private:
			using base_ = SStringConversionRange<char, SStringConversionRange<char32_t, Rng>>;

			template<typename Self>
			static constexpr decltype(auto) base_range_(Self&& self) noexcept {
				return std::forward<Self>(self).base_::base_range().base_range();
			}

		public:
			constexpr explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
				: base_(aggregate_tag, SStringConversionRange<char32_t, Rng>(aggregate_tag, std::forward<Rng>(rng)))
			{}
			using typename base_::index;

			constexpr auto border_base_index(index const& idx) const& return_decltype_noexcept(
				base_::base_range().border_base_index(base_::border_base_index(idx))
			)

			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(base_range)
		};

		template<typename Rng>
		struct [[nodiscard]] SStringConversionRange<tc::char16, Rng, enable_if_range_of_t<char, Rng>> final
			: SStringConversionRange<tc::char16, SStringConversionRange<char32_t, Rng>>
		{
		private:
			using base_ = SStringConversionRange<tc::char16, SStringConversionRange<char32_t, Rng>>;

			template<typename Self>
			static constexpr decltype(auto) base_range_(Self&& self) noexcept {
				return std::forward<Self>(self).base_::base_range().base_range();
			}

		public:
			constexpr explicit SStringConversionRange(aggregate_tag_t, Rng&& rng) noexcept
				: base_(aggregate_tag, SStringConversionRange<char32_t, Rng>(aggregate_tag, std::forward<Rng>(rng)))
			{}
			using typename base_::index;

			constexpr auto border_base_index(index const& idx) const& return_decltype_noexcept(
				base_::base_range().border_base_index(base_::border_base_index(idx))
			)

			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(base_range)
		};
	} // namespace convert_enc_impl

	//--------------------------------------------------------------------------------------------------------------------------
	// must_convert_enc
	// Converts range of char type to a lazy range of a different char type

	template< typename Dst, typename Src, std::enable_if_t<tc::is_char<Dst>::value>* = nullptr>
	[[nodiscard]] auto must_convert_enc(Src&& src) return_ctor_noexcept(
		convert_enc_impl::SStringConversionRange<Dst BOOST_PP_COMMA() Src>,
		(aggregate_tag, std::forward<Src>(src))
	)

	//--------------------------------------------------------------------------------------------------------------------------
	// may_convert_enc
	// Either forwards its argument (if it is a range of Dst), or converts it to a lazy range of Dst (if it is a range of another char type).

	template< typename Dst, typename Src, std::enable_if_t<!std::is_same<tc::range_value_t<Src>, Dst>::value >* = nullptr>
	[[nodiscard]] auto may_convert_enc( Src&& src ) return_decltype_noexcept(
		tc::must_convert_enc<Dst>(std::forward<Src>(src))
	)

	template< typename Dst, typename Src, std::enable_if_t<std::is_same<tc::range_value_t<Src>, Dst>::value >* = nullptr>
	[[nodiscard]] Src&& may_convert_enc( Src&& src ) noexcept {
		return std::forward<Src>( src );
	}
}

