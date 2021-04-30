
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "accessors.h"
#include "binary_operators.h"
#include "bitfield.h"
#include "counting_range.h"
#include "enum.h"
#include "equality_comparable.h"
#include "equal.h"
#include "integer.h"
#include "interval_types.h"
#include "range_adaptor.h"
#include "size.h"
#include "tag_type.h"
#include "transform.h"
#include "empty_range.h"

namespace tc {
	DEFINE_TAG_TYPE(enumset_all_set_tag)
	DEFINE_TAG_TYPE(enumset_from_underlying_tag)

	namespace enumset_adl {
#ifdef TC_PRIVATE
		template< typename Enum >
		struct enumset;

		template< typename Enum >
		auto debug_output_impl(enumset<Enum> const& sete) noexcept;

		template<typename Enum>
		void LoadType_impl(enumset<Enum>& sete, CXmlReader& loadhandler) THROW(ExLoadFail);
#endif

		template< typename Enum >
		struct enumset /*final*/
			: tc::setlike< tc::equality_comparable<enumset<Enum>> >
			, tc::range_iterator_from_index< enumset<Enum>, typename tc::all_values<Enum>::iterator >
		{
		private:
			using this_type = enumset;
		public:
			using typename this_type::range_iterator_from_index::index;
			static constexpr bool c_bHasStashingIndex = false;

			friend tc::counting_iterator<enumset<Enum>>;
			template< typename OtherEnum > friend struct enumset;

#ifdef TC_PRIVATE
			void DoSave(CSaveHandler& savehandler) const& MAYTHROW;
			friend void LoadType_impl<>(enumset& sete, CXmlReader& loadhandler) THROW(ExLoadFail);
#endif
		private:
			using bitset_type = typename tc::integer<tc::constexpr_size<tc::all_values<Enum>>::value>::unsigned_;
			DEFINE_MEMBER_AND_ACCESSORS(bitset_type, m_bitset);
	
			static constexpr bitset_type lsb_mask(int nDigits) noexcept {
				if (0 == nDigits) {
					return 0;
				} else {
					_ASSERTE(0 < nDigits);
					return static_cast<bitset_type>(-1)>>(std::numeric_limits<bitset_type>::digits - nDigits);
				}
			}
			static constexpr bitset_type mask() noexcept {
				return lsb_mask(tc::constexpr_size<tc::all_values<Enum>>::value);
			}
			static index make_index(unsigned long nIndex) {
				return tc::at<tc::return_element>(tc::all_values<Enum>(), tc::explicit_cast<typename boost::range_size<tc::all_values<Enum>>::type>(nIndex));
			}

		public:
			constexpr enumset() noexcept : m_bitset(0) {} // makes all bits 0
			constexpr enumset(tc::empty_range) noexcept: enumset() {}
			constexpr enumset(enumset_all_set_tag_t) noexcept : m_bitset(mask()) {}
			template<typename U>
			constexpr enumset(enumset_from_underlying_tag_t, U bitset) noexcept : MEMBER_INIT_CAST( m_bitset, bitset ) {
				_ASSERTE( !(m_bitset&~mask()) );
			}
			constexpr enumset(Enum e) noexcept : enumset(enumset_from_underlying_tag, tc::explicit_cast<bitset_type>(1) << tc::all_values<Enum>::index_of(e)) {}
			template<ENABLE_SFINAE>
			constexpr enumset(tc::interval<SFINAE_TYPE(Enum)> const& intvle) noexcept
				: enumset(enumset_from_underlying_tag,
					(tc::explicit_cast<typename tc::integer<tc::constexpr_size<tc::all_values<Enum>>::value + 1>::unsigned_>(1) << tc::all_values<Enum>::index_of(intvle[tc::hi]))
					- (tc::explicit_cast<typename tc::integer<tc::constexpr_size<tc::all_values<Enum>>::value + 1>::unsigned_>(1) << tc::all_values<Enum>::index_of(intvle[tc::lo]))
				)
			{
				_ASSERTE( !intvle.empty_inclusive() );
			}

			template<typename OtherEnum>
			constexpr enumset( enumset<OtherEnum> const& sete ) noexcept : enumset(enumset_from_underlying_tag, sete.m_bitset) {
				static_assert( tc::starts_with<tc::return_bool>(tc::all_values<Enum>(), tc::transform(tc::all_values<OtherEnum>(), TC_FN(tc::explicit_cast<Enum>))) );
			}
			template<typename Func>
			constexpr enumset(tc::func_tag_t, Func func) MAYTHROW : m_bitset(0) {
				tc::for_each(tc::all_values<Enum>(), [&](auto e) noexcept {
					if (tc::bool_cast(func(tc::as_const(e)))) { // MAYTHROW
						*this |= e;
					}
				});
			}
			constexpr void bitwise_not() & noexcept {
				m_bitset^=mask();
			}
			// operators
			constexpr enumset operator~() const& noexcept {
				enumset copy=*this;
				copy.bitwise_not();
				return copy;
			}
			constexpr enumset& operator&=( enumset const& sete ) & noexcept {
				m_bitset&=sete.m_bitset;
				return *this;
			}
			constexpr enumset& operator|=( enumset const& sete ) & noexcept {
				m_bitset|=sete.m_bitset;
				return *this;
			}
			constexpr enumset& operator^=( enumset const& sete ) & noexcept {
				m_bitset^=sete.m_bitset;
				return *this;
			}
			friend constexpr bool operator==( enumset const& lhs, enumset const& rhs ) noexcept {
				return EQUAL_MEMBERS(m_bitset);
			}
			friend constexpr bool operator==( enumset const& lhs, Enum rhs ) noexcept {
				return lhs==enumset(rhs);
			}
			bool is_singleton() const& noexcept {
				return 0!=m_bitset && 0==(m_bitset & (m_bitset - 1));
			}
	
			template<typename T=bitset_type,
				std::enable_if_t<std::numeric_limits<T>::digits<=std::numeric_limits<unsigned long long>::digits>* = nullptr>
			std::size_t size() const& noexcept {
				return std::bitset<tc::constexpr_size<tc::all_values<Enum>>::value>(m_bitset).count();
			}
			constexpr explicit operator bool() const& noexcept {
				return 0!=m_bitset;
			}
#ifdef TC_PRIVATE
			friend auto debug_output_impl<>(enumset<Enum> const& sete) noexcept;
#endif
			static constexpr enumset none() noexcept {
				return enumset<Enum>();
			}
			static constexpr enumset all() noexcept {
				return enumset<Enum>(enumset_all_set_tag);
			}

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return 0 == m_bitset ? this->end_index() : make_index(tc::index_of_least_significant_bit(tc::explicit_cast<unsigned long>(m_bitset)));
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return tc::all_values<Enum>::end();
			}

			STATIC_FINAL(dereference_index)(index it) const& noexcept -> Enum {
				_ASSERT( it != this->end_index() );
				return *it;
			}

			STATIC_FINAL(increment_index)(index& it) const& noexcept -> void {
				static_assert(tc::constexpr_size<tc::all_values<Enum>>::value <= std::numeric_limits<unsigned long>::digits);
				_ASSERT( it != this->end_index() );

				auto_cref(bitsetRemaining, m_bitset & ~lsb_mask(it - tc::all_values<Enum>::begin() + 1));
				it = 0 == bitsetRemaining ? this->end_index() : make_index(tc::index_of_least_significant_bit(tc::explicit_cast<unsigned long>(bitsetRemaining)));
			}

			STATIC_FINAL(decrement_index)(index& it) const& noexcept -> void {
				static_assert(tc::constexpr_size<tc::all_values<Enum>>::value <= std::numeric_limits<unsigned long>::digits);
				_ASSERT( it != this->begin_index() );

				it = make_index(tc::index_of_most_significant_bit(tc::explicit_cast<unsigned long>(m_bitset & lsb_mask(it - tc::all_values<Enum>::begin()))));
			}
		};
	} // enumset_adl
	using enumset_adl::enumset;

	template<typename Enum>
	constexpr bool is_subset(tc::enumset<Enum> const& seteSub, tc::enumset<Enum> const& seteSuper) noexcept {
		return !(seteSub & ~seteSuper);
	}

	template<typename Enum>
	constexpr bool is_subset(Enum eSub, tc::enumset<Enum> const& seteSuper) noexcept {
		return tc::is_subset(tc::enumset<Enum>(eSub), seteSuper);
	}

	template<typename Enum>
	constexpr bool is_subset(tc::enumset<Enum> const& seteSub, Enum eSuper) noexcept {
		return tc::is_subset(seteSub, tc::enumset<Enum>(eSuper));
	}

	namespace no_adl {
		template<typename Enum, bool bConst>
		struct range_reference_base_with_const<enumset<Enum>, bConst> {
			using type=Enum;
		};
	}

	namespace no_adl {
		template<typename Enum>
		struct [[nodiscard]] all_values<tc::enumset<Enum>> final {
		private:
			struct fn_make_enumset final { // Not inline, because MSVC 19.15 complains, when using lambda instead.
				template<typename N>
				constexpr tc::enumset<Enum> operator()(N n) const& noexcept {
					return {tc::enumset_from_underlying_tag, n};
				}
			};

			static_assert(tc::constexpr_size<tc::all_values<Enum>>::value < std::numeric_limits<std::size_t>::digits);
			using storage_type = typename tc::integer<tc::constexpr_size<tc::all_values<Enum>>::value + 1>::unsigned_;
			friend constexpr_size_base<all_values<tc::enumset<Enum>>>;
			static auto constexpr c_nSize = tc::explicit_cast<storage_type>(1) << tc::constexpr_size<tc::all_values<Enum>>::value;
			static auto constexpr c_rngsete = tc::transform(tc::iota(tc::explicit_cast<storage_type>(0), c_nSize), fn_make_enumset());

		public:
			static constexpr auto begin() noexcept {
				return tc::begin(c_rngsete);
			}
			static constexpr auto end() noexcept {
				return tc::end(c_rngsete);
			}

			using const_iterator = decltype(all_values::begin());
			using iterator = const_iterator;

			static constexpr std::size_t index_of(tc::enumset<Enum> const& eset) noexcept {
				return eset.m_bitset_();
			}
		};

		template<typename Enum>
		struct constexpr_size_base<all_values<tc::enumset<Enum>>> : std::integral_constant<std::size_t, tc::all_values<tc::enumset<Enum>>::c_nSize> {};
	}
}
