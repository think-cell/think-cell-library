
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/accessors.h"
#include "base/bitfield.h"
#include "base/enum.h"
#include "base/integer.h"
#include "base/tag_type.h"
#include "algorithm/binary_operators.h"
#include "algorithm/compare.h"
#include "algorithm/element.h"
#include "algorithm/equal.h"
#include "algorithm/size.h"
#include "range/iota_range.h"
#include "range/transform.h"
#include "range/empty_range.h"
#include "interval_types.h"

namespace tc {
	DEFINE_TAG_TYPE(enumset_from_underlying_tag)
	DEFINE_TAG_TYPE(union_tag)

	namespace explicit_convert_adl {
		template<typename EnumSuper, typename EnumSub> requires tc::is_sub_enum_of<EnumSub, EnumSuper>::value
		constexpr tc::enumset<EnumSuper> explicit_convert_impl(adl_tag_t, std::type_identity<tc::enumset<EnumSuper>>, tc::enumset<EnumSub> const setesub) noexcept;

		template<typename EnumSub, typename EnumSuper> requires tc::is_sub_enum_of<EnumSub, EnumSuper>::value
		constexpr tc::enumset<EnumSub> explicit_convert_impl(adl_tag_t, std::type_identity<tc::enumset<EnumSub>>, tc::enumset<EnumSuper> const setesuper) noexcept;
	}

	namespace enumset_adl {
#ifdef TC_PRIVATE
		template< typename Enum >
		struct enumset;

		template<typename Enum>
		void LoadType_impl(enumset<Enum>& sete, CXmlReader& loadhandler) THROW(ExLoadFail);
#endif

		template< typename Enum >
		struct enumset /*final*/
			: tc::setlike<tc::index_range_adaptor<
				enumset<Enum>,
				tc::all_values<Enum>,
				tc::index_range_adaptor_flags::inherit_dereference|tc::index_range_adaptor_flags::inherit_end
			>>
		{
		private:
			using this_type = enumset;
			static constexpr tc::all_values<Enum> c_rnge{};
		public:
			using typename this_type::index_range_adaptor::tc_index;
			static constexpr bool c_bHasStashingIndex = tc::has_stashing_index<tc::all_values<Enum>>::value;

			friend tc::counting_iterator<enumset<Enum>>;
			template< typename OtherEnum > friend struct enumset;

			template<typename EnumSuper, typename EnumSub> requires tc::is_sub_enum_of<EnumSub, EnumSuper>::value
			friend constexpr tc::enumset<EnumSuper> tc::explicit_convert_adl::explicit_convert_impl(tc::explicit_convert_adl::adl_tag_t, std::type_identity<tc::enumset<EnumSuper>>, tc::enumset<EnumSub> const setesub) noexcept;

			template<typename EnumSub, typename EnumSuper> requires tc::is_sub_enum_of<EnumSub, EnumSuper>::value
			friend constexpr tc::enumset<EnumSub> tc::explicit_convert_adl::explicit_convert_impl(tc::explicit_convert_adl::adl_tag_t, std::type_identity<tc::enumset<EnumSub>>, tc::enumset<EnumSuper> const setesuper) noexcept;

#ifdef TC_PRIVATE
			void DoSave(CSaveHandler& savehandler) const& MAYTHROW;
			friend void LoadType_impl<>(enumset& sete, CXmlReader& loadhandler) THROW(ExLoadFail);
#endif

			using bitset_type = tc::uint_least_t<tc::size(c_rnge)>;
			PUBLIC_MEMBER_PUBLIC_ACCESSOR(bitset_type, m_bitset); // Necessarily public so enumset can be used as a template parameter.
		private:

			template<typename N>
			static constexpr bitset_type lsb_mask(N const nDigits) noexcept {
				if (0 == nDigits) {
					return 0;
				} else {
					_ASSERTE(0 < nDigits);
					return static_cast<bitset_type>(-1)>>(std::numeric_limits<bitset_type>::digits - nDigits);
				}
			}
			
			static constexpr tc_index make_index(int nIndex) noexcept {
				return tc_modified(tc::begin_index(c_rnge), tc::advance_index(c_rnge, _, tc::explicit_cast<typename boost::range_size<tc::all_values<Enum>>::type>(nIndex)));
			}

		public:
			static constexpr bitset_type mask() noexcept {
				return lsb_mask(tc::size(c_rnge));
			}
			
			constexpr enumset() noexcept : m_bitset(0) {} // makes all bits 0
			constexpr enumset(tc::empty_range) noexcept: enumset() {}
			constexpr enumset(tc::all_values<Enum>) noexcept : m_bitset(mask()) {}
			template<typename U>
			constexpr enumset(enumset_from_underlying_tag_t, U bitset) noexcept : tc_member_init( m_bitset, bitset ) {
				_ASSERTE( !(m_bitset&~mask()) );
			}
			constexpr enumset(Enum e) noexcept : enumset(enumset_from_underlying_tag, tc::explicit_cast<bitset_type>(1) << c_rnge.index_of(e)) {}
			template<ENABLE_SFINAE>
			constexpr enumset(tc::interval<SFINAE_TYPE(Enum)> const& intvle) noexcept
				: enumset(enumset_from_underlying_tag,
					(tc::explicit_cast<tc::uint_least_t<tc::size(c_rnge) + 1>>(1) << c_rnge.index_of(intvle[tc::hi]))
					- (tc::explicit_cast<tc::uint_least_t<tc::size(c_rnge) + 1>>(1) << c_rnge.index_of(intvle[tc::lo]))
				)
			{
				_ASSERTE( !intvle.empty_inclusive() );
			}

			template<typename Rng>
			constexpr enumset(tc::union_tag_t, Rng&& rng) MAYTHROW : m_bitset(0)
			{
				tc::for_each(tc_move_if_owned(rng), [&](enumset const& sete) noexcept { // MAYTHROW
					*this |= sete;
				});
			}
			template<typename Func>
			constexpr enumset(tc::func_tag_t, Func func) MAYTHROW : m_bitset(0) {
				// Could be implemented in terms of union_tag constructor and filter, but it wasn't to avoid dependency on filter.
				tc::for_each(c_rnge, [&](auto e) noexcept {
					if (tc::explicit_cast<bool>(func(tc::as_const(e)))) { // MAYTHROW
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

			constexpr enumset& operator-=( enumset const& sete ) & noexcept {
				m_bitset&=~sete.m_bitset;
				return *this;
			}

			friend constexpr bool operator==( enumset const& lhs, enumset const& rhs ) noexcept {
				return EQUAL_MEMBERS(m_bitset);
			}
			friend constexpr bool operator==( enumset const& lhs, Enum rhs ) noexcept {
				return lhs==enumset(rhs);
			}
			constexpr bool is_singleton() const& noexcept {
				//return std::has_single_bit(m_bitset);
				return 0!=m_bitset && 0==(m_bitset & (m_bitset - 1));
			}
	
			constexpr std::size_t size() const& noexcept {
				return std::popcount(m_bitset);
			}
			constexpr explicit operator bool() const& noexcept {
				return 0!=m_bitset;
			}

			static constexpr enumset none() noexcept {
				return enumset<Enum>();
			}

			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				return 0 == m_bitset ? this->end_index() : make_index(tc::index_of_least_significant_bit(m_bitset));
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept -> void {
				_ASSERT( idx != this->end_index() );
				bitset_type const bitsetRemaining = m_bitset & ~lsb_mask(tc::distance_to_index(c_rnge, tc::begin_index(c_rnge), idx) + 1);
				idx = 0 == bitsetRemaining ? this->end_index() : make_index(tc::index_of_least_significant_bit(bitsetRemaining));
			}

			STATIC_FINAL_MOD(constexpr, decrement_index)(tc_index& idx) const& noexcept -> void {
				_ASSERT( idx != this->begin_index() );
				idx = make_index(tc::index_of_most_significant_bit(tc::explicit_cast<unsigned long>(m_bitset & lsb_mask(tc::distance_to_index(c_rnge, tc::begin_index(c_rnge), idx)))));
			}
		};

		template<typename Enum> enumset(Enum) -> enumset<Enum>;
		template<typename Enum> enumset(all_values<Enum>) -> enumset<Enum>;
		template<typename Enum> enumset(interval<Enum>) -> enumset<Enum>;
	} // enumset_adl
	using enumset_adl::enumset;

	template<tc::actual_integer_like NSub, tc::actual_integer_like NSuper>
	[[nodiscard]] constexpr bool is_subset(NSub const nSub, NSuper const nSuper) noexcept {
		return !(tc::as_unsigned(nSub) & ~tc::as_unsigned(nSuper));
	}

	template<typename Enum>
	[[nodiscard]] constexpr bool is_subset(tc::enumset<Enum> const& seteSub, tc::enumset<Enum> const& seteSuper) noexcept {
		return !(seteSub & ~seteSuper);
	}

	template<typename Enum>
	[[nodiscard]] constexpr bool is_subset(Enum const eSub, tc::enumset<Enum> const& seteSuper) noexcept {
		return tc::is_subset(tc::enumset<Enum>(eSub), seteSuper);
	}

	template<typename Enum>
	[[nodiscard]] constexpr bool is_subset(tc::enumset<Enum> const& seteSub, Enum const eSuper) noexcept {
		return tc::is_subset(seteSub, tc::enumset<Enum>(eSuper));
	}

	namespace no_adl {
		template<typename Enum>
		struct [[nodiscard]] all_values<tc::enumset<Enum>> final {
		public:
			static constexpr auto size() noexcept {
				constexpr auto size_all_values_enum = tc::constexpr_size<tc::all_values<Enum>>();
				static_assert(size_all_values_enum < std::size_t(std::numeric_limits<std::size_t>::digits));
				return tc::least_uint_constant<std::size_t(1) << size_all_values_enum>{};
			}

		private:
			static auto constexpr c_rngsete = tc::transform(tc::iota_range_constant<static_cast<decltype(size().value)>(0), size().value>(), [](auto const n) noexcept {
				return tc::enumset<Enum>(tc::enumset_from_underlying_tag, n);
			});

		public:
			static constexpr auto begin() noexcept {
				return tc::begin(c_rngsete);
			}
			static constexpr auto end() noexcept {
				return tc::end(c_rngsete);
			}

			static constexpr std::size_t index_of(tc::enumset<Enum> const& sete) noexcept {
				return sete.m_bitset_();
			}
		};
	}

	namespace explicit_convert_adl {
		template<typename EnumSuper, typename EnumSub> requires tc::is_sub_enum_of<EnumSub, EnumSuper>::value
		constexpr tc::enumset<EnumSuper> explicit_convert_impl(adl_tag_t, std::type_identity<tc::enumset<EnumSuper>>, tc::enumset<EnumSub> const setesub) noexcept {
			return tc::enumset<EnumSuper>(
				tc::enumset_from_underlying_tag,
				tc::explicit_cast<typename tc::enumset<EnumSuper>::bitset_type>(setesub.m_bitset_()) << tc::all_values<EnumSuper>::index_of(tc::explicit_cast<EnumSuper>(tc::contiguous_enum<EnumSub>::begin()))
			);
		}

		template<typename EnumSub, typename EnumSuper> requires tc::is_sub_enum_of<EnumSub, EnumSuper>::value
		constexpr tc::enumset<EnumSub> explicit_convert_impl(adl_tag_t, std::type_identity<tc::enumset<EnumSub>>, tc::enumset<EnumSuper> const setesuper) noexcept {
			_ASSERTE(tc::is_subset(setesuper, tc::explicit_cast<tc::enumset<EnumSuper>>(tc::enumset(tc::all_values<EnumSub>()))));
			return tc::enumset<EnumSub>(
				tc::enumset_from_underlying_tag,
				setesuper.m_bitset_() >> tc::all_values<EnumSuper>::index_of(tc::explicit_cast<EnumSuper>(tc::contiguous_enum<EnumSub>::begin()))
			);
		}
	}
}
