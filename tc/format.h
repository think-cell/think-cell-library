
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "range_defines.h"
#include "explicit_cast.h"
#include "for_each.h"
#include "subrange.h"
#include "bit_cast.h"
#include "empty.h"
#include "minmax.h"
#include "concat_adaptor.h"
#include "repeat_n.h"

namespace tc {
	///////////////
	// Wrapper to print integers as decimal

	namespace no_adl {
		template< typename T, std::size_t N>
		struct integral_as_padded_dec_impl;

		template< typename T>
		struct integral_as_padded_dec_impl<T,1>;

		template< typename T, std::size_t N>
		struct [[nodiscard]] integral_as_padded_dec_impl : protected integral_as_padded_dec_impl<T,N-1> {
			static constexpr unsigned long long c_nTenPow=integral_as_padded_dec_impl<T,N-1>::c_nTenPow*10;
			constexpr integral_as_padded_dec_impl( T n ) noexcept : integral_as_padded_dec_impl<T,N-1>(n) {}

			template<typename Sink, typename Char=tc::sink_value_or_char_t<Sink>>
			auto operator()(Sink sink) const& MAYTHROW -> tc::common_type_t<
				decltype(tc::continue_if_not_break(std::declval<Sink&>(), std::declval<Char>())),
				decltype(std::declval<integral_as_padded_dec_impl<T,N-1> const&>()(std::declval<Sink>()))
			> {
				static_assert( std::is_unsigned<T>::value );
				if( this->m_n<integral_as_padded_dec_impl::c_nTenPow ) {
					RETURN_IF_BREAK(tc::continue_if_not_break(sink, tc::explicit_cast<Char>('0')));
				}
				return tc::base_cast< integral_as_padded_dec_impl<T,N-1> >(*this)(tc_move(sink));
			}

			constexpr bool empty() const& noexcept { return false; }
		};

		template< typename T>
		struct [[nodiscard]] integral_as_padded_dec_impl<T,1> {
			T m_n;
			static constexpr unsigned long long c_nTenPow=1;
			constexpr integral_as_padded_dec_impl( T n ) noexcept : m_n(n) {}

			template<typename Sink>
			auto operator()(Sink&& sink) const& MAYTHROW {
				return tc::for_each(tc::ptr_begin( boost::lexical_cast< std::array<tc::sink_value_or_char_t<Sink>,50> >(m_n+0/*force integral promotion, otherwise unsigned/signed char gets printed as character*/) ), std::forward<Sink>(sink));
			}

			constexpr bool empty() const& noexcept { return false; }
		};
	}

	template< typename T, std::enable_if_t<tc::is_actual_integer<T>::value>* = nullptr >
	constexpr auto as_dec(T t) return_ctor_noexcept(
		no_adl::integral_as_padded_dec_impl<T BOOST_PP_COMMA() 1>,
		(t)
	)

	template< typename T , std::enable_if_t<std::is_class<T>::value>* = nullptr>
	constexpr auto as_dec(T const& t) return_decltype_noexcept(
		tc::as_dec(ConvertToUnderlying(t) )
	)

	template< std::size_t N, typename T, std::enable_if_t<tc::is_actual_integer<T>::value>* = nullptr >
	constexpr auto as_padded_dec(T t) return_ctor_noexcept(
		no_adl::integral_as_padded_dec_impl<std::make_unsigned_t<T> BOOST_PP_COMMA() N>,
		(tc::unsigned_cast(t))
	)

	template< std::size_t N, typename T, std::enable_if_t<std::is_class<T>::value>* = nullptr >
	constexpr auto as_padded_dec(T const& t) return_decltype_noexcept(
		tc::as_padded_dec<N>(ConvertToUnderlying(t) )
	)

	namespace no_adl {
		///////////////
		// Wrapper to print integers as hex
		template< typename T, unsigned int nWidth, char c_chLetterBase>
		struct [[nodiscard]] as_hex_impl final {
		private:
			typename boost::uint_t< CHAR_BIT*sizeof(T) >::exact m_n;
		public:
			as_hex_impl( T const& n ) noexcept : m_n(tc::bit_cast< typename boost::uint_t< CHAR_BIT*sizeof(T) >::exact >(n)) {} // print the bit pattern of anything we get

			template<typename Sink, typename Char=tc::sink_value_or_char_t<Sink>>
			auto operator()(Sink sink) const& MAYTHROW -> decltype(tc::continue_if_not_break(std::declval<Sink&>(), std::declval<Char>())) {
				static_assert( 0<nWidth );
				static_assert( nWidth<=(sizeof(m_n)*CHAR_BIT+3)/4 );

				auto nShift=sizeof(m_n)*CHAR_BIT;
				do {
					nShift-=4;
				} while( nWidth*4<=nShift && 0==(m_n>>nShift) );
				for(;;) {
					auto const nDigit=(m_n>>nShift)&0xf;
					RETURN_IF_BREAK(tc::continue_if_not_break(sink, tc::explicit_cast<Char>(static_cast<char>(nDigit<10 ? '0'+nDigit : c_chLetterBase+(nDigit-10)))));
					if( 0==nShift ) break;
					nShift-=4;
				}
				// TODO: if continue_if_not_break above returns INTEGRAL_CONSTANT(tc::break_), must exclude this from compilation:
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		};
	}
	using no_adl::as_hex_impl;

	template< unsigned int nWidth, typename T >
	auto as_uc_hex(T const& t) return_ctor_noexcept(
		as_hex_impl<T BOOST_PP_COMMA() nWidth BOOST_PP_COMMA() 'A'>,
		(t)
	)

	template< typename T >
	auto as_padded_uc_hex(T const& t) return_ctor_noexcept(
		as_hex_impl<T BOOST_PP_COMMA() (sizeof(T)*CHAR_BIT+3)/4 BOOST_PP_COMMA() 'A'>,
		(t)
	)

	//Do not use in XML, because the standard wants hexBinary to be padded to an even length
	template< typename T >
	auto as_unpadded_uc_hex(T const& t) return_ctor_noexcept(
		as_hex_impl<T BOOST_PP_COMMA() 1 BOOST_PP_COMMA() 'A'>,
		(t)
	)

	template< unsigned int nWidth, typename T >
	auto as_lc_hex(T const& t) return_ctor_noexcept(
		as_hex_impl<T BOOST_PP_COMMA() nWidth BOOST_PP_COMMA() 'a'>,
		(t)
	)

	template< typename T >
	auto as_padded_lc_hex(T const& t) return_ctor_noexcept(
		as_hex_impl<T BOOST_PP_COMMA() (sizeof(T)*CHAR_BIT+3)/4 BOOST_PP_COMMA() 'a'>,
		(t)
	)

	//Do not use in XML, because the standard wants hexBinary to be padded to an even length
	template< typename T >
	auto as_unpadded_lc_hex(T const& t) return_ctor_noexcept(
		as_hex_impl<T BOOST_PP_COMMA() 1 BOOST_PP_COMMA() 'a'>,
		(t)
	)

	//////////////////////////////////////////////////
	// conversion from string to number

	template< typename T, typename Rng >
	auto unsigned_integer_from_string_head(Rng&& rng) noexcept {
		auto pairnit=std::make_pair(tc::explicit_cast<T>(0),tc::begin(rng));
		auto const itEnd=tc::end(rng);
		while( pairnit.second!=itEnd ) {
			unsigned int const nDigit=*pairnit.second-tc::explicit_cast<tc::range_value_t<Rng>>('0');
			if( 9<nDigit || (std::numeric_limits<T>::max()-static_cast<int>(nDigit))/10<pairnit.first ) break; // overflow
			pairnit.first*=10;
#pragma warning(push)
#pragma warning(disable:4244) // conversion from 'const unsigned int' to 'uint16_t', possible loss of data
			pairnit.first+=nDigit;
#pragma warning(pop)
			++pairnit.second;
		}
		return pairnit;
	}

	template< typename T, typename Rng >
	auto signed_integer_from_string_head(Rng&& rng) noexcept {
		auto pairnit=std::make_pair(tc::explicit_cast<T>(0),tc::begin(rng));
		auto const itEnd=tc::end(rng);
		if( pairnit.second!=itEnd ) {
			if (tc::explicit_cast<tc::range_value_t<Rng>>('-') == *pairnit.second) {
				++pairnit.second;
				while (pairnit.second != itEnd) {
					unsigned int const nDigit = *pairnit.second - tc::explicit_cast<tc::range_value_t<Rng>>('0');
					if (9 < nDigit || pairnit.first < (std::numeric_limits<T>::lowest() + static_cast<int>(nDigit)) / 10) break; // underflow
					pairnit.first *= 10;
#pragma warning(push)
#pragma warning(disable:4244) // conversion from 'const unsigned int' to 'uint16_t', possible loss of data
					pairnit.first -= nDigit;
#pragma warning(pop)
					++pairnit.second;
				}
			} else if (tc::explicit_cast<tc::range_value_t<Rng>>('+') == *pairnit.second) {
				pairnit = unsigned_integer_from_string_head<T>(tc::drop_first(rng));
			} else {
				pairnit = unsigned_integer_from_string_head<T>(rng);
			}
		}
		return pairnit;
	}

	struct integer_parse_exception final {};

	template< typename T, typename Rng >
	T signed_integer_from_string( Rng const& rng ) THROW(tc::integer_parse_exception) {
		if (tc::empty(rng)) throw tc::integer_parse_exception();
		auto pairnit=tc::signed_integer_from_string_head<T>(rng);
		if( pairnit.second!=tc::end(rng) ) throw tc::integer_parse_exception();
		return pairnit.first;
	}

	template< typename T, typename Rng >
	T unsigned_integer_from_string( Rng const& rng ) THROW(tc::integer_parse_exception) {
		if (tc::empty(rng)) throw tc::integer_parse_exception();
		auto pairnit=tc::unsigned_integer_from_string_head<T>(rng);
		if( pairnit.second!=tc::end(rng) ) throw tc::integer_parse_exception();
		return pairnit.first;
	}

	namespace no_adl {
		template<typename Rng>
		struct [[nodiscard]] size_prefixed_impl {
			using value_type=unsigned char;

			template<typename Rhs>
			size_prefixed_impl(aggregate_tag_t, Rhs&& rhs) noexcept
				: m_rng(aggregate_tag, std::forward<Rhs>(rhs))
			{}

			template<typename Sink>
			void operator()(Sink&& sink) const& MAYTHROW {
				STATICASSERTSAME(tc::sink_value_t<Sink>, unsigned char, "size_prefixed should only be used on binary sinks.");
				tc::for_each(tc::concat(tc::as_blob(tc::implicit_cast<std::uint32_t>(tc::size(*m_rng))), tc::range_as_blob(*m_rng)), std::forward<Sink>(sink)); // THROW(tc::file_failure)
			}
		private:
			tc::reference_or_value<Rng> m_rng;
		};
	}

	template< typename Rng >
	auto size_prefixed(Rng&& rng) return_ctor_noexcept(
		no_adl::size_prefixed_impl<Rng>,
		(aggregate_tag, std::forward<Rng>(rng))
	)

	inline auto size_prefixed(tc::empty_range) noexcept {
		static constexpr std::uint32_t nSize=0;
		return tc::as_blob(nSize);
	}

	namespace no_adl {
		template<typename T>
		struct [[nodiscard]] bool_prefixed_impl {
			template<typename Rhs>
			bool_prefixed_impl(aggregate_tag_t, Rhs&& rhs) noexcept
				: m_ot(aggregate_tag, std::forward<Rhs>(rhs))
			{}

			template<typename Sink>
			void operator()(Sink&& sink) const& MAYTHROW {
				STATICASSERTSAME(tc::sink_value_t<Sink>, unsigned char, "bool_prefixed should only be used on binary sinks.");
				if(*m_ot) {
					tc::for_each(tc::concat(tc::as_blob(true), tc::as_blob(**m_ot)), std::forward<Sink>(sink)); // THROW(tc::file_failure)
				} else {
					tc::for_each(tc::as_blob(false), std::forward<Sink>(sink)); // THROW(tc::file_failure)
				}
			}
		private:
			tc::reference_or_value<T> m_ot;
		};
	}

	template< typename T, std::enable_if_t<tc::is_instance<std::optional, T>::value>* = nullptr >
	auto bool_prefixed(T&& t) return_ctor_noexcept(
		no_adl::bool_prefixed_impl<T>,
		(aggregate_tag, std::forward<T>(t))
	)
}
