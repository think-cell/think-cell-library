
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/enum.h"
#include "base/inplace.h"
#include "base/modified.h"

#ifdef TC_PRIVATE
#include "Library/Persistence/SaveableTraits.h"
#endif

//////////////////////////////////////
// EAlign

// persistent
TC_DEFINE_ENUM_WITH_OFFSET(EAlign, ealign, -1,
	(LOW)
	(CENTER)
	(HIGH)
)

namespace EAlign_adl {
	[[nodiscard]] constexpr EAlign operator-(EAlign const ealign) noexcept {
		return ealignLOW+(ealignHIGH-ealign);
	}
}

namespace tc {
	TC_DEFINE_UNPREFIXED_ENUM(lohi, (lo)(hi))

	namespace lohi_adl {
		// support for multiplication with tc::sign
		[[nodiscard]] constexpr lohi operator-(lohi lohi_) noexcept {
			return ~lohi_;
		}
	}

	[[nodiscard]] constexpr EAlign lohi_to_ealign(lohi lohi_) noexcept {
		if( lohi::lo == lohi_ ) {
			return ealignLOW;
		} else {
			return ealignHIGH;
		}
	}

	template< typename T >
	[[nodiscard]] constexpr T negate_if( bool_context b, T t ) noexcept {
		if(b) -tc::inplace(t);
		return t;
	}
	template< typename T >
	[[nodiscard]] constexpr T not_if( bool_context b, T t ) noexcept {
		if(b) ~tc::inplace(t);
		return t;
	}

	namespace interval_adl {
		template< typename T > struct interval;
	}
	using interval_adl::interval;

	TC_DEFINE_SCOPED_ENUM(sign,/*no prefix*/,(neg)(pos))

	namespace sign_adl {
		// support for multiplication with tc::sign
		[[nodiscard]] constexpr sign operator-(sign sign_) noexcept {
			return ~sign_;
		}

		template<typename T>
		constexpr T& operator*=(T& t, sign sign_) noexcept {
			if( sign::neg==tc::verify_not_end(sign_) ){
				-tc::inplace(t);
			}
			return t;
		}

		template<typename T>
		[[nodiscard]] constexpr tc::decay_t<T> operator*(T&& t, sign sign_) noexcept {
			return tc_modified( tc_move_if_owned(t), _ *= sign_ );
		}

		template<tc::decayed T>
		[[nodiscard]] constexpr T&& operator*(T&& t, sign sign_) noexcept {
			t *= sign_;
			return tc_move(t);
		}
	}

	namespace no_adl {
		// cannot be implemented as a lambda because lambdas are not assignable
		template<typename Func>
		struct [[nodiscard]] directed {
		private:
			static_assert(tc::decayed<Func>);
			Func m_func;
			tc::sign m_sign;

		public:
			template<typename FuncArg>
			constexpr directed(FuncArg&& func, tc::sign sign) noexcept
				: m_func(tc_move_if_owned(func))
				, m_sign(sign)
			{}

			template<typename Lhs, typename Rhs>
			constexpr auto operator()(Lhs && lhs, Rhs && rhs) const& return_decltype_MAYTHROW(
				tc::sign::neg==m_sign
				?	m_func( tc_move_if_owned(rhs), tc_move_if_owned(lhs) )
				:	m_func(tc_move_if_owned(lhs), tc_move_if_owned(rhs) )
			)
		};

		template<typename Func>
		directed(Func&&, tc::sign) -> directed<tc::decay_t<Func>>;
	}
	using no_adl::directed;
}

#ifdef TC_PRIVATE
namespace tc::sign_adl {
	// persistence of old EDirection was -1/1, which we must stay compatible with
	void LoadType_impl(sign& t, CXmlReader& loadhandler) THROW(ExLoadFail);
}

namespace no_adl {
	template<>
	struct SaveableTraits<tc::sign> final {
		static void DoSave(tc::sign t, CSaveHandler& savehandler) MAYTHROW;
	};
}
#endif
