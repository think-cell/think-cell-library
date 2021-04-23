
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "explicit_cast.h"
#include "construction_restrictiveness.h"

//-----------------------------------------------------------------------------------------------------------------------------

namespace tc {
	/////////////////////////////////////////////
	// derivable_t

	namespace no_adl {
		template<typename T>
		struct derivable_wrapper {
			STATICASSERTSAME( tc::remove_cvref_t<T>, T );
			static_assert( !std::is_class<T>::value );

			derivable_wrapper() noexcept
			{}

			template<typename A1, std::enable_if_t<tc::econstructionIMPLICIT==tc::construction_restrictiveness<T, A1&&>::value>* = nullptr>
			derivable_wrapper(A1&& a1) noexcept
				: m_t(std::forward<A1>(a1))
			{}

			template<typename A1, std::enable_if_t<tc::econstructionEXPLICIT==tc::construction_restrictiveness<T, A1&&>::value>* = nullptr>
			explicit derivable_wrapper(A1&& a1) noexcept
				: MEMBER_INIT_CAST( m_t, std::forward<A1>(a1) )
			{}

			operator T const&() const& noexcept {
				return m_t;
			}

			operator T&() & noexcept {
				return m_t;
			}

			operator T const&&() const&& noexcept {
				return static_cast<T const&&>(m_t);
			}

			operator T&&() && noexcept {
				return static_cast<T&&>(m_t);
			}

		private:
			T m_t;
		};

		template<>
		struct derivable_wrapper<void> {};
	}
	template<typename T>
	using derivable_t = std::conditional_t<std::is_class<T>::value, T, tc::no_adl::derivable_wrapper<T>>;

	#pragma push_macro("BASE_CAST_IMPL")
	#define BASE_CAST_IMPL(cvref) \
	template<typename Dst, std::enable_if_t<!std::is_class<Dst>::value>* = nullptr> \
	[[nodiscard]] constexpr Dst cvref base_cast(typename boost::mpl::identity<tc::derivable_t<Dst>>::type cvref t) noexcept { \
		STATICASSERTSAME(tc::remove_cvref_t<Dst>, Dst); \
		return static_cast<Dst cvref>(t); \
	}
	BASE_CAST_IMPL(&)
	BASE_CAST_IMPL(&&)
	BASE_CAST_IMPL(const&)
	BASE_CAST_IMPL(const&&)
	BASE_CAST_IMPL(volatile&)
	BASE_CAST_IMPL(volatile&&)
	BASE_CAST_IMPL(volatile const&)
	BASE_CAST_IMPL(volatile const&&)
	#pragma pop_macro("BASE_CAST_IMPL")	

	#pragma push_macro("BASE_CAST_IMPL")
	#define BASE_CAST_IMPL(cvref) \
	template<typename Dst, std::enable_if_t<!std::is_class<Dst>::value>* = nullptr> \
	[[nodiscard]] constexpr Dst cvref base_cast(typename boost::mpl::identity<tc::derivable_t<Dst>>::type cvref p) noexcept { \
		STATICASSERTSAME(tc::remove_cvref_t<Dst>, Dst); \
		return std::addressof(tc::base_cast<Dst>(*p)); \
	}
	BASE_CAST_IMPL(*)
	BASE_CAST_IMPL(const*)
	BASE_CAST_IMPL(volatile*)
	BASE_CAST_IMPL(volatile const*)
	#pragma pop_macro("BASE_CAST_IMPL")
}
