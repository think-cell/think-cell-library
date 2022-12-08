
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "explicit_cast_fwd.h"
#include "casts.h"
#include "noncopyable.h"
#include "../optional.h"
#include "../tuple.h"
#include "../container/string.h"
#include <type_traits>

namespace tc {
	DEFINE_FN2_TMPL( explicit_cast, (typename) )

	namespace explicit_convert_adl {
		template<typename TTarget, typename TSource> requires tc::is_char<TTarget>::value && tc::is_char<TSource>::value
		constexpr TTarget explicit_convert_impl(adl_tag_t, tc::type::identity<TTarget>, TSource src) noexcept {
			static_assert(tc::decayed<TTarget>);
			_ASSERTE(tc::char_in_range<TSource>(tc::underlying_cast(src)));
			_ASSERTE(tc::char_in_range<TTarget>(tc::underlying_cast(src)));
			return static_cast<TTarget>(src);
		}

		template<typename TTarget, typename TSource> requires tc::is_actual_integer<TTarget>::value && std::is_floating_point<TSource>::value
		TTarget explicit_convert_impl(adl_tag_t, tc::type::identity<TTarget>, TSource src) noexcept {
			TTarget target=static_cast<TTarget>(src);
			_ASSERTDEBUGEQUAL( target,src ); // default round-to-zero from floating point to integer is wrong most of the time, so we force rounding first
			return target;
		}

		template<typename TTarget, typename TSource> requires std::is_floating_point<TTarget>::value && tc::is_actual_integer<TSource>::value
		constexpr TTarget explicit_convert_impl(adl_tag_t, tc::type::identity<TTarget>, TSource src) noexcept {
			return static_cast<TTarget>(src); // silence warning for int to float conversion
		}

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-compare"
#else
MODIFY_WARNINGS_BEGIN(
	((disable)(4018)) // signed/unsigned mismatch
	((disable)(4388)) // signed/unsigned mismatch
)
#endif
		template<typename TTarget, typename TSource> requires tc::is_actual_integer<TTarget>::value && tc::is_actual_integer<TSource>::value
		constexpr TTarget explicit_convert_impl(adl_tag_t, tc::type::identity<TTarget>, TSource src) noexcept {
			_ASSERTE(
				(
					!std::is_signed<TSource>::value ||
					( std::is_signed<TTarget>::value 
						? std::numeric_limits<TTarget>::lowest() <= src
						: /*must be signed 0 to avoid conversion of src to unsigned*/0 <= src
					)
				) &&
				// conversion to unsigned (Warning 4018 and 4388) is ok here:
				src <= std::numeric_limits<TTarget>::max()
			);
			return static_cast<TTarget>(src);
		}
#ifdef __clang__
#pragma clang diagnostic pop
#else
MODIFY_WARNINGS_END
#endif

		template<typename T>
		std::streamoff explicit_convert_impl(adl_tag_t, tc::type::identity<std::streamoff>, std::fpos<T> const& pos) noexcept {
			return pos;
		}

		// std::pair

		template<typename TTargetFirst, typename TTargetSecond,	typename Tuple, /*not requires because of CWG issue 2369*/std::enable_if_t<2==std::tuple_size<std::remove_cvref_t<Tuple>>::value>* =nullptr>
		constexpr auto explicit_convert_impl(adl_tag_t, tc::type::identity<std::pair<TTargetFirst, TTargetSecond>>, Tuple&& tuple) return_decltype_MAYTHROW(
			std::pair<TTargetFirst, TTargetSecond>(
				tc::explicit_cast<TTargetFirst>(tc::get<0>(std::forward<Tuple>(tuple))),
				tc::explicit_cast<TTargetSecond>(tc::get<1>(std::forward<Tuple>(tuple)))
			)
		)

		template<typename TTargetFirst, typename TTargetSecond,	typename TSourceFirst, typename TSourceSecond>
		constexpr auto explicit_convert_impl(adl_tag_t, tc::type::identity<std::pair<TTargetFirst, TTargetSecond>>, TSourceFirst&& first, TSourceSecond&& second) return_decltype_MAYTHROW(
			std::pair<TTargetFirst, TTargetSecond>(
				tc::explicit_cast<TTargetFirst>(std::forward<TSourceFirst>(first)),
				tc::explicit_cast<TTargetSecond>(std::forward<TSourceSecond>(second))
			)
		)

		// tc::tuple

		template<typename... TTarget, typename... TSource, /*not requires because of CWG issue 2369*/std::enable_if_t<sizeof...(TTarget)==sizeof...(TSource)>* =nullptr>
		constexpr auto explicit_convert_impl(adl_tag_t, tc::type::identity<tc::tuple<TTarget...>>, TSource&&... src) return_decltype_MAYTHROW(
			tc::tuple<TTarget...>{
				tc::explicit_cast<TTarget>(std::forward<TSource>(src))...
			}
		)

		template<typename... TTarget, typename Tuple, /*not requires because of CWG issue 2369*/std::enable_if_t<sizeof...(TTarget)==std::tuple_size<std::remove_cvref_t<Tuple>>::value>* =nullptr>
		constexpr auto explicit_convert_impl(adl_tag_t, tc::type::identity<tc::tuple<TTarget...>>, Tuple&& tuple) return_decltype_MAYTHROW(
			tc::apply(tc::fn_explicit_cast<tc::tuple<TTarget...>>(), std::forward<Tuple>(tuple))
		)

		// tc::constant

		template<typename T, T t, typename TSource> requires tc::is_explicit_castable<T, TSource&&>::value
		constexpr tc::constant<t> explicit_convert_impl(adl_tag_t, tc::type::identity<std::integral_constant<T, t>>, TSource&& source) noexcept {
			_ASSERTE(t == tc::explicit_cast<T>(std::forward<TSource>(source)));
			return {};
		}
	}

	namespace return_cast_detail {
		namespace no_adl {
			template<typename... Args>
			struct [[nodiscard]] return_cast_impl final : tc::nonmovable {
				tc::tuple<Args&&...> m_tuple;
				template<typename TTarget> requires tc::is_explicit_castable<TTarget, Args...>::value
				constexpr operator TTarget() && return_MAYTHROW(
					tc::apply(tc::fn_explicit_cast<std::remove_cv_t<TTarget>>(), tc_move(m_tuple))
				)

				constexpr return_cast_impl&& operator+() && noexcept {
					return static_cast<return_cast_impl&&>(*this);
				}
			};
		}

		template<typename... Args>
		constexpr no_adl::return_cast_impl<Args...> return_cast(Args&&... args) noexcept {
			return { {}, tc::forward_as_tuple(std::forward<Args>(args)...) };
		}
	}

	// operator+ casts to xvalue to disallow RETURN_CAST in function with auto return type.
	#define RETURN_CAST/*(...)*/ \
		return +tc::return_cast_detail::return_cast /*(__VA_ARGS__)*/

	template<typename TTarget, typename TSource>
	[[nodiscard]] constexpr decltype(auto) reluctant_explicit_cast(TSource&& src) noexcept {
		STATICASSERTSAME(std::remove_cvref_t<TTarget>, TTarget);
		if constexpr( tc::is_base_of_decayed<TTarget, TSource>::value ) {
			return std::forward<TSource>(src);
		} else {
			return tc::explicit_cast<TTarget>(std::forward<TSource>(src));
		}
	}

	DEFINE_FN2_TMPL( reluctant_explicit_cast, (typename) )

	template<typename T>
	[[nodiscard]] constexpr bool is_single_codeunit(T const ch) noexcept {
		return char_in_range<T>(tc::underlying_cast(ch));
	}

	template<typename Target, typename Source>
	[[nodiscard]] Target explicit_cast_with_rounding(Source&& src) noexcept {
		if constexpr( std::is_floating_point< tc::decay_t<Source> >::value && tc::is_actual_integer< Target >::value ) {
			double srcRounded=std::floor( static_cast<double>(std::forward<Source>(src))+.5 );
			RETURN_CAST(srcRounded);
		} else {
			RETURN_CAST(src);
		}
	}

	DEFINE_FN2_TMPL( explicit_cast_with_rounding, (typename) );
	
	namespace no_adl {
		template<typename TTarget, typename... Args>
		struct [[nodiscard]] lazy_explicit_cast final : tc::nonmovable {
			tc::tuple<Args&&...> m_tuple;
			// std::remove_cv affects only values and leaves const/volatile references untouched, which is what we want.
			constexpr operator TTarget() && return_MAYTHROW(
				tc::apply(tc::fn_explicit_cast<std::remove_cv_t<TTarget>>(), tc_move(m_tuple))
			)
		};

		template <typename TTarget, typename... Args>
		struct is_value_safely_constructible<TTarget, lazy_explicit_cast<TTarget, Args...>&&> : tc::constant<true> {};
	}

	template<typename T, typename... Args>
	constexpr tc::no_adl::lazy_explicit_cast<T, Args...> lazy_explicit_cast(Args&&... args) noexcept {
		return { {}, tc::forward_as_tuple(std::forward<Args>(args)...) };
	}

	namespace detail {
		template<typename T, typename Func, typename... Args>
		constexpr decltype(auto) with_lazy_explicit_cast_impl(tc::constant<true>, Func func, Args&&... args) return_MAYTHROW(
			func(std::forward<Args>(args)...)
		)

		template<typename T, typename Func, typename... Args>
		constexpr decltype(auto) with_lazy_explicit_cast_impl(tc::constant<false>, Func func, Args&&... args) return_MAYTHROW(
			func(tc::lazy_explicit_cast<T>(std::forward<Args>(args)...))
		)
	}

	template<typename T, typename Func, typename... Args>
	constexpr decltype(auto) with_lazy_explicit_cast(Func&& func, Args&&... args) return_MAYTHROW(
		tc::detail::with_lazy_explicit_cast_impl<T>(tc::is_safely_constructible<T, Args&&...>{}, std::forward<Func>(func), std::forward<Args>(args)...)
	)

	namespace explicit_convert_adl {
		template<typename T, typename... TSource> requires tc::is_explicit_castable<T, TSource&&...>::value
		constexpr std::optional<T> explicit_convert_impl(adl_tag_t, tc::type::identity<std::optional<T>>, std::in_place_t, TSource&&... src) MAYTHROW {
			return tc::with_lazy_explicit_cast<T> (
				[](auto&&... args) return_ctor_MAYTHROW(std::optional<T>, (std::in_place, tc_move_if_owned(args)...)),
				std::forward<TSource>(src)...
			);
		}
	}

	template<typename Lhs, typename Rhs>
	constexpr void assign_explicit_cast(Lhs& lhs, Rhs&& rhs) noexcept {
		lhs=tc::explicit_cast<Lhs>(std::forward<Rhs>(rhs));
	}

	template<typename T, typename... Args>
	constexpr void optional_emplace(std::optional<T>& o, Args&& ... args) MAYTHROW {
		tc::with_lazy_explicit_cast<T>(
			[&](auto&&... args2) MAYTHROW {
				if constexpr( std::is_move_assignable<std::optional<T>>::value ) {
					if(std::is_constant_evaluated()) { // std::optional<T>::emplace is not constexpr
						o = std::optional<T>(std::in_place, tc_move_if_owned(args2)...);
						return;
					}
				}
				o.emplace(tc_move_if_owned(args2)...);
			},
			std::forward<Args>(args)...
		);
	}
}
