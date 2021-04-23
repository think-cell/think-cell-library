
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "casts.h"
#include "noncopyable.h"
#include "optional.h"
#include "tuple.h"

#include <type_traits>
#include <string>

//////////////////////////////////////////////////////////////////
// generic initialization and assignment between different types
//
// Copy/move/assignment cannot be overloaded for third party types
// or builtins.
//
// If tc::explicit_cast<T, S> is used to convert a builtin numeric type S into
// another builtin numeric type T, rounding and range checks are
// applied.
//
// For other types, the default behavior of explicit_cast is
//   A a(tc::explicit_cast<A>(b))                       is equivalent to   A(a) ,
//   a=tc::explicit_cast<remove_reference<A>::type>(b)  is equivalent to   a=b ,
// i.e., the argument b is forwarded to the copy/move/assignment operators
// of A.
//
// Specialize explicit_convert if additional conversions for specific target
// types are required.

namespace tc {
	DEFINE_TAG_TYPE(fill_tag)
	DEFINE_TAG_TYPE(range_tag)

	namespace no_adl {

		template<typename T>
		struct char_limits;

		template<>
		struct char_limits<char> {
			static constexpr std::size_t c_nMaxCodeUnitsPerCodePoint = 4;
			static constexpr bool in_range(unsigned int n) noexcept {
				return n <= 0x7f;
			}
		};

		template<>
		struct char_limits<char16_t> {
			static constexpr std::size_t c_nMaxCodeUnitsPerCodePoint = 2;
			static constexpr bool in_range(unsigned int n) noexcept {
				return n <= 0xd7ff || (0xe000 <= n && n <= 0xffff);
			}
		};

		template<>
		struct char_limits<char32_t> {
			static constexpr std::size_t c_nMaxCodeUnitsPerCodePoint = 1;
			static constexpr bool in_range(unsigned int n) noexcept {
				return n <= 0xd7ff || (0xe000 <= n && n <= 0x10ffff);
			}
		};

		struct char_limits_undefined_dummy {};

		template<>
		struct char_limits<wchar_t> :
			std::conditional_t<
			2 == sizeof(wchar_t),
			char_limits<char16_t>,
			std::conditional_t<
			4 == sizeof(wchar_t),
			char_limits<char32_t>,
			char_limits_undefined_dummy
			>
			>
		{};

	}
	using no_adl::char_limits;

#ifdef _MSC_VER
	namespace no_adl {
		template<typename, typename TTarget, typename... Args>
		struct is_nothrow_list_initializable : std::false_type {};

		template<typename... T, typename... Args>
		struct is_nothrow_list_initializable<std::enable_if_t<sizeof...(T) == sizeof...(Args)>, tc::tuple<T...>, Args...>
			: std::conjunction<std::is_nothrow_constructible<T, Args>...>
		{};
	}
	template<typename TTarget, typename... Args>
	using is_nothrow_list_initializable = no_adl::is_nothrow_list_initializable<void, TTarget, Args...>;
#endif

	namespace explicit_convert_default {
		// This is the default explicit_convert, not a specialization, because aggregate initialization must have lower priority than other specializations which could also be aggregate types
		// Aggregate types are tc::is_safely_constructible from same type (copy/move). This is handled in the default explicit_cast below.
		template<typename TTarget, typename... Args, std::enable_if_t<std::is_class<TTarget>::value && std::is_aggregate<TTarget>::value>* = nullptr>
		constexpr auto explicit_convert_impl(tc::type::identity<TTarget>, Args&&... args)
#ifdef _MSC_VER 
			// noexcept(noexcept(...)) leads to ICE for some combinations of TTarget and Args.
			noexcept(is_nothrow_list_initializable<TTarget, Args...>::value) -> decltype(TTarget{std::forward<Args>(args)...})
		{
			return TTarget{std::forward<Args>(args)...};
		}
#else
			return_decltype_MAYTHROW( TTarget{std::forward<Args>(args)...} )
#endif

		template<typename T, std::size_t N, typename... Args>
		std::array<T, N> explicit_convert_impl(tc::type::identity<std::array<T, N>>, Args&&... args) = delete; // explicitly delete direct list construction of std::array
	}

	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(explicit_convert)

	namespace explicit_convert_adl {
		template<
			typename TTarget, typename TSource,
			std::enable_if_t<tc::is_char<TTarget>::value>* = nullptr,
			std::enable_if_t<tc::is_char<TSource>::value>* = nullptr
		>
		constexpr TTarget explicit_convert_impl(adl_tag_t, tc::type::identity<TTarget>, TSource src) noexcept {
			static_assert(tc::is_decayed< TTarget >::value);
			_ASSERTE(tc::char_limits<TSource>::in_range(tc::underlying_cast(src)));
			_ASSERTE(tc::char_limits<TTarget>::in_range(tc::underlying_cast(src)));
			return static_cast<TTarget>(src);
		}

		template<
			typename TTarget, typename TSource,
			std::enable_if_t<tc::is_actual_integer<TTarget>::value>* = nullptr,
			std::enable_if_t<std::is_floating_point<TSource>::value>* = nullptr
		>
		TTarget explicit_convert_impl(adl_tag_t, tc::type::identity<TTarget>, TSource src) noexcept {
			TTarget target=static_cast<TTarget>(src);
			_ASSERTEQUALDEBUG( target,src ); // default round-to-zero from floating point to integer is wrong most of the time, so we force rounding first
			return target;

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
		template<
			typename TTarget, typename TSource,
			std::enable_if_t<tc::is_actual_integer<TTarget>::value>* = nullptr,
			std::enable_if_t<tc::is_actual_integer<TSource>::value>* = nullptr
		>
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
	}

	template<typename TTarget, typename... Args, std::enable_if_t<!tc::is_safely_constructible<std::remove_cv_t<TTarget>, Args&&...>::value>* = nullptr>
	[[nodiscard]] constexpr auto explicit_cast(Args&&... args)
		return_decltype_MAYTHROW(tc::explicit_convert(tc::type::identity<std::remove_cv_t<TTarget>>(), std::forward<Args>(args)...))

	template<typename TTarget, typename... Args, std::enable_if_t<tc::is_safely_constructible<std::remove_cv_t<TTarget>, Args&&...>::value>* = nullptr>
	[[nodiscard]] constexpr auto explicit_cast(Args&&... args)
		return_ctor_MAYTHROW(std::remove_cv_t<TTarget>, (std::forward<Args>(args)...))

	DEFINE_FN2_TMPL( explicit_cast, (typename) );

	namespace is_explicit_castable_detail {
		template<typename TTarget, typename ArgList, typename Enable=void>
		struct is_explicit_castable : std::false_type {};

		template<typename TTarget, typename... Args>
		struct is_explicit_castable<TTarget, tc::type::list<Args...>, tc::void_t<decltype(
			tc::explicit_cast<TTarget>(std::declval<Args>()...)
		)>> : std::true_type {};
	}
	template<typename TTarget, typename... Args>
	using is_explicit_castable=is_explicit_castable_detail::is_explicit_castable<TTarget,tc::type::list<Args...>>;

	namespace explicit_convert_adl {
		// std::pair

		template<typename TTargetFirst, typename TTargetSecond,	typename Tuple, std::enable_if_t<2==std::tuple_size<tc::remove_cvref_t<Tuple>>::value>* =nullptr>
		constexpr auto explicit_convert_impl(adl_tag_t, tc::type::identity<std::pair<TTargetFirst, TTargetSecond>>, Tuple&& tuple) return_decltype_MAYTHROW(
			std::pair<TTargetFirst, TTargetSecond>(
				tc::explicit_cast<TTargetFirst>(/*adl*/get<0>(std::forward<Tuple>(tuple))),
				tc::explicit_cast<TTargetSecond>(/*adl*/get<1>(std::forward<Tuple>(tuple)))
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

		template<typename... TTarget, typename... TSource, std::enable_if_t<sizeof...(TTarget)==sizeof...(TSource)>* =nullptr>
		constexpr auto explicit_convert_impl(adl_tag_t, tc::type::identity<tc::tuple<TTarget...>>, TSource&&... src) return_decltype_MAYTHROW(
			tc::tuple<TTarget...>{
				tc::explicit_cast<TTarget>(std::forward<TSource>(src))...
			}
		)

		template<typename... TTarget, typename Tuple, std::enable_if_t<sizeof...(TTarget)==std::tuple_size<tc::remove_cvref_t<Tuple>>::value>* =nullptr>
		constexpr auto explicit_convert_impl(adl_tag_t, tc::type::identity<tc::tuple<TTarget...>>, Tuple&& tuple) return_decltype_MAYTHROW(
			tc::apply(tc::fn_explicit_cast<tc::tuple<TTarget...>>(), std::forward<Tuple>(tuple))
		)

		// std::integral_constant

		template<typename T, T V, typename TSource,	std::enable_if_t<tc::is_explicit_castable<T, TSource&&>::value>* = nullptr>
		constexpr std::integral_constant<T, V> explicit_convert_impl(adl_tag_t, tc::type::identity<std::integral_constant<T, V>>, TSource&& source) MAYTHROW {
			_ASSERTE(V == tc::explicit_cast<T>(std::forward<TSource>(source)));
			return {};
		}
	}

	template<typename TTarget, typename TSource, std::enable_if_t<tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	[[nodiscard]] constexpr TSource&& reluctant_explicit_cast(TSource&& src) noexcept {
		STATICASSERTSAME(tc::remove_cvref_t<TTarget>, TTarget);
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource, std::enable_if_t<!tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	[[nodiscard]] constexpr std::remove_cv_t<TTarget> reluctant_explicit_cast(TSource&& src) noexcept {
		STATICASSERTSAME(tc::remove_cvref_t<TTarget>, TTarget);
		return tc::explicit_cast<TTarget>(std::forward<TSource>(src));
	}

	DEFINE_FN2_TMPL( reluctant_explicit_cast, (typename) )

	template<typename T>
	[[nodiscard]] bool issingleunit(T ch) noexcept {
		return no_adl::char_limits<T>::in_range(tc::underlying_cast(ch));
	}

	template<typename Target, typename Source>
	[[nodiscard]] std::enable_if_t<
		std::is_floating_point< tc::decay_t<Source> >::value && tc::is_actual_integer< Target >::value
	,Target> explicit_cast_with_rounding(Source&& src) noexcept {
		double srcRounded=std::floor( static_cast<double>(std::forward<Source>(src))+.5 );
		return tc::explicit_cast<Target>(srcRounded);
	}

	template<typename Target, typename Source>
	[[nodiscard]] std::enable_if_t<
		!(std::is_floating_point< tc::decay_t<Source> >::value && tc::is_actual_integer< Target >::value)
	,Target> explicit_cast_with_rounding(Source&& src) noexcept {
		return tc::explicit_cast<Target>(src);
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
	}

	template<typename T, typename... Args>
	constexpr tc::no_adl::lazy_explicit_cast<T, Args...> lazy_explicit_cast(Args&&... args) noexcept {
		return { {}, tc::forward_as_tuple(std::forward<Args>(args)...) };
	}

	namespace return_cast_detail {
		namespace no_adl {
			template<typename... Args>
			struct [[nodiscard]] return_cast_impl final : tc::nonmovable {
				tc::tuple<Args&&...> m_tuple;
				template<typename TTarget>
				constexpr operator TTarget() && return_MAYTHROW(
					tc::apply(tc::fn_explicit_cast<std::remove_cv_t<TTarget>>(), tc_move(m_tuple))
				)
			};
		}

		template<typename... Args>
		constexpr no_adl::return_cast_impl<Args...> return_cast(Args&&... args) noexcept {
			return { {}, tc::forward_as_tuple(std::forward<Args>(args)...) };
		}
	}

	#define RETURN_CAST(...) return std::move(tc::return_cast_detail::return_cast(__VA_ARGS__))
	// Using decltype(this->member) ensures casting to the correct type even if member is shadowed (but disallows base classes).
	#define MEMBER_INIT_CAST(member, ...) member(tc::explicit_cast<decltype(this->member)>(__VA_ARGS__))

	namespace detail {
		template<typename T, typename Func, typename... Args>
		constexpr decltype(auto) with_lazy_explicit_cast_impl(std::true_type, Func func, Args&&... args) return_MAYTHROW(
			func(std::forward<Args>(args)...)
		)

		template<typename T, typename Func, typename... Args>
		constexpr decltype(auto) with_lazy_explicit_cast_impl(std::false_type, Func func, Args&&... args) return_MAYTHROW(
			func(tc::lazy_explicit_cast<T>(std::forward<Args>(args)...))
		)
	}

	template<typename T, typename Func, typename... Args>
	constexpr decltype(auto) with_lazy_explicit_cast(Func&& func, Args&&... args) return_MAYTHROW(
		tc::detail::with_lazy_explicit_cast_impl<T>(tc::is_safely_constructible<T, Args&&...>{}, std::forward<Func>(func), std::forward<Args>(args)...)
	)

	namespace explicit_convert_adl {
		template<typename T, typename... TSource>
		constexpr std::optional<T> explicit_convert_impl(adl_tag_t, tc::type::identity<std::optional<T>>, std::in_place_t, TSource&&... src) MAYTHROW { // return_MAYTHROW triggers C1001 An internal error has occured in the compiler
			return tc::with_lazy_explicit_cast<T> (
				[](auto&&... args) MAYTHROW { return std::optional<T>(std::in_place, std::forward<decltype(args)>(args)...); }, // return_MAYTHROW triggers C1001 An internal error has occured in the compiler
				std::forward<TSource>(src)...
			);
		}
	}

	template<typename Lhs, typename Rhs>
	constexpr void assign_explicit_cast(Lhs& lhs, Rhs&& rhs) noexcept {
		lhs=tc::explicit_cast<Lhs>(std::forward<Rhs>(rhs));
	}

	template<typename T, typename... Args>
	void optional_emplace(std::optional<T>& o, Args&& ... args) MAYTHROW {
		tc::with_lazy_explicit_cast<T>(
			[&](auto&&... args2) MAYTHROW { o.emplace(tc_move_if_owned(args2)...); },
			std::forward<Args>(args)...
		);
	}
}
