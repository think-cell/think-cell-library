
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "casts.h"

#include <type_traits>
#include <string>
#include <optional>

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
// Specialize SConversions if additional conversions for specific target
// types are required.

namespace tc {
	DEFINE_TAG_TYPE(list_initialize_tag)

	namespace no_adl {
		///////////////////////////////////////////////
		// default conversions

		template<typename TTarget, typename Enable=void>
		struct SConversions final {};

		template<typename T>
		struct char_limits;

		template<>
		struct char_limits<char> {
			static constexpr bool in_range(unsigned int n) noexcept {
				return n<=0x7f;
			}
		};

		template<>
		struct char_limits<char16_t> {
			static constexpr bool in_range(unsigned int n) noexcept {
				return n<=0xd7ff || (0xe000<=n && n<=0xffff);
			}
		};

		template<>
		struct char_limits<char32_t> {
			static constexpr bool in_range(unsigned int n) noexcept {
				return n<=0xd7ff || (0xe000<=n && n<=0x10ffff);
			}
		};

		struct char_limits_undefined_dummy {};

		template<>
		struct char_limits<wchar_t> :
			std::conditional_t<
				2==sizeof(wchar_t),
				char_limits<char16_t>,
				std::conditional_t<
					4==sizeof(wchar_t),
					char_limits<char32_t>,
					char_limits_undefined_dummy
				>
			>
		{};

		template<typename TTarget>
		struct SConversions<TTarget, std::enable_if_t<tc::is_char<TTarget>::value>> {
			template<typename TSource>
			static std::enable_if_t<
				tc::is_char< TSource >::value
			, TTarget > constexpr fn (TSource src) noexcept {
				static_assert( tc::is_decayed< TTarget >::value );
				_ASSERTE( char_limits<TSource>::in_range(tc::underlying_cast(src)) );
				_ASSERTE( char_limits<TTarget>::in_range(tc::underlying_cast(src)) );
				return static_cast<TTarget>(src);
			}
		};

		template<typename TTarget>
		struct SConversions<TTarget, std::enable_if_t<tc::is_actual_integer<TTarget>::value>> {
			template<typename TSource, std::enable_if_t<
				std::is_floating_point<TSource>::value>* = nullptr>
			static TTarget fn(TSource src) noexcept {
				TTarget target=static_cast<TTarget>(src);
				_ASSERTEQUALDEBUG( target,src ); // default round-to-zero from floating point to integer is wrong most of the time, so we force rounding first
				return target;
			}

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-compare"
#else
#pragma warning(push)
#pragma warning(disable:4018) // signed/unsigned mismatch
#endif
			template<typename TSource, std::enable_if_t<
				tc::is_actual_integer<TSource>::value>* = nullptr>
			static constexpr TTarget fn(TSource src) noexcept {
				_ASSERTE(
					(
						!std::is_signed<TSource>::value ||
						( std::is_signed<TTarget>::value 
							? std::numeric_limits<TTarget>::lowest() <= src
							: /*must be signed 0 to avoid conversion of src to unsigned*/0 <= src
						)
					) &&
					// conversion to unsigned (Warning 4018) is ok here:
					src <= std::numeric_limits<TTarget>::max()
				);
				return static_cast<TTarget>(src);
			}
#ifdef __clang__
#pragma clang diagnostic pop
#else
#pragma warning( pop )
#endif
		};
	}

	namespace explicit_cast_detail {
		template<typename T>
		std::streamoff ConvertToUnderlying(std::fpos<T> const& pos) noexcept {
			return pos;
		}

		template <typename TTarget, typename ArgList, typename Enable=void>
		struct HaveConversions final : std::false_type {};
		
		template <typename TTarget, typename... Args>
		struct HaveConversions<TTarget, tc::type::list<Args...>, tc::void_t<decltype(tc::no_adl::SConversions<TTarget>::fn(std::declval<Args>()...))>> final : std::true_type {};
		
		template<typename TTarget, typename... Args>
		constexpr auto InternalConvert( Args&&... args ) MAYTHROW -> std::enable_if_t<
			HaveConversions<TTarget,tc::type::list<Args...>>::value,
		TTarget> {
			return tc::no_adl::SConversions<TTarget>::fn(std::forward<Args>(args)...); // MAYTHROW
		}

		template<typename TTarget, typename... Args>
		constexpr auto InternalConvert( Args&&... args ) MAYTHROW -> std::enable_if_t<
			!HaveConversions<TTarget,tc::type::list<Args...>>::value,
		decltype(tc::no_adl::SConversions<TTarget>::fn(ConvertToUnderlying(std::forward<Args>(args)...))) > {
			return tc::no_adl::SConversions<TTarget>::fn(ConvertToUnderlying(std::forward<Args>(args)...)); // MAYTHROW
		}
	}

	template<typename TTarget, typename... Args>
	constexpr auto explicit_cast(Args&&... args) MAYTHROW -> std::enable_if_t<
		!tc::is_safely_constructible<std::remove_cv_t<TTarget>, Args&&...>::value,
	decltype(explicit_cast_detail::InternalConvert<std::remove_cv_t<TTarget>>(std::forward<Args>(args)... )) > {
		return explicit_cast_detail::InternalConvert<std::remove_cv_t<TTarget>>(std::forward<Args>(args)... );
	}

	template<typename TTarget, typename... Args>
	constexpr auto explicit_cast(Args&&... args) MAYTHROW -> std::enable_if_t<
		tc::is_safely_constructible<std::remove_cv_t<TTarget>, Args&&...>::value,
	std::remove_cv_t<TTarget> > {
		return std::remove_cv_t<TTarget>(std::forward<Args>(args)...); // MAYTHROW
	}

	template<typename TTarget, typename... Args>
	constexpr auto explicit_cast(tc::list_initialize_tag_t, Args&&... args) MAYTHROW -> std::enable_if_t<
		std::is_class<TTarget>::value,
	decltype(std::remove_cv_t<TTarget>{std::declval<Args>()...})> {
		return std::remove_cv_t<TTarget>{std::forward<Args>(args)...};
	}

	///////////////////////////////////////////////
	// special conversions
	namespace no_adl {
		template<typename TTargetFirst,typename TTargetSecond>
		struct SConversions<std::pair<TTargetFirst,TTargetSecond>> final {
			template<typename TSourceFirst,typename TSourceSecond>
			static constexpr std::pair<TTargetFirst,TTargetSecond> fn(std::pair<TSourceFirst,TSourceSecond> const& pair) MAYTHROW {
				return std::pair<TTargetFirst,TTargetSecond>(
					tc::explicit_cast<TTargetFirst>(pair.first),
					tc::explicit_cast<TTargetSecond>(pair.second)
				);
			}

			template<typename TSourceFirst,typename TSourceSecond>
			static constexpr std::pair<TTargetFirst,TTargetSecond> fn(std::pair<TSourceFirst,TSourceSecond>&& pair) MAYTHROW {
				return std::pair<TTargetFirst,TTargetSecond>(
					tc::explicit_cast<TTargetFirst>(tc_move(pair).first),
					tc::explicit_cast<TTargetSecond>(tc_move(pair).second)
				);
			}

			template<typename TSourceFirst,typename TSourceSecond>
			static constexpr std::pair<TTargetFirst,TTargetSecond> fn(TSourceFirst&& first, TSourceSecond&& second) MAYTHROW {
				return std::pair<TTargetFirst,TTargetSecond>(
					tc::explicit_cast<TTargetFirst>(std::forward<TSourceFirst>(first)),
					tc::explicit_cast<TTargetSecond>(std::forward<TSourceSecond>(second))
				);
			}
		};
	}

	DEFINE_FN_TMPL( explicit_cast, (typename) );
	
	template<typename TTarget, typename TSource, std::enable_if_t<tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	constexpr TSource&& reluctant_explicit_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource, std::enable_if_t<!tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	constexpr std::remove_cv_t<TTarget> reluctant_explicit_cast(TSource&& src) noexcept {
		return explicit_cast<TTarget>(std::forward<TSource>(src));
	}

	template<typename T>
	bool issingleunit(T ch) noexcept {
		return no_adl::char_limits<T>::in_range(tc::underlying_cast(ch));
	}

	template<typename Target, typename Source>
	std::enable_if_t<
		std::is_floating_point< tc::decay_t<Source> >::value && tc::is_actual_integer< Target >::value
	,Target> explicit_cast_with_rounding(Source&& src) noexcept {
		double srcRounded=std::floor( static_cast<double>(std::forward<Source>(src))+.5 );
		return tc::explicit_cast<Target>(srcRounded);
	}

	template<typename Target, typename Source>
	std::enable_if_t<
		!(std::is_floating_point< tc::decay_t<Source> >::value && tc::is_actual_integer< Target >::value)
	,Target> explicit_cast_with_rounding(Source&& src) noexcept {
		return tc::explicit_cast<Target>(src);
	}

	DEFINE_FN_TMPL( explicit_cast_with_rounding, (typename) );
	
	namespace no_adl {
		template<typename T, typename Tuple>
		struct lazy_ctor final {
			Tuple m_tuple;
			operator T() && noexcept {
				return std::apply(tc::fn_explicit_cast<std::remove_cv_t<T>>(), tc_move_always(m_tuple));
			}
		};
	}

	template<typename T, typename... Args>
	auto lazy_ctor(Args&&... args) noexcept {
		return tc::no_adl::lazy_ctor<T, decltype(std::forward_as_tuple(std::forward<Args>(args)...))>{ std::forward_as_tuple(std::forward<Args>(args)...) };
	}

	namespace no_adl {
		template<typename TTarget>
		struct SConversions<std::optional<TTarget>> final {
			template<typename... TSource>
			static constexpr std::optional<TTarget> fn(std::in_place_t, TSource&&... src) MAYTHROW {
				// std::remove_cv affects only values and leaves const/volatile references untouched, which is what we want.
				return std::optional<TTarget>(std::in_place, tc::lazy_ctor<TTarget>(std::forward<TSource>(src)...));
			}
		};
	}

	template<typename Lhs, typename Rhs>
	void assign_explicit_cast(Lhs& lhs, Rhs&& rhs) noexcept {
		lhs=tc::explicit_cast<Lhs>(std::forward<Rhs>(rhs));
	}

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
}
