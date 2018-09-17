
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "is_static_castable.h"
#include "casts.h"

#include <type_traits>
#include <string>
#include <boost/optional.hpp>

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
	namespace no_adl {
		///////////////////////////////////////////////
		// default conversions

		template<typename TTarget, typename Enable=void>
		struct SDefaultConversions;

		// if target is a reference, disallow initializations which involve nontrivial conversions
		template<typename TTarget>
		struct SDefaultConversions<TTarget,  std::enable_if_t<std::is_reference<TTarget>::value>> {
			template<typename TSource>
			static std::enable_if_t<
				tc::creates_no_reference_to_temporary<TSource, TTarget>::value
			, TTarget> constexpr fn (TSource&& src) noexcept {
				return src; // static_cast if needed?
			}
		};

		template<typename TTarget >
		struct SClassConversions {
			static_assert( tc::is_decayed< TTarget >::value );

			// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
			template<typename... TSource>
			static std::enable_if_t<
				std::is_class<TTarget>::value &&
				tc::is_safely_constructible< TTarget, TSource&&... >::value
			, TTarget > constexpr fn (TSource&&... src) MAYTHROW {
				return TTarget(std::forward<TSource>(src)...);
			}

			// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
			template<typename TSource>
			static std::enable_if_t<
				!std::is_class<TTarget>::value &&
				std::is_class< std::remove_reference_t<TSource> >::value &&
				tc::is_static_castable< TSource&&, TTarget >::value
			, TTarget > constexpr fn (TSource&& src) MAYTHROW {
				return static_cast<TTarget>(std::forward<TSource>(src));
			}
		};

		template<typename TTarget>
		struct SDefaultConversions<TTarget, std::enable_if_t<std::is_class<TTarget>::value || std::is_union<TTarget>::value>> : SClassConversions<TTarget> {};

		template<>
		struct SDefaultConversions<bool> : SClassConversions<bool> {
			using SClassConversions<bool>::fn;

			template<typename TSource>
			static std::enable_if_t<
				std::is_same<tc::decay_t<TSource>, bool>::value
			, bool > constexpr fn (TSource&& src) noexcept {
				return src;
			}
		};

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
		struct SDefaultConversions<TTarget,  std::enable_if_t<tc::is_char<TTarget>::value>> : SClassConversions<TTarget> {
			using SClassConversions<TTarget>::fn;

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
		struct SDefaultConversions<TTarget,  std::enable_if_t<tc::is_actual_integer<TTarget>::value>> : SClassConversions<TTarget> {
			using SClassConversions<TTarget>::fn;

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

		template<typename TTarget>
		struct SDefaultConversions<TTarget,  std::enable_if_t<std::is_floating_point<TTarget>::value>> : SClassConversions<TTarget> {
			using SClassConversions<TTarget>::fn;

			template<typename TSource>
			static std::enable_if_t<
				tc::is_actual_arithmetic< TSource >::value
			, TTarget > constexpr fn (TSource src) noexcept {
				return static_cast<TTarget>(src);
			}
		};

		template<typename TTarget>
		struct SDefaultConversions<TTarget, std::enable_if_t<std::is_enum<TTarget>::value || std::is_pointer<TTarget>::value>> : SClassConversions<TTarget> {
			using SClassConversions<TTarget>::fn;

			static constexpr TTarget fn (TTarget src) noexcept {
				return src;
			}
		};

		template<typename TTarget, typename Enable=void>
		struct SConversions final : SDefaultConversions<TTarget> {};
	}

	namespace explicit_cast_detail {
		template<typename T>
		std::streamoff ConvertToUnderlying(std::fpos<T> const& pos) noexcept {
			return pos;
		}

		namespace no_adl {
			// control whether to ConvertToUnderlying first
			struct SFirstConvertToUnderlying {};
			struct SDirectConvert final: SFirstConvertToUnderlying {};
		}
		using no_adl::SFirstConvertToUnderlying;
		using no_adl::SDirectConvert;

		template<typename TTarget, typename... TSource>
		constexpr auto InternalConvert( SDirectConvert, TSource&&... src ) MAYTHROW
			return_decltype_rvalue_by_ref( tc::no_adl::SConversions<TTarget>::fn(std::forward<TSource>(src)...) ) // MAYTHROW

		template<typename TTarget, typename... TSource>
		constexpr TTarget InternalConvert( SFirstConvertToUnderlying, TSource&&... src ) MAYTHROW {
			return InternalConvert<TTarget>(/*prefer no second ConvertToUnderlying*/ SDirectConvert(), ConvertToUnderlying(std::forward<TSource>(src)... )); // MAYTHROW
		}
	}

	template<typename TTarget, typename... TSource>
	constexpr std::remove_cv_t<TTarget> explicit_cast(TSource&&... src) MAYTHROW {
		return explicit_cast_detail::InternalConvert<std::remove_cv_t<TTarget>>(/*prefer no ConvertToUnderlying*/ explicit_cast_detail::SDirectConvert(), std::forward<TSource>(src)... ); // MAYTHROW
	}

	///////////////////////////////////////////////
	// special conversions
	namespace no_adl {
		// features conversion from basic_string to char const*
		template<typename Char>
		struct SConversions<Char const* > final: SDefaultConversions<Char const*> {
			using SDefaultConversions<Char const*>::fn;

			template<typename Alloc>
			static Char const* fn(std::basic_string<Char,std::char_traits<Char>,Alloc> const& str) noexcept {
				return tc::as_c_str(str);
			}
		};


		template<typename TTargetFirst,typename TTargetSecond>
		struct SConversions<std::pair<TTargetFirst,TTargetSecond>> final: SDefaultConversions<std::pair<TTargetFirst,TTargetSecond>> {
			using SDefaultConversions<std::pair<TTargetFirst,TTargetSecond>>::fn;

			template<typename TSourceFirst,typename TSourceSecond>
			static constexpr std::pair<TTargetFirst,TTargetSecond> fn(std::pair<TSourceFirst,TSourceSecond> const& pair) MAYTHROW {
				// std::remove_cv affects only values and leaves const/volatile references untouched, which is what we want.
				return std::pair<TTargetFirst,TTargetSecond>(
					tc::explicit_cast<std::remove_cv_t<TTargetFirst>>(pair.first),
					tc::explicit_cast<std::remove_cv_t<TTargetSecond>>(pair.second)
				);
			}
			
			template<typename TSourceFirst,typename TSourceSecond>
			static constexpr std::pair<TTargetFirst,TTargetSecond> fn(TSourceFirst&& first, TSourceSecond&& second) MAYTHROW {
				// std::remove_cv affects only values and leaves const/volatile references untouched, which is what we want.
				return std::pair<TTargetFirst,TTargetSecond>(
					tc::explicit_cast<std::remove_cv_t<TTargetFirst>>(std::forward<TSourceFirst>(first)),
					tc::explicit_cast<std::remove_cv_t<TTargetSecond>>(std::forward<TSourceSecond>(second))
				);
			}
		};
	}

	DEFINE_FN_TMPL( explicit_cast, (typename) );
	
	template<typename TTarget, typename TSource, std::enable_if_t<tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	TSource&& reluctant_explicit_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource, std::enable_if_t<!tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	std::remove_cv_t<TTarget> reluctant_explicit_cast(TSource&& src) noexcept {
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
			operator T() const& noexcept {
				return std::make_from_tuple<T>(m_tuple);
			}
			operator T() && noexcept {
				return std::make_from_tuple<T>(std::move(m_tuple));
			}
		};
	}

	template<typename T, typename... Args>
	auto lazy_ctor(Args&&... args) noexcept {
		return tc::no_adl::lazy_ctor<T, decltype(std::forward_as_tuple(std::forward<Args>(args)...))>{ std::forward_as_tuple(std::forward<Args>(args)...) };
	}

	namespace no_adl {
		// SConversions cannot implement templated fn(Rng&&) *and* use fn(TSource&&)
		// from SDefaultConversions. Apparently, despite the std::enable_if constructs, both are considered
		// to have the same signatures and the using declaration is therefore ignored. Only Clang implements
		// this standard rule, however:
		// http://stackoverflow.com/questions/18861514/using-and-overloading-a-template-member-function-of-a-base-class
		template<typename TTarget>
		struct SOptionalConversionsHelper {
			template<typename... TSource, std::enable_if_t<
				!tc::is_safely_constructible< boost::optional<TTarget>, TSource&&... >::value // disable for trivial conversions to use move semantic / copy on write where possible
			>* = nullptr>
			static constexpr boost::optional<TTarget> fn(TSource&&... src) MAYTHROW {
				// std::remove_cv affects only values and leaves const/volatile references untouched, which is what we want.
				return boost::optional<TTarget>(tc::lazy_ctor<TTarget>(tc::explicit_cast<std::remove_cv_t<TTarget>>(std::forward<TSource>(src)...)));
			}
		};

		template<typename TTarget>
		struct SConversions<boost::optional<TTarget>> final: SDefaultConversions<boost::optional<TTarget>>, SOptionalConversionsHelper<TTarget> {
			using SDefaultConversions<boost::optional<TTarget>>::fn;
			using SOptionalConversionsHelper<TTarget>::fn;
		};
	}

	template<typename Lhs, typename Rhs>
	void assign_explicit_cast(Lhs& lhs, Rhs&& rhs) noexcept {
		lhs=tc::explicit_cast<Lhs>(std::forward<Rhs>(rhs));
	}
}
