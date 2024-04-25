
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "generic_macros.h"
#include "move.h"
#include "type_list.h"

#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/facilities/overload.hpp>
#include <boost/preprocessor/seq/cat.hpp>
#include <boost/integer.hpp>

#include <type_traits>
#include <limits>
#include <concepts>

//////////////////////////////////////////////////////////////////////////
// STATICASSERTSAME
#ifdef TC_MAC
# define TC_CONSTEVAL_PCH_WORKAROUND constexpr // Xcode 13 still has problems with consteval when compiling our precompiled header
#else
# define TC_CONSTEVAL_PCH_WORKAROUND consteval
#endif
namespace tc::static_assert_impl {
	template <typename T1, T1 n1, typename T2, T2 n2>
	struct NotEqual { // struct name chosen to make __FUNCSIG__ in static_assert message more meaningful
		static TC_CONSTEVAL_PCH_WORKAROUND void test() {
			// assert is inside a function so __FUNCSIG__ can be used to include template parameters in error message
			static_assert(n1 == n2,
#ifdef _MSC_VER
				"STATICASSERTEQUAL failed: " __FUNCSIG__
#else
				"values not equal: see template parameters in error log" // __PRETTY_FUNCTION__ is similar to __FUNCSIG__ but is not a string literal
#endif
			);
		}
	};

	template <typename T1, typename T2>
	struct NotSame { // struct name chosen to make __FUNCSIG__ in static_assert message more meaningful
		static TC_CONSTEVAL_PCH_WORKAROUND void test() {
			// assert is inside a function so __FUNCSIG__ can be used to include template parameters in error message
			static_assert(std::is_same<T1, T2>::value
#ifdef _MSC_VER
				// clang's default message in this case contains the non-matching type parameters
				, "STATICASSERTSAME failed: " __FUNCSIG__
#endif
			);
		}
	};
}

// STATICASSERTEQUAL_IMPL and STATICASSERTSAME_IMPL should expand to something that is allowed wherever static_assert is allowed
// and has no effect on the generated code, but forces NotEqual::test() or NotSame::test() to be compiled, which is why we use static_assert((test(), true)).
#define STATICASSERTEQUAL_IMPL(n1, n2) static_assert((tc::static_assert_impl::NotEqual<decltype(n1), n1, decltype(n2), n2>::test(), true))
#define STATICASSERTSAME_IMPL(t1, t2) static_assert((tc::static_assert_impl::NotSame<t1, t2>::test(), true))

// Each macro should static_assert at the point of expansion (to point to the correct error location and show the correct message)
// as well is inside the template (to show the erroneous parameters).
#define STATICASSERTEQUAL(n1, n2, ...) static_assert((n1) == (n2) __VA_OPT__(,) __VA_ARGS__); STATICASSERTEQUAL_IMPL(TC_FWD(n1), TC_FWD(n2))
#define STATICASSERTSAME(t1, t2, ...) static_assert(std::is_same<t1, t2>::value __VA_OPT__(,) __VA_ARGS__); STATICASSERTSAME_IMPL(TC_FWD(t1), TC_FWD(t2))

// Use as type of constructor arguments that are required for enabling / disabling constructor through SFINAE.
// To be replaced by template parameter default when Visual C++ supports template parameter defaults for functions.
struct unused_arg final {};

namespace tc {
	//////////////////////////
	// decay
	namespace no_adl {
		template<typename T, bool bPreventSlicing = true>
		struct decay {
			using type = std::decay_t<T>; // must still do function-to-pointer
		};

		// forbid decaying of C arrays, they decay to pointers, very unlike std/tc::array
		template<typename T>
		struct do_not_decay_arrays {
		private:
			char dummy; // make non-empty, empty structs are preferred in make_subrange_result
		};

#pragma push_macro("DECAY_ARRAY_IMPL")
#define DECAY_ARRAY_IMPL(cv) \
		template<typename T, bool bPreventSlicing> \
		struct decay<T cv[], bPreventSlicing> { \
			using type=do_not_decay_arrays<T cv[]>; \
		}; \
		template<typename T,std::size_t N, bool bPreventSlicing> \
		struct decay<T cv[N], bPreventSlicing> { \
			using type = do_not_decay_arrays<T cv[N]>; \
		};

		DECAY_ARRAY_IMPL(BOOST_PP_EMPTY())
		DECAY_ARRAY_IMPL(const)
		DECAY_ARRAY_IMPL(volatile)
		DECAY_ARRAY_IMPL(const volatile)
#pragma pop_macro("DECAY_ARRAY_IMPL")

		template<typename T, bool bPreventSlicing>
		struct decay<T volatile, bPreventSlicing> {
			using type = typename decay<T, bPreventSlicing>::type; // recursive
		};

		template<typename T, bool bPreventSlicing>
		struct decay<T const, bPreventSlicing> {
			using type = typename decay<T, bPreventSlicing>::type; // recursive
		};

		template<typename T, bool bPreventSlicing>
		struct decay<T const volatile, bPreventSlicing> {
			using type = typename decay<T, bPreventSlicing>::type; // recursive
		};

		template<typename T, bool bPreventSlicing, bool bIsNonLeafPolymorphic = std::is_polymorphic<T>::value && !std::is_final<T>::value>
		struct decay_reference {
			using type = typename decay<T, bPreventSlicing>::type; // recursive
		};

		template<typename T>
		struct decay_reference<T, /*bPreventSlicing*/true, /*bIsNonLeafPolymorphic*/true> {};

		template<typename T, bool bPreventSlicing>
		struct decay<T&, bPreventSlicing> : decay_reference<T, bPreventSlicing> {}; // recursive

		template<typename T, bool bPreventSlicing>
		struct decay<T&&, bPreventSlicing> : decay_reference<T, bPreventSlicing> {}; // recursive
	}
	using no_adl::decay;

	template<typename T>
	using decay_t = typename decay<T>::type;

	//	auto a=b; uses std::decay
	// <=>
	//	auto a=decay_copy(b); uses tc::decay_t
	template<typename T>
	constexpr tc::decay_t<T&&> decay_copy(T&& t) noexcept {
		return tc_move_if_owned(t);
	}
	
	//////////////////////////
	// type classification concepts

	template<typename T>
	concept char_type = boost::mp11::mp_set_contains<boost::mp11::mp_list<char, wchar_t, char8_t, char16_t, char32_t>, std::remove_cv_t<T>>::value;

	template< typename T >
	concept char_ptr = std::is_pointer<T>::value && tc::char_type<std::remove_pointer_t<T>>;

	namespace char_like_detail {
		template<typename T>
		inline constexpr bool char_like_impl = tc::char_type<T>; // Customizable
	}

	template<typename T>
	concept char_like = char_like_detail::char_like_impl<T>;

	template< typename T >
	concept decayed = std::same_as< T, tc::decay_t<T> >;

	template<typename T>
	concept actual_integer = std::integral<T> && (!std::same_as<std::remove_cv_t<T>, bool>) && (!tc::char_type<T>);
	template<typename T>
	concept actual_unsigned_integer = tc::actual_integer<T> && std::unsigned_integral<T>;
	template<typename T>
	concept actual_signed_integer = tc::actual_integer<T> && std::signed_integral<T>;

	template<typename T>
	concept actual_arithmetic = tc::actual_integer<T> || std::floating_point<T>;

	template<typename T>
	concept empty_type = std::is_empty<T>::value && std::is_trivial<std::remove_cv_t<T>>::value;

	template<typename T>
	concept enum_type = std::is_enum<T>::value;

	template<tc::actual_integer auto val>
	using int_value_least_t = std::conditional_t<
		0<=val,
		typename boost::int_max_value_t<val>::least,
		typename boost::int_min_value_t<val>::least
	>;

	template<tc::actual_integer auto val>
	using uint_value_least_t = typename boost::uint_value_t<val>::least;

	//////////////////////////////////////////////////////////////////////////
	// constant

	template<auto t>
	using constant = std::integral_constant<decltype(t), t>;

	template<tc::actual_integer auto N> requires (N >= 0)
	using least_uint_constant = tc::constant<static_cast<tc::uint_value_least_t<N>>(N)>;

	//////////////////////////////////////////////////////////////////////////
	// TRAITFROMCONCEPT

	namespace no_adl {
		template<auto ConceptLambda>
		struct trait_from_concept {
			template<typename T>
			using trait = tc::constant<ConceptLambda.template operator()<T>()>;
		};
	}

	#define TRAITFROMCONCEPT(TheConcept) \
		tc::no_adl::trait_from_concept<[]<typename TraitFromConceptType /* prevent name shadow */> () consteval { return TheConcept<TraitFromConceptType>; }>::template trait

	//////////////////////////
	// derived_from

	// 1. std::derived_from<X,X> is true only if X is a class type.
	// 2. use std::is_base_of for private/protected inheritance

	namespace derived_from_detail {
		template<typename Derived, typename Base>
		inline constexpr bool derived_from_impl = std::same_as<Derived, Base> || std::derived_from<Derived, Base>; // Customizable
	}

	template<typename Derived, typename Base>
	concept derived_from = derived_from_detail::derived_from_impl<std::remove_cv_t<Derived>, std::remove_cv_t<Base>>;

	namespace no_adl {
		template<typename Derived, typename Base>
		struct decayed_derived_from_impl : tc::constant<
			tc::derived_from< typename tc::decay<Derived, /*bPreventSlicing*/false>::type, Base >
		> {
			static_assert(tc::decayed<Base>);
		};
	}
	template<typename Derived, typename Base>
	concept decayed_derived_from = no_adl::decayed_derived_from_impl<Derived, Base>::value;

	namespace no_adl {
		template<template<typename...> typename A, template<typename...> typename B>
		struct is_same_template : tc::constant<false> {};

		template<template<typename...> typename A>
		struct is_same_template<A, A> : tc::constant<true> {};
	}
	template<template<typename...> typename A, template<typename...> typename B>
	concept same_template_as = no_adl::is_same_template<A, B>::value;
} // tc

	/////////////////////////////////////////////
	// is_instance

#define IS_INSTANCE_TRAIT(suffix, seq, ...) \
namespace no_adl { \
	template<typename TInstance, template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template> struct is_instance ## suffix: tc::constant<false> {}; \
	template<typename TInstance, template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template> struct is_instance ## suffix<TInstance const, Template>: is_instance ## suffix<TInstance, Template> {}; \
	template<typename TInstance, template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template> struct is_instance ## suffix<TInstance volatile, Template>: is_instance ## suffix<TInstance, Template> {}; \
	template<typename TInstance, template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template> struct is_instance ## suffix<TInstance const volatile, Template>: is_instance ## suffix<TInstance, Template> {}; \
	template< \
		TC_PP_PARAMS_ENUM(seq), \
		template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template \
	> struct is_instance ## suffix<Template<TC_PP_PARAMS_ARG_ENUM(seq)>, Template> : tc::constant<true> { \
		__VA_ARGS__ \
	}; \
} \
using no_adl::is_instance ## suffix; \
template<typename TInstance, template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template> \
concept instance ## suffix = no_adl::is_instance ## suffix<TInstance, Template>::value;

namespace tc {
	// Each n in the suffix adds one non-type template parameter, each t a prefix type template parameter.
	IS_INSTANCE_TRAIT(, ((typename)(...)(T)), using arguments = boost::mp11::mp_list<T...>;)
	IS_INSTANCE_TRAIT(_n, ((auto)(V))((typename)(...)(T)), using arguments = boost::mp11::mp_list<tc::constant<V>, T...>; )
	IS_INSTANCE_TRAIT(_tn, ((typename)(T1))((auto)(V))((typename)(...)(T)), using arguments = boost::mp11::mp_list<T1, tc::constant<V>, T...>;)
	IS_INSTANCE_TRAIT(_ttn, ((typename)(T1))((typename)(T2))((auto)(V))((typename)(...)(T)), using arguments = boost::mp11::mp_list<T1, T2, tc::constant<V>, T...>;)
}

	/////////////////////////////////////////////
	// is_instance_or_derived

#define IS_INSTANCE_OR_DERIVED_TRAIT(suffix, seq, ...) \
namespace no_adl { \
	template<template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template, TC_PP_PARAMS_ENUM(seq)> \
	struct BOOST_PP_CAT(is_instance_or_derived_found, suffix) final: tc::constant<true> { \
		__VA_ARGS__ \
	}; \
	template<template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template> \
	struct BOOST_PP_CAT(is_instance_or_derived_detector, suffix) final { \
		template<TC_PP_PARAMS_ENUM(seq)> \
		static BOOST_PP_CAT(is_instance_or_derived_found, suffix)<Template, TC_PP_PARAMS_ARG_ENUM(seq)> detector(Template<TC_PP_PARAMS_ARG_ENUM(seq)>*); \
		static tc::constant<false> detector(...); \
	}; \
} \
template<typename TInstance, template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template> \
using BOOST_PP_CAT(is_instance_or_derived, suffix) = \
	decltype( \
		no_adl::BOOST_PP_CAT(is_instance_or_derived_detector, suffix)<Template>::detector( \
			std::declval<std::conditional_t<std::is_reference<TInstance>::value, /* anything that cannot bind to Template<...>* */ int*, std::remove_cvref_t<TInstance>*>>() \
		) \
	); \
template<typename TInstance, template<TC_PP_PARAMS_TYPE_ENUM(seq)> typename Template> \
concept instance_or_derived ## suffix = \
	BOOST_PP_CAT(is_instance_or_derived, suffix)<TInstance, Template>::value;

namespace tc {
	IS_INSTANCE_OR_DERIVED_TRAIT(, ((typename)(...)(T)), using base_instance=Template<T...>;using arguments = boost::mp11::mp_list<T...>;)

	/////////////////////////////////////////////
	// apply_cvref

	template<typename Dst, typename Src>
	struct apply_cvref {
		static_assert( std::is_same< Src, std::remove_cvref_t<Src> >::value && !std::is_reference<Src>::value, "Src must not be cv-qualified. Check if a template specialization of apply_cvref is missing." );
		using type = Dst;
	};

	#pragma push_macro("APPLY_CVREF_IMPL")
	#define APPLY_CVREF_IMPL(cvref) \
	template<typename Dst, typename Src> \
	struct apply_cvref<Dst, Src cvref> { \
		using type = Dst cvref; \
	};

	APPLY_CVREF_IMPL(&)
	APPLY_CVREF_IMPL(&&)
	APPLY_CVREF_IMPL(const&)
	APPLY_CVREF_IMPL(const&&)
	APPLY_CVREF_IMPL(const)
	APPLY_CVREF_IMPL(volatile&)
	APPLY_CVREF_IMPL(volatile&&)
	APPLY_CVREF_IMPL(volatile)
	APPLY_CVREF_IMPL(volatile const&)
	APPLY_CVREF_IMPL(volatile const&&)
	APPLY_CVREF_IMPL(volatile const)

	#pragma pop_macro("APPLY_CVREF_IMPL")

	template< typename Dst, typename Src >
	using apply_cvref_t = typename apply_cvref<Dst, Src>::type;

	/////////////////////////////////////////////
	// same_cvref

	template<typename Dst, typename Src>
	struct same_cvref : apply_cvref<Dst, Src> {
		STATICASSERTSAME(Dst, std::remove_cvref_t<Dst>); // use non-cv-qualified non-reference Dst type or apply_cvref
	};

	template< typename Dst, typename Src >
	using same_cvref_t = typename same_cvref<Dst, Src>::type;

	//////////////////////////
	// safely_convertible_to/assignable_from/constructible_from

	namespace no_adl {
		template <typename TTarget, typename... Args>
		struct is_class_safely_constructible;

		template <typename TTarget, typename... Args>
		struct is_value_safely_constructible_base : tc::constant<false> {};

		template <typename TTarget, typename Arg0, typename... Args> requires std::is_class<TTarget>::value
		struct is_value_safely_constructible_base<TTarget, Arg0, Args...>
			// prevent slicing
			: tc::constant<
				( 0<sizeof...(Args) || std::is_same<TTarget, std::remove_cvref_t<Arg0>>::value || !tc::derived_from<std::remove_reference_t<Arg0>, TTarget> ) &&
				is_class_safely_constructible<TTarget, Arg0, Args...>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};

		// non-trivial default construction of classes is ok
		template <typename TTarget> requires (!std::is_trivially_default_constructible<TTarget>::value)
		struct is_value_safely_constructible_base<TTarget> : tc::constant<true> {
			static_assert(!std::is_reference<TTarget>::value);
		};

		template <typename TTarget, typename TSource> requires std::is_union<TTarget>::value
		struct is_value_safely_constructible_base<TTarget, TSource>
			// allow classes to control their convertibility to unions, we have conversion to CURRENCY somewhere
			: tc::constant<
				std::is_same<TTarget, std::remove_cvref_t<TSource>>::value || std::is_class<std::remove_reference_t<TSource>>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};

		// If TTarget is arithmetic
		//   - Character types must not be mixed with other arithmetic types.
		//   - Any arithmetic type (except character types) can be assigned to floating point.
		//   - Unsigned integral types can be assigned to any integral type if the upper bound is large enough.
		//   - Signed integral types can be assigned to signed integral types if upper and lower bounds are large enough.
		template<typename TTarget, typename TSource>
		struct is_arithmetic_value_safely_constructible final
			: tc::constant<false>
		{};

		template<typename TTarget, typename TSource> requires std::is_class<std::remove_reference_t<TSource>>::value
		struct is_arithmetic_value_safely_constructible<TTarget, TSource> final
			: tc::constant<true>
		{
			static_assert(std::is_arithmetic<TTarget>::value);
		};

		template<typename TSource> requires std::is_pointer<std::remove_reference_t<TSource>>::value || std::is_same<std::remove_cvref_t<TSource>, std::nullptr_t>::value
		struct is_arithmetic_value_safely_constructible<bool, TSource> final // explicit conversion from pointers/std::nullptr_t to bool is allowed in tc::no_adl::is_value_safely_constructible
			: tc::constant<true>
		{};

MODIFY_WARNINGS_BEGIN(
	((disable)(4018)) // signed/unsigned mismatch
	((disable)(4388)) // signed/unsigned mismatch
	((disable)(4804)) // '<=': unsafe use of type 'bool' in operation
)

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_between_arithmetic_values
			: tc::constant<
				std::is_same<tc::decay_t<TSource>, TTarget>::value // covers bool and various char types, which are only convertible within their own type
				|| (
					(
						(std::floating_point<TSource> && std::floating_point<TTarget>)
						||
						(tc::actual_integer<TSource> && tc::actual_integer<TTarget>)
					)
					&& (
						std::is_signed<TSource>::value
						?	std::is_signed<TTarget>::value
							&& std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
							&& std::numeric_limits<TTarget>::lowest() <= std::numeric_limits<TSource>::lowest()
						:	// conversion to unsigned (Warning 4018 and 4388) is ok here:
							std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
					)
				)
				|| (
					tc::actual_integer<TSource> && std::floating_point<TTarget>	&&
					std::numeric_limits<TTarget>::is_iec559 &&
					std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max() &&
					std::numeric_limits<TTarget>::lowest() <= std::numeric_limits<TSource>::lowest() &&
					std::numeric_limits<TSource>::digits <= std::numeric_limits<TTarget>::digits
				)
			>
		{
			static_assert(std::convertible_to<TSource, TTarget>);
		};

MODIFY_WARNINGS_END

		template<typename TTarget, typename TSource> requires std::is_arithmetic<std::remove_reference_t<TSource>>::value
		struct is_arithmetic_value_safely_constructible<TTarget, TSource> final
			: is_safely_convertible_between_arithmetic_values<std::remove_reference_t<TSource>, TTarget>
		{
			static_assert(std::is_arithmetic<TTarget>::value);
		};
	
		template <typename TTarget, typename TSource> requires std::is_arithmetic<TTarget>::value
		struct is_value_safely_constructible_base<TTarget, TSource>
			// disable unwanted arithmetic conversions
			: tc::constant<
				is_arithmetic_value_safely_constructible<TTarget, TSource>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};

#ifdef TC_MAC
		template<typename T>
		struct is_objc_block : tc::constant<false> {};

		template<typename R, typename... Args>
		struct is_objc_block<R (^)(Args...)> : tc::constant<true> {};
#endif

		template <typename TTarget, typename TSource>
		requires std::is_enum<TTarget>::value
			|| std::is_pointer<TTarget>::value
			|| std::is_member_pointer<TTarget>::value
			|| std::is_same<TTarget,std::nullptr_t>::value
#ifdef TC_MAC
			|| is_objc_block<TTarget>::value
#endif
		
		struct is_value_safely_constructible_base<TTarget, TSource>
			// std::is_constructible does the right thing for enums, pointers, and std::nullptr_t
			: tc::constant<true>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};

		template <typename TTarget, typename... Args>
		struct is_value_safely_constructible final : is_value_safely_constructible_base<TTarget, Args...> {}; // Has customizations

		template <typename TSource, typename TTarget>
		struct is_safely_convertible_to_reference final  {
			// creates no reference to temporary
			// For target references that may bind to temporaries, i.e., const&, &&, const&&
			// prevent initialization by
			//  - value -> (const)&&
			//  - value or (const)&& -> const&
			// binding to reference is only allowed to same type or derived to base conversion

			static auto constexpr value = 
				// 1. a mutable lvalue reference does not bind to temporary objects, so it is safe to allow it
				(std::is_lvalue_reference<TTarget>::value && !std::is_const<std::remove_reference_t<TTarget>>::value)
				|| 
				// 2. same type or derived to base (const)& -> const& does not bind to temporary objects
				// 3. same type or derived to base (const)& -> const&& does not bind to temporary objects
				// 4. same type or derived to base && -> (const)&& does not bind to temporary objects
				// 5. same type or derived to base const&& -> const&& does not bind to temporary objects
				(
					(
						std::is_lvalue_reference<TSource>::value
						|| (std::is_rvalue_reference<TSource>::value && std::is_rvalue_reference<TTarget>::value)
					)
					&&
					tc::derived_from<
						std::remove_reference_t<TSource>,
						std::remove_reference_t<TTarget>
					>
				);
		};
	}

	namespace safely_convertible_to_detail {
		template<typename TSource, typename TTarget>
		concept expanded_convertible_to =
			// 1. std::convertible_to
			std::convertible_to<TSource, TTarget> // void could only std::convertible_to or from void
			||
			// 2. or, if TTarget is const&&, consider std::convertible_to const& also convertible_to const&&. static_cast is needed in this case.
			(
				std::is_rvalue_reference<TTarget>::value &&
				std::is_const<std::remove_reference_t<TTarget>>::value &&
				std::convertible_to<TSource, std::remove_reference_t<TTarget>&>
			);

		// used when expanded_convertible_to is met
		template<typename TSource, typename TTarget>
		concept safely_convertible_to_reference = std::is_reference<TTarget>::value && no_adl::is_safely_convertible_to_reference<TSource, TTarget>::value;

		// used when expanded_convertible_to is met
		template<typename TSource, typename TTarget>
		concept safely_convertible_to_value =
			(!std::is_reference<TTarget>::value)
			&&
			!(
				std::same_as<std::remove_cv_t<TTarget>, bool> &&
				std::is_pointer<std::remove_reference_t<TSource>>::value
			) // pointers should not be implicitly convertible to bool (std::nullptr_t is already not std::convertible_to bool)
			&&
			no_adl::is_value_safely_constructible<std::remove_cv_t<TTarget>, TSource>::value;
	}

	// Disable unwanted conversions despite true==std::convertible_to<TSource, TTarget>
	// See some static_assert in type_traits.cpp.
	template<typename TSource, typename TTarget>
	concept safely_convertible_to =
		std::same_as<TTarget, TSource> // optimistically assume guaranteed copy elision
		||
		(
			safely_convertible_to_detail::expanded_convertible_to<TSource, TTarget>
			&&
			(
				safely_convertible_to_detail::safely_convertible_to_reference<TSource, TTarget>
				||
				safely_convertible_to_detail::safely_convertible_to_value<TSource, TTarget>
			)
		);

	// TODO: similar to std::convertible_to, implicit_uniform_construction_from should check if the function
	//		T F() { return { Arg1, Arg2, Arg3.... }; }
	// would compile. Currently, we only check the weaker expression
	//		G({ Arg1, Arg2, Arg3.... });
	// where G is a method accepting a value of T.
	namespace is_implicitly_constructible_detail {
		template<typename T>
		void check_construction(T); // unevaluated

		template<typename T, typename... Args>
		concept implicit_uniform_construction_from = requires { check_construction<T>({std::declval<Args>()...}); };

		template<typename TTarget, typename... Args>
		concept implicit_constructible_from_not_single_source =
			1 != sizeof...(Args) &&
			std::is_class<TTarget>::value &&
			implicit_uniform_construction_from<TTarget, Args...> &&
			tc::no_adl::is_value_safely_constructible<std::remove_cv_t<TTarget>, Args...>::value;

		template<typename TTarget, typename Arg0>
		concept implicit_constructible_from_single_source = tc::safely_convertible_to<Arg0,TTarget>;
	}

	template<typename TTarget, typename... Args>
	concept implicit_constructible_from =
		is_implicitly_constructible_detail::implicit_constructible_from_single_source<TTarget, tc::mp_only<boost::mp11::mp_list<Args...>>> ||
		is_implicitly_constructible_detail::implicit_constructible_from_not_single_source<TTarget, Args...>;

	namespace safely_constructible_from_detail {
		// We can't use std::is_constructible/std::constructible_from for types with private destructors.
		template<typename TTarget, typename ... Args>
		concept object_constructible_from = requires(Args&&... args) {
			// Note: Using the global placement new fails if TTarget is a reference type.
			::new(static_cast<void*>(nullptr)) TTarget(tc_move_if_owned(args)...);
		};

		template<typename TTarget, typename... Args>
		concept safely_constructible_from_not_single_source =
			// Require std::is_class<TTarget>:
			// - class types and const references to class types are is_constructible from two or more aguments. Initializing
			//	 a const reference using uniform initialization with multiple arguments would bind the reference to a temporary,
			//   which we do not allow,
			// - non-reference types may be std::is_constructible from zero arguments. We do not want this for native types like int.
			1!=sizeof...(Args) &&
			std::is_class<TTarget>::value &&
			object_constructible_from<TTarget, Args...> &&
			no_adl::is_value_safely_constructible<std::remove_cv_t<TTarget>, Args...>::value;

		template<typename TTarget, typename TSource>
		concept reference_safely_constructible_from =
			std::is_reference<TTarget>::value &&
			tc::safely_convertible_to<TSource, TTarget>;

		template<typename TTarget, typename TSource>
		concept value_safely_constructible_from_single_source =
			(!std::is_reference<TTarget>::value) &&
			(
				object_constructible_from<TTarget, TSource> ||
				std::is_same<TTarget, TSource>::value // optimistically assume guaranteed copy elision
			) &&
			(
				(
					std::floating_point<std::remove_cv_t<TTarget>> &&
					std::floating_point<std::remove_cvref_t<TSource>>
				) ||
				no_adl::is_value_safely_constructible<std::remove_cv_t<TTarget>, TSource>::value
			);

		template<typename TTarget, typename TSource>
		concept safely_constructible_from_single_source =
			reference_safely_constructible_from<TTarget, TSource> ||
			value_safely_constructible_from_single_source<TTarget, TSource>;
	}

	template<typename TTarget, typename... Args>
	concept safely_constructible_from =
		safely_constructible_from_detail::safely_constructible_from_single_source<TTarget, tc::mp_only<boost::mp11::mp_list<Args...>>> ||
		safely_constructible_from_detail::safely_constructible_from_not_single_source<TTarget, Args...>;

	template<typename TTarget, typename TSource>
	concept safely_assignable_from =
		std::is_assignable<TTarget, TSource>::value &&
		no_adl::is_value_safely_constructible<std::remove_reference_t<TTarget>, TSource>::value;
}

namespace tc {
	namespace no_adl {
		template <typename T, typename U>
		struct common_type_decayed_impl {}; // no common type
	}

	// 1. tc::common_type_t:
	//		a) input: any
	//		b) result: std::common_type_t of the decayed input types if the decayed types are tc::safely_convertible_to the result type.
	//		c) customization: Yes (many)
	//		d) SFINAE friendly: Yes
	template<typename... T>
	using common_type_t = tc::mp_fold_with_front<boost::mp11::mp_list<tc::decay_t<T>...>, boost::mp11::mp_quote_trait<no_adl::common_type_decayed_impl>::template fn>;

	namespace no_adl {
		template<typename T0, typename T1> requires
			tc::safely_convertible_to<T0, std::common_type_t<T0, T1>> &&
			tc::safely_convertible_to<T1, std::common_type_t<T0, T1>>
		struct common_type_decayed_impl<T0, T1> : std::common_type<T0, T1> {};

		template<typename T, typename U, U u>
		struct common_type_decayed_impl<T, std::integral_constant<U, u>> : common_type_decayed_impl<T, U> {};

		template<typename T, T t, typename U>
		struct common_type_decayed_impl<std::integral_constant<T, t>, U> : common_type_decayed_impl<T, U> {};

		template<typename T, T t, typename U, U u>
		struct common_type_decayed_impl<std::integral_constant<T, t>, std::integral_constant<U, u>> : std::conditional_t<
			t == u,
			std::type_identity<std::integral_constant<tc::common_type_t<T, U>, t>>,
			common_type_decayed_impl<T, U>
		> {};
	} // namespace no_adl

	namespace no_adl {
		template <typename T0, typename T1>
		struct common_base_fallback {};

		template <typename T>
		struct common_base_fallback<T, T> {
			using type = T;
		};

		template <typename T0, typename T1> requires tc::derived_from<T0, T1>
		struct common_base_fallback<T0, T1> {
			using type = T1;
		};

		template <typename T0, typename T1> requires tc::derived_from<T1, T0>
		struct common_base_fallback<T0, T1> {
			using type = T0;
		};

		template <typename T0, typename T1>
		struct common_base_impl : common_base_fallback<T0, T1> {};
	}

	// 2. tc::common_base_t:
	//		a) input: non-cvref-qualified types
	//		b) result: the most-derived common base class of the types
	//		c) customization: Yes (types derived from Obj)
	//		d) SFINAE friendly: Yes
	// Note: this is non-variadic as the default implementation is not-associative:
	// `common_base_t<common_base_t<Base, Derived1>, Derived2>` is `Base`, but `common_base_t<Base, common_base_t<Derived1, Derived2>>` does not exist.
	template <typename T0, typename T1> requires std::same_as<T0, std::remove_cvref_t<T0>> && std::same_as<T1, std::remove_cvref_t<T1>>
	using common_base_t = typename no_adl::common_base_impl<T0, T1>::type;

	namespace no_adl {
		template <typename T0, typename T1>
		struct actual_common_reference {};

		template <typename T0, typename T1>
			requires std::is_reference<T0>::value && std::is_reference<T1>::value
				&& requires { typename tc::common_base_t<std::remove_cvref_t<T0>, std::remove_cvref_t<T1>>; }
		struct actual_common_reference<T0, T1> {
			using T0Value = std::remove_reference_t<T0>;
			using T1Value = std::remove_reference_t<T1>;

			template<typename ValueType>
			using referenceness = std::conditional_t<
				std::is_lvalue_reference<T0>::value && std::is_lvalue_reference<T1>::value,
				ValueType&,
				ValueType&&
			>;

			template<typename ValueType>
			using constness = std::conditional_t<
				std::is_const<T0Value>::value || std::is_const<T1Value>::value || std::is_rvalue_reference<T0>::value != std::is_rvalue_reference<T1>::value,
				std::add_const_t<ValueType>,
				ValueType
			>;

			template<typename ValueType>
			using volatileness = std::conditional_t<
				std::is_volatile<T0Value>::value || std::is_volatile<T1Value>::value,
				std::add_volatile_t<ValueType>,
				ValueType
			>;

			using type = referenceness<constness<volatileness<common_base_t<std::remove_cv_t<T0Value>, std::remove_cv_t<T1Value>>>>>;
			static_assert(tc::safely_convertible_to<T0, type>);
			static_assert(tc::safely_convertible_to<T1, type>);
		};
	}

	template <typename ... T>
	using actual_common_reference_t = tc::mp_fold_with_front<boost::mp11::mp_list<T...>, boost::mp11::mp_quote_trait<no_adl::actual_common_reference>::template fn>;

	namespace no_adl {
		// This trait can be specialized to add further common reference types.
		template <typename T0, typename T1>
		struct common_reference_impl {};
		template <typename ... T>
		using common_reference_impl_t = tc::mp_fold_with_front<boost::mp11::mp_list<T...>, boost::mp11::mp_quote_trait<common_reference_impl>::template fn>;

		template <typename ... T>
		struct common_reference {};

		template <typename ... T> requires
			requires { typename actual_common_reference_t<T...>; }
		struct common_reference<T...> {
			using type = actual_common_reference_t<T...>;
		};

		template <typename ... T> requires
			requires { typename common_reference_impl_t<T...>; }
			&& (!requires { typename actual_common_reference_t<T...>; })
		struct common_reference<T...> {
			using type = common_reference_impl_t<T...>;
			static_assert((tc::safely_convertible_to<T, type> && ...));
		};

		template <typename ... T> requires
			(!requires { typename common_reference_impl_t<T...>; })
			&& (!requires { typename actual_common_reference_t<T...>; })
			&& requires { typename tc::common_type_t<T...>; }
		struct common_reference<T...> {
			using type = tc::common_type_t<T...>;
		};
	}

	// 3. tc::common_reference_t:
	//		a) input: any
	//		b) result:
	//			i) all types are references to the same/base/derived types: base type with correct cv ref (actual_common_reference)
	//			ii) if customized: user-defined type with reference semantics
	//			iii) else: tc::common_type_t
	//			Note: if input has at least 1 prvalue, the result (if it exists) will be a prvalue.
	//		c) customization: Yes (tc::span, tc::subrange)
	//		d) SFINAE friendly: Yes
	template <typename... T>
	using common_reference_t = typename no_adl::common_reference<T...>::type;

	namespace no_adl {
		template <typename T>
		struct has_operator_arrow final : tc::constant<false> {};
		template <typename T> requires
			requires { std::declval<T>().operator->(); }
				|| std::is_pointer<std::decay_t<T>>::value && (
					// Pseudo destructor access is legal for scalars types.
					std::is_class<std::remove_pointer_t<std::decay_t<T>>>::value ||
					std::is_union<std::remove_pointer_t<std::decay_t<T>>>::value ||
					std::is_scalar<std::remove_pointer_t<std::decay_t<T>>>::value
				)
		struct has_operator_arrow<T> final : tc::constant<true> {};
	}
	using no_adl::has_operator_arrow;

	template <typename...>
	struct dependent_false : tc::constant<false> {};

	namespace no_adl {
		struct sfinae_dependency_dummy; /*undefined*/

		template<typename T, typename DummyT>
		struct sfinae_dependent_type;
		
		template<typename T>
		struct sfinae_dependent_type<T, sfinae_dependency_dummy> final {
			using type = T;
		};

		template<typename T, typename DummyT>
		using sfinae_dependent_type_t = typename sfinae_dependent_type<T, DummyT>::type;
	}

#define ENABLE_SFINAE \
	typename EnableSfinaeDependencyT = tc::no_adl::sfinae_dependency_dummy

#define SFINAE_TYPE(...) \
	tc::no_adl::sfinae_dependent_type_t<__VA_ARGS__, EnableSfinaeDependencyT>

#define SFINAE_VALUE(...) \
	(SFINAE_TYPE(void)(), __VA_ARGS__)

	namespace no_adl {
		template<typename F>
		struct is_noexcept_function : tc::constant<false> {};
		template<typename Ret, typename... Args>
		struct is_noexcept_function<Ret(Args...) noexcept> : tc::constant<true> {};
		template<typename Ret, typename... Args>
		struct is_noexcept_function<Ret(Args..., ...) noexcept> : tc::constant<true> {};

		template< typename T >
		struct is_noexcept_member_function_pointer_helper : tc::constant<false> {};
		template< typename T, typename U>
		struct is_noexcept_member_function_pointer_helper<T U::*> : is_noexcept_function<std::remove_cvref_t<T>> {};
 
		template< typename T >
		struct is_noexcept_member_function_pointer : is_noexcept_member_function_pointer_helper<T> {};
	}
	using no_adl::is_noexcept_function;
	using no_adl::is_noexcept_member_function_pointer;

	namespace no_adl {
		template<typename T, typename = void>
		struct is_equality_comparable : tc::constant<false> {};

		template<typename T> requires tc::safely_convertible_to<decltype(std::declval<T const&>() == std::declval<T const&>()), bool>
		struct is_equality_comparable<T> : tc::constant<true> {
			STATICASSERTSAME(
				decltype(std::declval<T const&>() == std::declval<T const&>()),
				decltype(std::declval<T const&>() != std::declval<T const&>())
			);
		};
	}
	using no_adl::is_equality_comparable;
}
