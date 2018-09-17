
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

//#include "generic_macros.h"
#include <boost/mpl/identity.hpp>
#include <type_traits>
#include <limits>

// Use as type of constructor arguments that are required for enabling / disabling constructor through SFINAE.
// To be replaced by template parameter default when Visual C++ supports template parameter defaults for functions.
struct unused_arg final {};

namespace tc {
	//////////////////////////
	// void_t

#ifdef __clang__
	// If {template< typename... Args > using void_t = void;} is used, the test fails to compile with:
	// error: redefinition of 'Foo<type-parameter-0-0, void>'
	//	struct Foo<T, void_t<typename T::type2>> {
	//         ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// note: previous definition is here
	//	struct Foo<T, void_t<typename T::type1>> {
	//         ^
	//------------------------------
	// see also https://bugs.llvm.org/show_bug.cgi?id=26086
	// The implementation at http://en.cppreference.com/w/cpp/types/void_t solves the problem
	template<typename... Args>
	struct make_void {
		using type = void;
	};

	template< typename... Args >
	using void_t = typename tc::make_void<Args...>::type;
#else
	using std::void_t;
#endif

	//////////////////////////
	// remove_cvref

	template<typename T>
	using remove_cvref_t = std::remove_cv_t< std::remove_reference_t<T> >;

	//////////////////////////
	// decay
	template<typename T>
	struct decay {
		using type = std::decay_t<T>; // must still do function-to-pointer
	};

	// forbid decaying of C arrays, they decay to pointers, very unlike std/tc::array
	template<typename T>
	struct do_not_decay_arrays {
	private:
		char dummy; // make non-empty, empty structs are preferred in make_sub_range_result
	};

#pragma push_macro("DECAY_ARRAY_IMPL")
#define DECAY_ARRAY_IMPL(cv) \
	template<typename T> \
	struct decay<T cv[]> { \
		using type=do_not_decay_arrays<T cv[]>; \
	}; \
	template<typename T,std::size_t N> \
	struct decay<T cv[N]> { \
		using type = do_not_decay_arrays<T cv[N]>; \
	};

	DECAY_ARRAY_IMPL(BOOST_PP_EMPTY())
#if defined(__clang__) || 190023725<=_MSC_FULL_VER
		DECAY_ARRAY_IMPL(const)
		DECAY_ARRAY_IMPL(volatile)
		DECAY_ARRAY_IMPL(const volatile)
#endif
#pragma pop_macro("DECAY_ARRAY_IMPL")

	template<typename T>
	struct decay<T volatile> {
		using type = typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T const> {
		using type = typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T const volatile> {
		using type = typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T&> {
		using type = typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T&&> {
		using type = typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	using decay_t = typename decay<T>::type;

	//	auto a=b; uses std::decay
	// <=>
	//	auto a=decay_copy(b); uses tc::decay_t
	template<typename T>
	constexpr tc::decay_t<T&&> decay_copy(T&& t) noexcept {
		return std::forward<T>(t);
	}
	
	/////////////////////////////
	// remove_rvalue_reference

	namespace no_adl {
		template <typename T>
		struct remove_rvalue_reference {
			using type=T;
		};
		template <typename T>
		struct remove_rvalue_reference<T&&> {
			using type=T;
		};
	}

	using no_adl::remove_rvalue_reference;

	template<typename T>
	using remove_rvalue_reference_t=typename tc::remove_rvalue_reference<T>::type;

	//////////////////////////
	// is_XXX

	namespace is_char_detail {
		template<typename T>
		struct is_char: std::false_type {};
		template<>
		struct is_char<char>: std::true_type {};
		template<>
		struct is_char<wchar_t>: std::true_type {};
		template<>
		struct is_char<char16_t>: std::true_type {};
		template<>
		struct is_char<char32_t>: std::true_type {};
	}
	template< typename T >
	struct is_char: is_char_detail::is_char< std::remove_cv_t< T > > {};

	template< typename T >
	using is_bool=std::is_same< std::remove_cv_t<T>, bool >;

	template< typename T >
	using is_decayed=std::is_same< T, tc::decay_t<T> >;

	template<typename T>
	using is_actual_integer=std::integral_constant< bool, std::is_integral<T>::value && !tc::is_char<T>::value && !tc::is_bool<T>::value >;

	template<typename T>
	using is_actual_arithmetic=std::integral_constant<bool, tc::is_actual_integer<T>::value || std::is_floating_point<T>::value>;

	//////////////////////////
	// is_base_of

	// Both boost::is_base_of<X,X> and std::is_base_of<X,X> inherit from true_type if and only if X is a class type.
	template<typename Base, typename Derived>
	struct is_base_of_impl : std::integral_constant< bool,
		std::is_same<Base,Derived>::value || std::is_base_of<Base, Derived>::value
	> {};
	
	template<typename Base, typename Derived>
	using is_base_of=tc::is_base_of_impl< std::remove_cv_t<Base>, std::remove_cv_t<Derived> >;
	
	template<typename Base, typename Derived>
	struct is_base_of_decayed : std::integral_constant< bool,
		tc::is_base_of< Base, tc::decay_t<Derived> >::value
	> {
		static_assert(tc::is_decayed<Base>::value);
	};

	//////////////////////////
	// is_safely_convertible/assignable/constructible

	// For references that may bind to temporaries, i.e.,
	// - TTarget const&
	// - TTarget &&
	// - TTarget const&&
	// prevent initialization by
	//  - TSource {&&}, since this would bind a temporary,
	//  - TSource {{const}&}, if an implicit conversion to a temporary TTarget takes place.
	//    As this cannot be tested, disallow initialization unless TTarget is base or same
	//    as TSource.
	template<typename TSource, typename TTarget>
	struct creates_no_reference_to_temporary : std::integral_constant<
		bool,
		(!std::is_rvalue_reference<TTarget>::value
			&& (!std::is_lvalue_reference<TTarget>::value || !std::is_const<std::remove_reference_t<TTarget>>::value) // a mutable lvalue reference does not bind to temporary objects, so it is safe to allow it
		)
		|| (std::is_lvalue_reference<TSource>::value && tc::is_base_of<
			std::remove_reference_t<TTarget>,
			std::remove_reference_t<TSource>
		>::value)
	> {
	};

	namespace no_adl {
		// If TTarget is arithmetic
		//   - Character types must not be mixed with other arithmetic types.
		//   - Any arithmetic type (except character types) can be assigned to floating point.
		//   - Unsigned integral types can be assigned to any integral type if the upper bound is large enough.
		//   - Signed integral types can be assigned to signed integral types if upper and lower bounds are large enough.
		template<typename TSource, typename TTarget, typename=void>
		struct is_safely_convertible_to_arithmetic_value
			: std::false_type
		{};

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_to_arithmetic_value<TSource, TTarget, std::enable_if_t<std::is_class<std::remove_reference_t<TSource>>::value>>
			: std::true_type
		{
			static_assert(std::is_arithmetic<TTarget>::value);
		};

#pragma warning(push)
#pragma warning(disable: 4018) // signed/unsigned mismatch
#pragma	warning(disable: 4804) // '<=': unsafe use of type 'bool' in operation

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_between_arithmetic_values
			: std::integral_constant<bool,
				std::is_same<std::remove_cv_t<TSource>, std::remove_cv_t<TTarget>>::value // covers bool and various char types, which are only convertible within their own type
				|| (
					tc::is_actual_arithmetic<TSource>::value
					&& (
						std::is_floating_point<TTarget>::value // TODO: disable subranged conversions (such as int to float, or double to float) ?
						|| (
							tc::is_actual_integer<TTarget>::value
							&& (	std::is_signed<TSource>::value
								?	std::is_signed<TTarget>::value
									&& std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
									&& std::numeric_limits<TTarget>::lowest() <= std::numeric_limits<TSource>::lowest()
								:	// conversion to unsigned (Warning 4018) is ok here:
									std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
							)
						)
					)
				)
			>
		{
			static_assert(std::is_convertible<TSource, TTarget>::value);
		};

#pragma warning(pop)

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_to_arithmetic_value<TSource, TTarget, std::enable_if_t<std::is_arithmetic<std::remove_reference_t<TSource>>::value>>
			: is_safely_convertible_between_arithmetic_values<std::remove_reference_t<TSource>, TTarget>
		{
			static_assert(std::is_arithmetic<TTarget>::value);
		};
	
		// dispatch requried because logical operators are not lazy during template instantiation
		template <typename TSource, typename TTarget, typename=void>
		struct is_safely_convertible_to_value : std::false_type {};

		template <typename TSource, typename TTarget>
		struct is_safely_convertible_to_value<TSource, TTarget, std::enable_if_t<std::is_class<TTarget>::value>>
			// std::is_convertible already checks for existing conversion operator/ctor
			// in addition, prevent accidental slicing
			: std::integral_constant<
				bool,
				std::is_same<std::remove_cv_t<TTarget>, tc::remove_cvref_t<TSource>>::value || !tc::is_base_of<TTarget, std::remove_reference_t<TSource>>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};


		template <typename TSource, typename TTarget>
		struct is_safely_convertible_to_value<TSource, TTarget, std::enable_if_t<std::is_arithmetic<TTarget>::value>>
			// disable unwanted arithmetic conversions
			: std::integral_constant<
				bool,
				is_safely_convertible_to_arithmetic_value<TSource, TTarget>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};


		template <typename TSource, typename TTarget>
		struct is_safely_convertible_to_value<TSource, TTarget, std::enable_if_t<std::is_enum<TTarget>::value || std::is_pointer<TTarget>::value || std::is_same<std::remove_cv_t<TTarget>,std::nullptr_t>::value >>
			// std::is_convertible does the right thing for enums, pointers, and std::nullptr_t
			: std::true_type
		{
			static_assert(!std::is_reference<TTarget>::value);
		};
	}

	// Disable unwanted conversions despite true==std::is_convertible<TSource, TTarget>::value
	// See some static_assert in type_traits.cpp.
	template<typename TSource, typename TTarget>
	struct is_safely_convertible : 
		std::integral_constant<
			bool,	
			std::is_convertible<TSource, TTarget>::value
			&& (
				std::is_reference<TTarget>::value
				? tc::creates_no_reference_to_temporary<TSource, TTarget>::value
				: no_adl::is_safely_convertible_to_value<TSource, TTarget>::value
			)
		>
	{};

	template<typename TTarget, typename TSource>
	struct is_safely_assignable final
		: std::integral_constant<
			bool,
			std::is_assignable<TTarget, TSource>::value
			&& no_adl::is_safely_convertible_to_value<TSource, std::remove_reference_t<TTarget>>::value
		>
	{};

	namespace no_adl {
		template <typename TSource, typename TTarget, typename=void>
		struct is_value_safely_constructible : std::false_type {};

		template <typename TSource, typename TTarget>
		struct is_value_safely_constructible<TSource, TTarget, std::enable_if_t<std::is_class<TTarget>::value>>
			// prevent slicing
			: std::integral_constant<
				bool,
				std::is_same<std::remove_cv_t<TTarget>, tc::remove_cvref_t<TSource>>::value || !tc::is_base_of<TTarget, std::remove_reference_t<TSource>>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};


		template <typename TSource, typename TTarget>
		struct is_value_safely_constructible<TSource, TTarget, std::enable_if_t<std::is_arithmetic<TTarget>::value>>
			// disable unwanted arithmetic conversions between bool, char, and "real" arithmetic types
			// TODO: disable subranged conversions, e.g., double -> int
			: std::integral_constant<
				bool,
				std::is_same<tc::remove_cvref_t<TSource>, std::remove_cv_t<TTarget>>::value
				|| std::is_class<std::remove_reference_t<TSource>>::value
				|| (tc::is_actual_arithmetic<std::remove_reference_t<TSource>>::value && tc::is_actual_arithmetic<TTarget>::value)
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};


		template <typename TSource, typename TTarget>
		struct is_value_safely_constructible<TSource, TTarget, std::enable_if_t<std::is_enum<TTarget>::value || std::is_pointer<TTarget>::value || std::is_same<std::remove_cv_t<TTarget>,std::nullptr_t>::value >>
			// std::is_constructible does the right thing for enums, pointers, and std::nullptr_t
			: std::true_type
		{
			static_assert(!std::is_reference<TTarget>::value);
		};
	}

	template<typename TTarget, typename ...Args>
	struct is_safely_constructible :
		std::integral_constant<
			bool,
			// Require std::is_class<TTarget>:
			// - class types and const references to class types are std::is_constructible from two or more aguments. Initializing
			//	 a const reference using uniform initialization with multiple arguments would bind the reference to a temporary,
			//   which we do not allow,
			// - non-reference types may be std::is_constructible from zero arguments. We do not want this for native types like int.
			std::is_class<TTarget>::value && std::is_constructible<TTarget, Args...>::value
		>
	{
		static_assert(1 != sizeof...(Args));
	};

	template<typename TTarget, typename TSource>
	struct is_safely_constructible<TTarget, TSource> :
		std::integral_constant<
			bool,
			std::is_constructible<TTarget, TSource>::value
			&& (
				std::is_reference<TTarget>::value
				? tc::creates_no_reference_to_temporary<TSource, TTarget>::value
				: no_adl::is_value_safely_constructible<TSource, TTarget>::value
			)
		>
	{};
}


////////////////////////////////////////////////
// initialization of TTarget member/element by TSource

constexpr int forbidden_construction = 0; // would initialize a reference to a temporary
constexpr int explicit_construction = 1;
constexpr int implicit_construction = 3; // we want to bit-and these values in order to find the minimum


// TODO: similar to std::is_convertible, implict_uniform_construction_from should check if the function
//		T F() { return { Arg1, Arg2, Arg3.... }; }
// would compile. Currently, we only check the weaker expression
//		G({ Arg1, Arg2, Arg3.... });
// where G is a method accepting a value of T.
namespace construction_resctrictiveness_detail {
	template<typename T>
	std::true_type check_construction(T); // unevaluated

	template<typename T, typename ...Args>
	decltype(check_construction<T>({ std::declval<Args>()... })) return_implict_uniform_construction_from(int); // unevaluated overload

	template<typename...>
	std::false_type return_implict_uniform_construction_from(...); // unevaluated overload
}

namespace no_adl {
	template<typename T, typename ...Args>
	struct implict_uniform_construction_from : decltype(construction_resctrictiveness_detail::return_implict_uniform_construction_from<T, Args...>(0)) {};
}

// Construction from 0 or >= 2 arguments. For single argument constructors, see specialization below
template<typename TTarget, typename ...Args>
struct construction_restrictiveness : std::integral_constant<
	int,
	// Require std::is_class<TTarget>:
	// - class types and const references to class types are std::is_constructible from two or more aguments. Initializing
	//	 a const reference using uniform initialization with multiple arguments would bind the reference to a temporary,
	//   which we do not allow,
	// - non-reference types may be std::is_constructible from zero arguments. We do not want this for native types like int.
	std::is_class<TTarget>::value && std::is_constructible<TTarget, Args...>::value
	? (
		no_adl::implict_uniform_construction_from<TTarget, Args...>::value
			? implicit_construction
			: explicit_construction
	)
	: forbidden_construction
> {
	static_assert(1!=sizeof...(Args));
};

// Similar to http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4387.html :
// - implicit construction == is_constructible && is_convertible,
// - explicit construction == is_constructible && !is_convertible.
// However, we make some unsafe conversions explicit or do not allow them at all.
// TODO: exlicit construction, if there is a sensible definition for tc::explicit_cast<TTarget>(TSource) 
template<typename TTarget, typename TSource>
struct construction_restrictiveness<TTarget, TSource> : std::integral_constant<
	int,
	tc::is_safely_constructible<TTarget, TSource>::value
		? (
			tc::is_safely_convertible<TSource, TTarget>::value
				? implicit_construction
				: explicit_construction
		)
		: forbidden_construction
> {
	static_assert(!std::is_rvalue_reference<TTarget>::value);
};

// initialize N elements of TTarget by forwarding one arg per element
template <typename TTarget, typename ...Args>
struct elementwise_construction_restrictiveness final {
private:
	template< int... nRestrictiveness >
	static constexpr int internal_value = (implicit_construction & ... & nRestrictiveness);
public:
	static constexpr int value = internal_value<construction_restrictiveness<TTarget, Args>::value...>;
};

namespace tc {
	/////////////////////////////////////////////////////////////////////////
	// type_holder

	template<typename... Ts>
	struct type_list final {};
}

namespace tc {
	namespace no_adl {
		template< template<typename...> class T, typename List, typename=void >
		struct apply_args { };

		template< template<typename...> class T, typename... Args >
		struct apply_args<T, tc::type_list<Args...>, tc::void_t<typename Args::type..., T<typename Args::type...>>> {
			using type = T<typename Args::type...>;
		};

		template< template<typename...> class T, typename... Args >
		using template_apply_identity = tc::no_adl::apply_args<T, tc::type_list<Args...>>;
	}
	using no_adl::template_apply_identity;

	namespace no_adl {
		template<typename... Args>
		struct common_type_decayed;
	}
	using no_adl::common_type_decayed;

	template<typename... Args>
	using common_type_decayed_t = typename tc::common_type_decayed<Args...>::type;

	template<typename... Args>
	using common_type_t = typename tc::common_type_decayed<tc::decay_t<Args>...>::type;

	namespace no_adl {
		template<typename T>
		struct common_type_decayed<T> {
			using type=T;
		};

		template<typename T0, typename T1, template<typename> class Condition>
		struct same_category final: std::integral_constant<bool, !(Condition<T0>::value || Condition<T1>::value) || Condition<std::common_type_t<T0, T1>>::value >
		{};

		template<typename T0, typename T1, typename=void>
		struct common_type_decayed_base_conversion {};

		template<typename T0, typename T1>
		struct common_type_decayed_base_conversion<
			T0,
			T1,
			std::enable_if_t<
				same_category<T0, T1, std::is_class>::value && // silent class conversion not allowed
				( // Object slicing is prevented
					!std::is_class<std::common_type_t<T0, T1>>::value || (
				 		(std::is_same<std::common_type_t<T0, T1>,T0>::value || !tc::is_base_of<std::common_type_t<T0, T1>,T0>::value) &&
				  		(std::is_same<std::common_type_t<T0, T1>,T1>::value || !tc::is_base_of<std::common_type_t<T0, T1>,T1>::value)
					)
				) &&
				same_category<T0, T1, tc::is_actual_arithmetic>::value && // silent integer conversion not allowed
				same_category<T0, T1, tc::is_bool>::value && // silent bool conversion not allowed
				same_category<T0, T1, tc::is_char>::value && // silent char conversion not allowed
				( // silent conversion from signed to unsigned is not allowed
					!tc::is_actual_integer<std::common_type_t<T0, T1>>::value ||
					std::is_signed<std::common_type_t<T0, T1>>::value ||
				 	(!std::is_signed<T0>::value && !std::is_signed<T1>::value)
				)
			>
		> {
			using type = std::common_type_t<T0, T1>;
		};

		template<typename T0, typename T1, typename=void>
		struct common_type_decayed_base {};

		// common_type is sfinae-friendly
		template<typename T0, typename T1>
		struct common_type_decayed_base<T0, T1, tc::void_t<decltype(std::declval<bool>() ? std::declval<T0>() : std::declval<T1>())>>: common_type_decayed_base_conversion<T0, T1> {};

		template<typename T0, typename T1>
		struct common_type_decayed<T0, T1> : common_type_decayed_base<T0, T1> {};

		template<typename T, T t>
		struct common_type_decayed<T, std::integral_constant<T, t>> {
			using type = std::common_type_t<T, std::integral_constant<T, t>>;
		};

		template<typename T, T t>
		struct common_type_decayed<std::integral_constant<T, t>, T> {
			using type = std::common_type_t<std::integral_constant<T, t>, T>;
		};

		template<typename T, T t1, T t2>
		struct common_type_decayed<std::integral_constant<T, t1>, std::integral_constant<T, t2>> {
#ifdef __clang__
			using type = std::common_type_t<std::integral_constant<T, t1>, std::integral_constant<T, t2>>;
#else
			using type = std::conditional_t<t1 == t2, std::integral_constant<T, t1>, T>;
#endif
		};

		// Patch for XCode 8 - std::common_type<void, void> should be void, but fails to compile
		template<>
		struct common_type_decayed<void, void> {
			using type = void;
		}; 

		template<typename T0, typename T1, typename... Args>
		struct common_type_decayed<T0, T1, Args...>
			: tc::template_apply_identity<tc::common_type_t, boost::mpl::identity<T0>, tc::common_type_decayed<T1, Args...>> { };

		template<typename... types>
		struct common_reference;

		template<typename T>
		struct common_reference<T> {
			using type = T;
		};
	} // namespace no_adl
	using no_adl::common_reference;

	template<typename... Args>
	using common_reference_t =
		typename common_reference<Args...>::type
	;

	template<bool bCondition, template<typename> class template_, typename T>
	using apply_if_t = std::conditional_t<
		bCondition,
		typename template_<T>::type,
		T
	>;

	namespace no_adl {
		template<typename T0Value, typename T1Value>
		using common_reference_base_type = std::remove_cv_t<
			std::conditional_t<
				tc::is_base_of<T0Value, T1Value>::value,
				T0Value,
				std::conditional_t<
					tc::is_base_of<T1Value, T0Value>::value,
					T1Value,
					void
				>
			>
		>;

		template<bool, typename T0, typename T1, typename Delayed>
		struct delayed_base_or_common_type;

		template<typename T0, typename T1, typename Delayed>
		struct delayed_base_or_common_type<false, T0, T1, Delayed> {
			using T0Value = std::remove_reference_t<T0>;
			using T1Value = std::remove_reference_t<T1>;

			template<typename ValueType>
			using referenceness = std::conditional_t<
				std::is_lvalue_reference<T0>::value && std::is_lvalue_reference<T1>::value,
				ValueType&,
				ValueType&&
			>;

			template<typename ValueType>
			using constness = tc::apply_if_t<
				std::is_const<T0Value>::value || std::is_const<T1Value>::value || std::is_rvalue_reference<T0>::value != std::is_rvalue_reference<T1>::value,
				std::add_const,
				ValueType
			>;

			template<typename ValueType>
			using volatileness = tc::apply_if_t<
				std::is_volatile<T0Value>::value ||
				std::is_volatile<T1Value>::value,
				std::add_volatile,
				ValueType
			>;

			using type = referenceness<
				constness<
					volatileness<common_reference_base_type<T0Value, T1Value>>
				>
			>;
		};

		template<typename T0, typename T1, typename Delayed>
		struct delayed_base_or_common_type<true, T0, T1, Delayed>: tc::common_type_decayed<tc::decay_t<T0>, tc::decay_t<T1>> {
		};

		template<typename T0, typename T1>
		struct common_reference<T0,T1>: 
			no_adl::delayed_base_or_common_type<
				!std::is_reference<T0>::value || !std::is_reference<T1>::value || std::is_same<no_adl::common_reference_base_type<std::remove_reference_t<T0>, std::remove_reference_t<T1>>, void>::value,
				T0, T1, void
			> {
		};

		template<typename T0, typename T1, typename... Args>
		struct common_reference<T0, T1, Args...>
			: tc::template_apply_identity<tc::common_reference_t, boost::mpl::identity<T0>, tc::common_reference<T1, Args...>> { };
	} // namespace no_adl

	template< typename Func >
	struct delayed_returns_reference_to_argument {
		using type=decltype(returns_reference_to_argument(std::declval<Func>())); // invoke ADL
	};

	namespace no_adl {
		template<typename Func, typename TargetExpr, typename... SourceExpr>
		struct transform_return final {
			static constexpr bool bDecay=std::conditional_t<
				!std::conjunction<std::is_reference<SourceExpr>...>::value && std::is_rvalue_reference<TargetExpr>::value
				, delayed_returns_reference_to_argument<Func>
				, std::false_type
			>::type::value;
			using type=std::conditional_t<
				bDecay
				, tc::decay_t<TargetExpr>
				, TargetExpr
			>;
		};
	}

	template<typename... Args>
	using transform_return_t = typename no_adl::transform_return<Args...>::type;

	template <typename...>
	struct dependent_false : std::false_type {};

	/////////////////////////////////////////////
	// is_instance

	namespace no_adl {
		template<template<typename...> class X, typename T> struct is_instance : public std::false_type {};
		template<template<typename...> class X, typename T> struct is_instance<X, T const> : public is_instance<X, T> {};
		template<template<typename...> class X, typename T> struct is_instance<X, T volatile> : public is_instance<X, T> {};
		template<template<typename...> class X, typename T> struct is_instance<X, T const volatile> : public is_instance<X, T> {};
		template<template<typename...> class X, typename... Y> struct is_instance<X, X<Y...>> : public std::true_type {};

		template<template<typename, typename, bool> class X, typename T> struct is_instance2 : public std::false_type {};
		template<template<typename, typename, bool> class X, typename T> struct is_instance2<X, T const> : public is_instance2<X, T> {};
		template<template<typename, typename, bool> class X, typename T> struct is_instance2<X, T volatile> : public is_instance2<X, T> {};
		template<template<typename, typename, bool> class X, typename T> struct is_instance2<X, T const volatile> : public is_instance2<X, T> {};
		template<template<typename, typename, bool> class X, typename Y1, typename Y2, bool b> struct is_instance2<X, X<Y1,Y2,b>> : public std::true_type {};
	}
	using no_adl::is_instance;
	using no_adl::is_instance2;
}
