//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include <boost/numeric/conversion/conversion_traits.hpp>

#include <boost/mpl/if.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/zip_view.hpp>
#include <boost/mpl/unpack_args.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/min_element.hpp>
#include <boost/mpl/identity.hpp>

#include <type_traits>

// Use if you have to check if an expression compiles, e.g., to check if an operator is defined, a cast is valid etc
#define TC_HAS_EXPR(name, expr) \
	template<typename U> \
	struct BOOST_PP_CAT(has_,name) { \
	private: \
		template<typename T> static auto test(int) -> decltype((expr), std::true_type()); \
		template<typename> static std::false_type test(...); \
	public: \
		static constexpr bool value = std::is_same<decltype(test<U>(0)), std::true_type>::value; \
	};

// Use as type of constructor arguments that are required for enabling / disabling constructor through SFINAE.
// To be replaced by template parameter default when Visual C++ supports template parameter defaults for functions.
struct unused_arg final {};

namespace tc {
	//////////////////////////
	// remove_cvref

	template<typename T>
	using remove_cvref_t = std::remove_cv_t< std::remove_reference_t<T> >;

	template<typename T>
	struct decay {
		using type = std::decay_t<T>; // must still do function-to-pointer
	};

	//////////////////////////
	// decay

	// forbid decaying of C arrays, they decay to pointers, very unlike std/tc::array
	template<typename T>
	struct do_not_decay_arrays {};
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
#undef DECAY_ARRAY_IMPL

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
	//	auto a=make_copy(b); uses tc::decay_t
	template<typename T>
	tc::decay_t<T&&> make_copy(T&& t) noexcept {
		return std::forward<T>(t);
	}
	
	/////////////////////////////
	// remove_rvalue_reference

	namespace remove_rvalue_reference_adl_barrier {
		template <typename T>
		struct remove_rvalue_reference {
			using type=T;
		};
		template <typename T>
		struct remove_rvalue_reference<T&&> {
			using type=T;
		};
	}

	using remove_rvalue_reference_adl_barrier::remove_rvalue_reference;

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
		static_assert(tc::is_decayed<Base>::value, "");
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
		!std::is_rvalue_reference<TTarget>::value
		&& (!std::is_lvalue_reference<TTarget>::value || !std::is_const<std::remove_reference_t<TTarget>>::value) // a mutable lvalue reference does not bind to temporary objects, so it is safe to allow it
		|| std::is_lvalue_reference<TSource>::value && tc::is_base_of< 
			std::remove_reference_t<TTarget>,
			std::remove_reference_t<TSource>
		>::value
	> {
	};

	namespace is_safely_convertible_adl_barrier {
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
			static_assert(std::is_arithmetic<TTarget>::value, "");
		};

#pragma warning(push)
#pragma warning(disable: 4018) // signed/unsigned mismatch
#pragma	warning(disable: 4804) // '<=': unsafe use of type 'bool' in operation

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_between_arithmetic_values
			: std::integral_constant<bool,
				std::is_same<std::remove_cv_t<TSource>, std::remove_cv_t<TTarget>>::value // covers bool and various char types, which are only convertible within their own type
				||
					tc::is_actual_arithmetic<TSource>::value
					&& (
						std::is_floating_point<TTarget>::value // TODO: disable subranged conversions (such as int to float, or double to float) ?
						||
						tc::is_actual_integer<TTarget>::value
						&&(	std::is_signed<TSource>::value
							?	std::is_signed<TTarget>::value
								&& std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
								&& std::numeric_limits<TTarget>::lowest() <= std::numeric_limits<TSource>::lowest()
							:	// conversion to unsigned (Warning 4018) is ok here:
								std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
						)
					)
			>
		{
			static_assert(std::is_convertible<TSource, TTarget>::value, "");
		};

#pragma warning(pop)

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_to_arithmetic_value<TSource, TTarget, std::enable_if_t<std::is_arithmetic<std::remove_reference_t<TSource>>::value>>
			: is_safely_convertible_between_arithmetic_values<std::remove_reference_t<TSource>, TTarget>
		{
			static_assert(std::is_arithmetic<TTarget>::value, "");
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
			static_assert(!std::is_reference<TTarget>::value, "");
		};


		template <typename TSource, typename TTarget>
		struct is_safely_convertible_to_value<TSource, TTarget, std::enable_if_t<std::is_arithmetic<TTarget>::value>>
			// disable unwanted arithmetic conversions
			: std::integral_constant<
				bool,
				is_safely_convertible_to_arithmetic_value<TSource, TTarget>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value, "");
		};


		template <typename TSource, typename TTarget>
		struct is_safely_convertible_to_value<TSource, TTarget, std::enable_if_t<std::is_enum<TTarget>::value || std::is_pointer<TTarget>::value >>
			// std::is_convertible does the right thing for enums
			: std::true_type
		{
			static_assert(!std::is_reference<TTarget>::value, "");
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
				: is_safely_convertible_adl_barrier::is_safely_convertible_to_value<TSource, TTarget>::value
			)
		>
	{};

	template<typename TTarget, typename TSource>
	struct is_safely_assignable final
		: std::integral_constant<
			bool,
			std::is_assignable<TTarget, TSource>::value
			&& is_safely_convertible_adl_barrier::is_safely_convertible_to_value<TSource, std::remove_reference_t<TTarget>>::value
		>
	{};

	namespace is_safely_constructible_adl_barrier {
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
			static_assert(!std::is_reference<TTarget>::value, "");
		};


		template <typename TSource, typename TTarget>
		struct is_value_safely_constructible<TSource, TTarget, std::enable_if_t<std::is_arithmetic<TTarget>::value>>
			// disable unwanted arithmetic conversions between bool, char, and "real" arithmetic types
			// TODO: disable subranged conversions, e.g., double -> int
			: std::integral_constant<
				bool,
				std::is_same<tc::remove_cvref_t<TSource>, std::remove_cv_t<TTarget>>::value
				|| std::is_class<std::remove_reference_t<TSource>>::value
				|| tc::is_actual_arithmetic<std::remove_reference_t<TSource>>::value && tc::is_actual_arithmetic<TTarget>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value, "");
		};


		template <typename TSource, typename TTarget>
		struct is_value_safely_constructible<TSource, TTarget, std::enable_if_t<std::is_enum<TTarget>::value || std::is_pointer<TTarget>::value >>
			// std::is_constructible does the right thing for enums
			: std::true_type
		{
			static_assert(!std::is_reference<TTarget>::value, "");
		};
	}

	template<typename TTarget, typename TSource>
	struct is_safely_constructible : 
		std::integral_constant<
			bool,	
			std::is_constructible<TTarget, TSource>::value
			&& (
				std::is_reference<TTarget>::value
				? tc::creates_no_reference_to_temporary<TSource, TTarget>::value
				: is_safely_constructible_adl_barrier::is_value_safely_constructible<TSource, TTarget>::value
			)
		>
	{};
}


////////////////////////////////////////////////
// initialization of TTarget member/element by TSource

constexpr int forbidden_construction = 0; // would initialize a reference to a temporary
constexpr int explicit_construction = 1;
constexpr int implicit_construction = 3; // we want to bit-and these values in order to find the minimum

namespace construction_resctrictiveness_adl_barrier {
	// TODO: similar to std::is_convertible, implict_uniform_construction_from should check if the function
	//		T F() { return { Arg1, Arg2, Arg3.... }; }
	// would compile. Currently, we only check the weaker expression
	//		G({ Arg1, Arg2, Arg3.... });
	// where G is a method accepting a value of T.
	template<typename T>
	std::true_type check_construction(T); // unevaluated

	template<typename T, typename ...Args>
	decltype(check_construction<T>({ std::declval<Args>()... })) return_implict_uniform_construction_from(int); // unevaluated overload

	template<typename...>
	std::false_type return_implict_uniform_construction_from(...); // unevaluated overload

	template<typename T, typename ...Args>
	struct implict_uniform_construction_from : decltype(return_implict_uniform_construction_from<T, Args...>(0)) {};
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
		construction_resctrictiveness_adl_barrier::implict_uniform_construction_from<TTarget, Args...>::value
			? implicit_construction
			: explicit_construction
	)
	: forbidden_construction
> {
	static_assert(1!=sizeof...(Args), "");
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
	static_assert(!std::is_rvalue_reference<TTarget>::value, "");
};

// WORKAROUND until compiler supports C++17 fold expressions
#define FOLD_EXPRESSION(op, type, neutral, name) \
template <type ...args> \
struct fold_expression_ ## name; \
\
template <> \
struct fold_expression_ ## name<> \
	: std::integral_constant<type, neutral> \
{}; \
\
template <type head, type ...tail> \
struct fold_expression_ ## name<head, tail...> \
	: std::integral_constant<type, head op fold_expression_ ## name <tail...>::value > \
{};

FOLD_EXPRESSION(&, int, -1, bitwise_and) // equivalent to ( args && ... ) , currently only for int

// initialize N elements of TTarget by forwarding one arg per element
template <typename TTarget, typename ...Args>
struct elementwise_construction_restrictiveness final
	: fold_expression_bitwise_and<construction_restrictiveness<TTarget, Args>::value...>
{};

FOLD_EXPRESSION(|, int, 0, bitwise_or);

namespace tc {
	template<typename... Args>
	struct common_type_decayed;

	template<typename... Args>
	using common_type_decayed_t = typename common_type_decayed<Args...>::type;

	template<typename... Args>
	using common_type_t = typename common_type_decayed<typename tc::decay<Args>::type...>::type; // decay_t triggers a compiler bug https://connect.microsoft.com/VisualStudio/Feedback/Details/2173269 when used from tc::make_array (passing parameter pack from template alias to template alias)

	template<typename T>
	struct common_type_decayed<T> {
		using type=T;
	};

	template<typename T0, typename T1>
	struct common_type_decayed<T0, T1> {
		using type=std::common_type_t<T0,T1>;

		template< template<typename> class Condition>
		struct same_category : std::integral_constant<bool, !(Condition<T0>::value || Condition<T1>::value) || Condition<type>::value > {
		};

		static_assert(
			same_category<std::is_class>::value,
			"silent class conversion not allowed"
		);

		static_assert(
			!std::is_class<type>::value ||
			(std::is_same<type,T0>::value || !tc::is_base_of<type,T0>::value) && (std::is_same<type,T1>::value || !tc::is_base_of<type,T1>::value),
			"Object slicing is prevented"
		);

		static_assert(
			same_category<tc::is_actual_arithmetic>::value,
			"silent integer conversion not allowed"
		);

		static_assert(
			same_category<tc::is_bool>::value,
			"silent bool conversion not allowed"
		);

		static_assert(
			same_category<tc::is_char>::value,
			"silent char conversion not allowed"
		);

		static_assert(
			!tc::is_actual_integer<type>::value ||
			std::is_signed<type>::value ||
			!std::is_signed<T0>::value &&
			!std::is_signed<T1>::value,
			"silent conversion from signed to unsigned is not allowed"
		);
	};

	template<typename T0, typename T1, typename... Args>
	struct common_type_decayed<T0, T1, Args...> {
		using type=common_type_decayed_t<T0, common_type_decayed_t<T1, Args...>>;
	};

	template<typename... types>
	struct common_reference;

	template<typename... Args>
	using common_reference_t =
		typename common_reference<Args...>::type
	;

	template<
		typename T0,
		typename T1
	>
	struct common_reference<T0,T1> {
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
			std::is_const<T0Value>::value || std::is_const<T1Value>::value
				|| std::is_rvalue_reference<T0>::value != std::is_rvalue_reference<T1>::value,
			std::add_const_t<ValueType>,
			ValueType
		>;

		template<typename ValueType>
		using volatileness = std::conditional_t<
			std::is_volatile<T0Value>::value ||
			std::is_volatile<T1Value>::value,
			std::add_volatile_t<ValueType>,
			ValueType
		>;

		using base = std::remove_cv_t<
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

		template<bool, typename Delayed>
		struct delayed_base_or_common_type;

		template<typename Delayed>
		struct delayed_base_or_common_type<false, Delayed> {
			using type = referenceness<
				constness<
					volatileness<base>
				>
			>;
		};

		template<typename Delayed>
		struct delayed_base_or_common_type<true, Delayed> {
			using type = tc::common_type_t<T0, T1>;
		};

		using type = typename delayed_base_or_common_type<
			!std::is_reference<T0>::value || !std::is_reference<T1>::value
				|| std::is_same<base, void>::value,
			void
			>::type;
	};

	template<typename head, typename... tail>
	struct common_reference<head,tail...> : common_reference<head, common_reference_t<tail...>> {};

	namespace other_trait_operations_adl_barrier {
		// c++17 std::conjunction
		template<typename...>
		struct conjunction : std::true_type { };

		template<typename B1> struct conjunction<B1> : B1 { };

		template<typename B1, typename... Bn>
		struct conjunction<B1, Bn...> : std::conditional_t<B1::value != false, conjunction<Bn...>, B1>  {};

		// c++17 std::disjunction
		template<typename...>
		struct disjunction : std::false_type { };

		template<typename B1> struct disjunction<B1> : B1 { };

		template<typename B1, typename... Bn>
		struct disjunction<B1, Bn...> : std::conditional_t<B1::value != false, B1, disjunction<Bn...>>  {};
	}
	using other_trait_operations_adl_barrier::conjunction;
	using other_trait_operations_adl_barrier::disjunction;

	template< typename Func >
	struct delayed_returns_reference_to_argument {
		using type=decltype(returns_reference_to_argument(std::declval<Func>())); // invoke ADL
	};

	namespace transform_return_adl_barrier {
		template<typename Func, typename TargetExpr, typename... SourceExpr>
		struct transform_return final {
			static constexpr bool bDecay=boost::mpl::eval_if_c<
				!tc::conjunction<std::is_reference<SourceExpr>...>::value && std::is_rvalue_reference<TargetExpr>::value
				, delayed_returns_reference_to_argument<Func>
				, boost::mpl::identity<std::false_type>
			>::type::value;
			using type=std::conditional_t<
				bDecay
				, tc::decay_t<TargetExpr>
				, TargetExpr
			>;
		};
	}

	template<typename... Args>
	using transform_return_t = typename transform_return_adl_barrier::transform_return<Args...>::type;
}