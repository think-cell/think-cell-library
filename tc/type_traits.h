
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_list.h"

#include <boost/mpl/identity.hpp>
#include <type_traits>
#include <limits>
#include <vector>

// Use as type of constructor arguments that are required for enabling / disabling constructor through SFINAE.
// To be replaced by template parameter default when Visual C++ supports template parameter defaults for functions.
struct unused_arg final {};

namespace tc {
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
		char dummy; // make non-empty, empty structs are preferred in make_subrange_result
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

	namespace no_adl {
		template <typename TTarget, typename ArgList, typename=void>
		struct is_class_safely_constructible final : std::true_type {};

		// allow only copy construction from other classes, everything else we do ourselves
		template <typename T, typename A, typename Arg0, typename... Args>
		struct is_class_safely_constructible<std::vector<T,A>, tc::type::list<Arg0, Args...>> final
			: std::integral_constant<
				bool,
				0==sizeof...(Args) &&
				std::is_class<std::remove_reference_t<Arg0>>::value
			>
		{
		};

		// allow only copy construction from other classes, everything else we do ourselves
		template <typename C, typename T, typename A, typename Arg0, typename... Args>
		struct is_class_safely_constructible<std::basic_string<C,T,A>, tc::type::list<Arg0, Args...>> final
			: std::integral_constant<
				bool,
				0==sizeof...(Args) &&
				( std::is_class<std::remove_reference_t<Arg0>>::value || std::is_convertible<Arg0, C const*>::value )
			>
		{
		};

		template <typename TTarget, typename ArgList, typename=void>
		struct is_value_safely_constructible : std::false_type {};

		template <typename TTarget, typename Arg0, typename... Args>
		struct is_value_safely_constructible<TTarget, tc::type::list<Arg0, Args...>, std::enable_if_t<std::is_class<TTarget>::value>> final
			// prevent slicing
			: std::integral_constant<
				bool,
				( 0<sizeof...(Args) || std::is_same<TTarget, tc::remove_cvref_t<Arg0>>::value || !tc::is_base_of<TTarget, std::remove_reference_t<Arg0>>::value ) &&
				is_class_safely_constructible<TTarget, tc::type::list<Arg0, Args...>>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};

		// default construction of classes is ok
		template <typename TTarget>
		struct is_value_safely_constructible<TTarget, tc::type::list<>, std::enable_if_t<std::is_class<TTarget>::value>> final : std::true_type {
			static_assert(!std::is_reference<TTarget>::value);
		};

		template <typename TTarget, typename TSource>
		struct is_value_safely_constructible<TTarget, tc::type::list<TSource>, std::enable_if_t<std::is_union<TTarget>::value>> final
			// allow classes to control their convertibility to unions, we have conversion to CURRENCY somewhere
			: std::integral_constant<
				bool,
				std::is_same<TTarget, tc::remove_cvref_t<TSource>>::value || std::is_class<std::remove_reference_t<TSource>>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};

		// If TTarget is arithmetic
		//   - Character types must not be mixed with other arithmetic types.
		//   - Any arithmetic type (except character types) can be assigned to floating point.
		//   - Unsigned integral types can be assigned to any integral type if the upper bound is large enough.
		//   - Signed integral types can be assigned to signed integral types if upper and lower bounds are large enough.
		template<typename TSource, typename TTarget, typename=void>
		struct is_safely_convertible_to_arithmetic_value final
			: std::false_type
		{};

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_to_arithmetic_value<TSource, TTarget, std::enable_if_t<std::is_class<std::remove_reference_t<TSource>>::value>> final
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
				std::is_same<tc::decay_t<TSource>, TTarget>::value // covers bool and various char types, which are only convertible within their own type
				||
					(
						std::is_floating_point<TSource>::value && std::is_floating_point<TTarget>::value
						||
						tc::is_actual_integer<TSource>::value && tc::is_actual_arithmetic<TTarget>::value
					)
					&& (
						std::is_signed<TSource>::value
						?	std::is_signed<TTarget>::value
							&& std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
							&& std::numeric_limits<TTarget>::lowest() <= std::numeric_limits<TSource>::lowest()
						:	// conversion to unsigned (Warning 4018) is ok here:
							std::numeric_limits<TSource>::max() <= std::numeric_limits<TTarget>::max()
					)
			>
		{
			static_assert(std::is_convertible<TSource, TTarget>::value);
		};

#pragma warning(pop)

		template<typename TSource, typename TTarget>
		struct is_safely_convertible_to_arithmetic_value<TSource, TTarget, std::enable_if_t<std::is_arithmetic<std::remove_reference_t<TSource>>::value>> final
			: is_safely_convertible_between_arithmetic_values<std::remove_reference_t<TSource>, TTarget>
		{
			static_assert(std::is_arithmetic<TTarget>::value);
		};
	
		template <typename TTarget, typename TSource>
		struct is_value_safely_constructible<TTarget, tc::type::list<TSource>, std::enable_if_t<std::is_arithmetic<TTarget>::value>> final
			// disable unwanted arithmetic conversions
			: std::integral_constant<
				bool,
				is_safely_convertible_to_arithmetic_value<TSource, TTarget>::value
			>
		{
			static_assert(!std::is_reference<TTarget>::value);
		};

#ifdef TC_MAC
		template<typename T>
		struct is_objc_block : std::false_type {};

		template<typename R, typename ... Args>
		struct is_objc_block<R (^)(Args...)> : std::true_type {};
#endif

		template <typename TTarget, typename TSource>
		struct is_value_safely_constructible<TTarget, tc::type::list<TSource>, std::enable_if_t<std::is_enum<TTarget>::value || std::is_pointer<TTarget>::value || std::is_member_pointer<TTarget>::value || std::is_same<TTarget,std::nullptr_t>::value
#ifdef TC_MAC
			|| is_objc_block<TTarget>::value
#endif
		>> final
			// std::is_constructible does the right thing for enums, pointers, and std::nullptr_t
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
			// std::is_convertible or, if TTarget is const&&, consider std::is_convertible to const& also is_convertible to const&&. static_cast is needed in this case.
			// we expand std::is_convertible because
			(std::is_convertible<TSource, TTarget>::value || std::is_rvalue_reference<TTarget>::value && std::is_const<std::remove_reference_t<TTarget>>::value && std::is_convertible<TSource, std::remove_reference_t<TTarget>&>::value)
			&& (
				std::is_reference<TTarget>::value
				? ( // creates no reference to temporary
					// For target references that may bind to temporaries, i.e., const&, &&, const&&
					// prevent initialization by
					//  - value -> (const)&&
					//  - value or (const)&& -> const&
					// binding to reference is only allowed to same type or derived to base conversion

					// 1. a mutable lvalue reference does not bind to temporary objects, so it is safe to allow it
					std::is_lvalue_reference<TTarget>::value && !std::is_const<std::remove_reference_t<TTarget>>::value
					|| 
					// 2. same type or derived to base (const)& -> const& does not bind to temporary objects
					// 3. same type or derived to base (const)& -> const&& does not bind to temporary objects
					// 4. same type or derived to base && -> (const)&& does not bind to temporary objects
					// 5. same type or derived to base const&& -> const&& does not bind to temporary objects
					(std::is_lvalue_reference<TSource>::value || std::is_rvalue_reference<TSource>::value && std::is_rvalue_reference<TTarget>::value) &&
					tc::is_base_of<
						std::remove_reference_t<TTarget>,
						std::remove_reference_t<TSource>
					>::value
				)
				: no_adl::is_value_safely_constructible<std::remove_cv_t<TTarget>, tc::type::list<TSource>>::value
			)
		>
	{};

	template<>
	struct is_safely_convertible<void, void> : std::true_type {};

	// TODO: similar to std::is_convertible, implicit_uniform_construction_from should check if the function
	//		T F() { return { Arg1, Arg2, Arg3.... }; }
	// would compile. Currently, we only check the weaker expression
	//		G({ Arg1, Arg2, Arg3.... });
	// where G is a method accepting a value of T.
	namespace is_implicitly_constructible_detail {
		template<typename T>
		std::true_type check_construction(T); // unevaluated

		template<typename T, typename ...Args>
		decltype(check_construction<T>({ std::declval<Args>()... })) return_implicit_uniform_construction_from(int); // unevaluated overload

		template<typename...>
		std::false_type return_implicit_uniform_construction_from(...); // unevaluated overload
	}

	template<typename TTarget, typename ...Args>
	struct is_implicitly_constructible final :
		std::integral_constant<
			bool,
			decltype(is_implicitly_constructible_detail::return_implicit_uniform_construction_from<TTarget, Args...>(0))::value &&
			std::is_class<TTarget>::value &&
			no_adl::is_value_safely_constructible<std::remove_cv_t<TTarget>, tc::type::list<Args...>>::value
		>
	{
		static_assert(1 != sizeof...(Args));
	};

	template<typename TTarget, typename Arg0>
	struct is_implicitly_constructible<TTarget,Arg0> final : tc::is_safely_convertible<Arg0,TTarget> {};

	template<typename TTarget, typename... Args>
	struct is_safely_constructible final :
		std::integral_constant<
			bool,
			// Require std::is_class<TTarget>:
			// - class types and const references to class types are std::is_constructible from two or more aguments. Initializing
			//	 a const reference using uniform initialization with multiple arguments would bind the reference to a temporary,
			//   which we do not allow,
			// - non-reference types may be std::is_constructible from zero arguments. We do not want this for native types like int.
			std::is_class<TTarget>::value && std::is_constructible<TTarget, Args...>::value && no_adl::is_value_safely_constructible<std::remove_cv_t<TTarget>, tc::type::list<Args...>>::value
		>
	{
		static_assert(1 != sizeof...(Args));
	};

	template<typename TTarget, typename TSource>
	struct is_safely_constructible<TTarget, TSource> final :
		std::integral_constant<
			bool,
			std::is_reference<TTarget>::value
			? tc::is_safely_convertible<TSource, TTarget>::value
			: std::is_constructible<TTarget, TSource>::value && (
				no_adl::is_value_safely_constructible<std::remove_cv_t<TTarget>, tc::type::list<TSource>>::value ||
				std::is_floating_point<std::remove_cv_t<TTarget>>::value && std::is_floating_point<tc::remove_cvref_t<TSource>>::value
			)
		>
	{};

	template<typename TTarget, typename TSource>
	struct is_safely_assignable final
		: std::integral_constant<
			bool,
			std::is_assignable<TTarget, TSource>::value
			&& no_adl::is_value_safely_constructible<std::remove_reference_t<TTarget>, tc::type::list<TSource>>::value
		>
	{};
}

namespace tc {
	namespace no_adl {
		template<typename... Args>
		struct common_type_decayed;
	}
	using no_adl::common_type_decayed;

	template<typename... Args>
	using common_type_decayed_t = typename tc::common_type_decayed<Args...>::type;

	// 1. tc::common_type_t:
	//		a) input: any
	//		b) result: std::common_type_t of the decayed input types if the decayed types are tc::is_safely_convertible to the result type.
	//		c) customization: Yes (many)
	//		d) SFINAE friendly: Yes
	template<typename... Args>
	using common_type_t = typename tc::common_type_decayed<tc::decay_t<Args>...>::type;

	namespace no_adl {
		template<typename T>
		struct common_type_decayed<T> {
			using type=T;
		};

		template<typename T0, typename T1, typename=void>
		struct common_type_decayed_base_conversion {};

		template<typename T0, typename T1>
		struct common_type_decayed_base_conversion<
			T0,
			T1,
			std::enable_if_t<
				tc::is_safely_convertible<T0, std::common_type_t<T0, T1>>::value &&
				tc::is_safely_convertible<T1, std::common_type_t<T0, T1>>::value
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

		template<typename T0, typename T1, typename... Args>
		struct common_type_decayed<T0, T1, Args...>
			: tc::type::accumulate<tc::type::list<T0, T1, Args...>, tc::common_type_decayed_t> {};
	} // namespace no_adl

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

		template<typename T0, typename T1, typename Enable=void>
		struct guaranteed_common_reference final {};

		template<typename T0, typename T1>
		struct guaranteed_common_reference<T0, T1, std::enable_if_t<
			std::is_reference<T0>::value &&
			std::is_reference<T1>::value &&
			!std::is_same<common_reference_base_type<std::remove_reference_t<T0>, std::remove_reference_t<T1>>, void>::value
		>> final {
			static_assert(std::is_reference<T0>::value && std::is_reference<T1>::value);

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

			static_assert(tc::is_safely_convertible<T0, type>::value);
			static_assert(tc::is_safely_convertible<T1, type>::value);
		};

		template<typename TypeList, typename Enable=void>
		struct common_reference_xvalue_as_ref_common_type {};

		template<typename... Args>
		struct common_reference_xvalue_as_ref_common_type<tc::type::list<Args...>, std::enable_if_t<
			tc::type::all_of<tc::type::list<Args...>, std::is_reference>::value
		>>: tc::common_type_decayed<tc::decay_t<Args>...> {};

		template<typename T0, typename T1>
		using guaranteed_common_reference_t = typename guaranteed_common_reference<T0, T1>::type;

		template<typename TypeList, typename Enable=void>
		struct has_guaranteed_common_reference final: std::false_type {};

		template<typename TypeList>
		struct has_guaranteed_common_reference<TypeList, tc::void_t<typename tc::type::accumulate<TypeList, guaranteed_common_reference_t>::type>> final: std::true_type {};

		template<typename TypeList, typename Enable=void>
		struct common_reference_xvalue_as_ref final: common_reference_xvalue_as_ref_common_type<TypeList> {};

		template<typename TypeList>
		struct common_reference_xvalue_as_ref<TypeList, std::enable_if_t<
			has_guaranteed_common_reference<TypeList>::value
		>> final {
			using type = typename tc::type::accumulate<TypeList, guaranteed_common_reference_t>::type;
		};
	}

	// 2. tc::common_reference_xvalue_as_ref_t:
	//		a) input: reference types, prvalue is not allowed
	//		b) result:
	//			i) all types are same/base/derived types: base type with correct cv ref (guaranteed_common_reference)
	//			ii) else: tc::common_type_t
	//		c) customization: Yes (tc::ptr_range, tc::sub_range)
	//		d) SFINAE friendly: Yes
	template<typename... Args>
	using common_reference_xvalue_as_ref_t = typename no_adl::common_reference_xvalue_as_ref<tc::type::list<Args...>>::type;

	namespace no_adl {
		template< typename TypeList, typename Enable = void >
		struct has_common_reference_xvalue_as_ref /*final*/: std::false_type {};

		template< typename... Args >
		struct has_common_reference_xvalue_as_ref<tc::type::list<Args...>, tc::void_t<tc::common_reference_xvalue_as_ref_t<Args...>>> /*final*/: std::true_type {};
	}
	using no_adl::has_common_reference_xvalue_as_ref;

	namespace no_adl {
		template<typename TTarget>
		struct curried_is_safely_convertible /*final*/ {
			template<typename TSource>
			using type = tc::is_safely_convertible<TSource, TTarget>;
		};

		template<typename TypeList>
		struct common_reference_prvalue_as_val_base;

		template<typename... Args>
		struct common_reference_prvalue_as_val_base<tc::type::list<Args...>>: tc::common_type_decayed<tc::decay_t<Args>...> {};

		template<typename TypeList, typename Enable=void>
		struct common_reference_prvalue_as_val final: common_reference_prvalue_as_val_base<TypeList> {};

		template<typename... Args>
		struct common_reference_prvalue_as_val<tc::type::list<Args...>, std::enable_if_t<
			tc::type::all_of<
				tc::type::list<Args...>,
				curried_is_safely_convertible<tc::common_reference_xvalue_as_ref_t<Args&&...>>::template type
			>::value
		>> final {
			using type = tc::common_reference_xvalue_as_ref_t<Args&&...>;
		};
	}

	// 3. tc::common_reference_prvalue_as_val_t:
	//		a) input: any
	//		b) result:
	//			i) tc::common_reference_xvalue_as_ref_t<Args&&...> if all Input types are tc::is_safely_convertible to the result type.
	//			ii) else: tc::common_type_t
	//			iii) Note: if input has at least 1 prvalue, the result will be a prvalue or none. Because prvalues are not tc::is_safely_convertible to reference.
	//		c) customization: No
	//		d) SFINAE friendly: Yes
	template<typename... Args>
	using common_reference_prvalue_as_val_t = typename no_adl::common_reference_prvalue_as_val<tc::type::list<Args...>>::type;

	namespace no_adl {
		template< typename TypeList, typename Enable = void >
		struct has_common_reference_prvalue_as_val final: std::false_type {};

		template< typename... Args >
		struct has_common_reference_prvalue_as_val<tc::type::list<Args...>, tc::void_t<tc::common_reference_prvalue_as_val_t<Args...>>> final: std::true_type {};
	}

	using no_adl::has_common_reference_prvalue_as_val;

	namespace no_adl {
		template <typename T, typename = void>
		struct has_operator_arrow final : std::false_type {};
		template <typename T>
		struct has_operator_arrow<T, tc::void_t<decltype(std::declval<T>().operator->())>> final : std::true_type {};
	}
	using no_adl::has_operator_arrow;

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

	namespace no_adl {
		struct sfinae_dependency_dummy; /*undefined*/

		template<typename T, typename DummyT, typename Enable=void>
		struct sfinae_dependent_type;
		
		template<typename T, typename DummyT>
		struct sfinae_dependent_type<T, DummyT, std::enable_if_t<std::is_same<DummyT, sfinae_dependency_dummy>::value>> final {
			using type = T;
		};

		template<typename T, typename DummyT>
		using sfinae_dependent_type_t = typename sfinae_dependent_type<T, DummyT>::type;

		template<auto val, typename DummyT, typename Enable=void>
		struct sfinae_dependent_value;

		template<auto val, typename DummyT>
		struct sfinae_dependent_value<val, DummyT, std::enable_if_t<std::is_same<DummyT, sfinae_dependency_dummy>::value>> final {
			static constexpr auto value = val;
		};
	}

#define ENABLE_SFINAE \
	typename EnableSfinaeDependencyT = tc::no_adl::sfinae_dependency_dummy

#define SFINAE_TYPE(T) \
	tc::no_adl::sfinae_dependent_type_t<T, EnableSfinaeDependencyT>

#define SFINAE_VALUE(val) \
	tc::no_adl::sfinae_dependent_value<val, EnableSfinaeDependencyT>::value

	/////////////////////////////////////////////
	// is_instance

	namespace no_adl {
		template<template<typename...> class X, typename T> struct is_instance : public std::false_type {};
		template<template<typename...> class X, typename T> struct is_instance<X, T const> : public is_instance<X, T> {};
		template<template<typename...> class X, typename T> struct is_instance<X, T volatile> : public is_instance<X, T> {};
		template<template<typename...> class X, typename T> struct is_instance<X, T const volatile> : public is_instance<X, T> {};
		template<template<typename...> class X, typename... Y> struct is_instance<X, X<Y...>> : public std::true_type {
			using arguments = tc::type::list<Y...>;
		};

		template<template<typename, typename, bool> class X, typename T> struct is_instance2 : public std::false_type {};
		template<template<typename, typename, bool> class X, typename T> struct is_instance2<X, T const> : public is_instance2<X, T> {};
		template<template<typename, typename, bool> class X, typename T> struct is_instance2<X, T volatile> : public is_instance2<X, T> {};
		template<template<typename, typename, bool> class X, typename T> struct is_instance2<X, T const volatile> : public is_instance2<X, T> {};
		template<template<typename, typename, bool> class X, typename Y1, typename Y2, bool b> struct is_instance2<X, X<Y1,Y2,b>> : public std::true_type {
			using first_argument = Y1;
			using second_argument = Y2;
			static constexpr auto third_argument = b;
		};

		template<template<typename, bool> class X, typename T> struct is_instance3 : public std::false_type {};
		template<template<typename, bool> class X, typename T> struct is_instance3<X, T const> : public is_instance3<X, T> {};
		template<template<typename, bool> class X, typename T> struct is_instance3<X, T volatile> : public is_instance3<X, T> {};
		template<template<typename, bool> class X, typename T> struct is_instance3<X, T const volatile> : public is_instance3<X, T> {};
		template<template<typename, bool> class X, typename Y, bool b> struct is_instance3<X, X<Y, b>> : public std::true_type {
			using first_argument = Y;
			static constexpr auto second_argument = b;
		};
	}
	using no_adl::is_instance;
	using no_adl::is_instance2;
	using no_adl::is_instance3;

	namespace is_instance_or_derived_detail {
		namespace no_adl {
			template<template<typename...> class Template, typename... Args>
			struct is_instance_or_derived_found : std::true_type {
				using base_instance = Template<Args...>;
				using arguments = tc::type::list<Args...>;
			};

			template<template<typename...> class Template>
			struct CDetector {
				template<typename... Args>
				static is_instance_or_derived_found<Template, Args...> detector(Template<Args...>*);

				static std::false_type detector(...);
			};
		}
		using no_adl::CDetector;
	}
	template<template<typename...> class Template, typename T>
	using is_instance_or_derived = decltype(
		tc::is_instance_or_derived_detail::CDetector<Template>::detector(
			std::declval<tc::remove_cvref_t<T>*>()
		)
	);

	/////////////////////////////////////////////
	// apply_cvref

	template<typename Dst, typename Src>
	struct apply_cvref {
		static_assert( std::is_same< Src, tc::remove_cvref_t<Src> >::value && !std::is_reference_v<Src>, "Src must not be cv-qualified. Check if a template specialization of apply_cvref is missing." );
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
}
