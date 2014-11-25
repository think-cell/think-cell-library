#pragma once

#include <type_traits>

/* 
	clang or rather clang's libc++ does not implement std::is_constructible correctly.
	See the bug report at http://llvm.org/bugs/show_bug.cgi?id=21574

	VS2013 has no library implementation anymore, it uses compiler intrinsics. 
	
	boost::is_convertible<S, T> is defined to be true when S can be explicitly converted to T,
	but in VS2013 boost::is_convertible uses the std::is_convertible compiler intrinsics 
	whcih only allows implict conversion.
	
	Therefore we always use the patched libc++ implementation of std::is_convertible.
	The complete special case when target type _Tp is a reference or scalar type has been
	removed.
*/
namespace tc {
	// fwd declaration
	template <class _Tp, class... _Args> struct is_constructible;

	namespace is_constructible_detail {
		template<typename, typename _Tp> struct __select_2nd { typedef _Tp type; };

		struct __any
		{
			__any(...);
		};

		// template <class T, class... Args> struct is_constructible;

		//      main is_constructible test

		template <class _Tp, class ..._Args>
		typename __select_2nd<decltype(std::move(_Tp(std::declval<_Args>()...))), std::true_type>::type
		__is_constructible_test(_Tp&&, _Args&& ...);

		template <class ..._Args>
		std::false_type
		__is_constructible_test(__any, _Args&& ...);

		template <class _Tp, class... _Args>
		struct internal_is_constructible // false, _Tp is not a scalar
			: public std::common_type
					 <
						 decltype(__is_constructible_test(std::declval<_Tp>(), std::declval<_Args>()...))
					 >::type
			{};

		//      function types are not constructible

		template <class _Rp, class... _A1, class... _A2>
		struct internal_is_constructible<_Rp(_A1...), _A2...>
				: public std::false_type
			{};
		
		//      Array types are default constructible if their element type
		//      is default constructible
		
		template <class _Ap, size_t _Np>
		struct internal_is_constructible<_Ap[_Np]>
		: public tc::is_constructible<typename std::remove_all_extents<_Ap>::type>
		{};
		
		//      Otherwise array types are not constructible by this syntax
		
		template <class _Ap, size_t _Np, class ..._Args>
		struct internal_is_constructible<_Ap[_Np], _Args...>
		: public std::false_type
		{};
		
		//      Incomplete array types are not constructible
		
		template <class _Ap, class ..._Args>
		struct internal_is_constructible<_Ap[], _Args...>
		: public std::false_type
		{};

		template <bool, class _Tp, class... _Args>
		struct __is_constructible_void_check
			: public internal_is_constructible< _Tp, _Args...>
			{};

		//      If any of T or Args is void, is_constructible should be false

		template <class _Tp, class... _Args>
		struct __is_constructible_void_check<true, _Tp, _Args...>
			: public std::false_type
			{};

		template <class ..._Args> struct __contains_void;

		template <> struct __contains_void<> : std::false_type {};

		template <class _A0, class ..._Args>
		struct __contains_void<_A0, _Args...>
		{
			static const bool value = std::is_void<_A0>::value ||
									  __contains_void<_Args...>::value;
		};
	}
	// is_constructible entry point

	template <class _Tp, class... _Args>
	struct is_constructible
		: public is_constructible_detail::__is_constructible_void_check<
						is_constructible_detail::__contains_void<_Tp, _Args...>::value
						|| std::is_abstract<_Tp>::value,
						_Tp, _Args...>
		{};
}

namespace test {
	struct S {
		template<typename T>
		explicit operator T() const;
	};

	struct T {
		explicit T(S const&);

		explicit operator int const&() const;
	};

	static_assert( tc::is_constructible<int, S>::value, "tc::is_constructible does not work with explicit templated cast operator");
	static_assert( tc::is_constructible<int const&, T>::value, "tc::is_constructible does not work with explicit cast operator");

	static_assert( tc::is_constructible<T, S>::value, "tc::is_constructible does not work with explicit ctor");
	

	static_assert( !tc::is_constructible<S, T>::value, "S is not constructible from T");
	static_assert( !tc::is_constructible<S, int>::value, "S is not constructible from int");

	static_assert( tc::is_constructible<int, double>::value, "");
	static_assert( tc::is_constructible<double, int>::value, "");
	static_assert( !tc::is_constructible<double, int, int>::value, "");
	static_assert( tc::is_constructible<double>::value, "");
}


// clang, msvc and gcc all disagree on the nameing of this trait. we use the official C++14 names, but in our namespace
namespace tc {
#if defined(_MSC_VER) && !defined(__clang__)
	template<typename T> struct is_trivially_destructible : std::has_trivial_destructor < T > {};
	template<typename T> struct is_trivially_default_constructible : std::has_trivial_default_constructor < T > {};
#elif defined (__GNUC__) && !defined(__clang__)  // may need to take the default branch in future gcc releases
	template<typename T> struct is_trivially_destructible : std::is_trivially_destructible<T> {};
	template<typename T> struct is_trivially_default_constructible : std::has_trivial_default_constructor < T > {};
#else // use standard conformant way
	template<typename T> struct is_trivially_destructible : std::is_trivially_destructible<T> {};
	template<typename T> struct is_trivially_default_constructible : std::is_trivially_default_constructible < T > {};
#endif 
}


