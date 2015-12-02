#pragma once

#include <type_traits>
#include <boost/mpl/identity.hpp>

namespace tc {
	template<typename T>
	using remove_cvref_t=std::remove_cv_t< std::remove_reference_t<T> >;

	template<typename T>
	struct add_const_also_to_ref {
		using type = T const;
	};

	template<typename T>
	struct add_const_also_to_ref<T&> {
		using type = T const&;
	};

	template<typename T>
	struct add_const_also_to_ref<T&&> {
		using type = T const&&;
	};

	template<typename T>
	using add_const_also_to_ref_t=typename add_const_also_to_ref<T>::type;

	template<typename T>
	struct decay {
		using type=std::decay_t<T>; // must still do array-to-pointer, function-to-pointer
	};

	template<typename T>
	struct decay<T volatile> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T const> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T const volatile> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T&> {
		using type=typename tc::decay<T>::type; // recursive
	};
	
	template<typename T>
	struct decay<T&&> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	using decay_t=typename decay<T>::type;

	//	auto a=b; uses std::decay
	// <=>
	//	auto a=make_copy(b); uses tc::decay_t
	template<typename T>
	tc::decay_t<T&&> make_copy(T&& t) {
		return static_cast< tc::decay_t<T&&> >( std::forward<T>(t) );
	}
}

