#pragma once

#include <type_traits>
#include <boost/mpl/identity.hpp>

namespace tc {
	template<typename T>
	struct remove_cvref {
		typedef typename std::remove_cv<
			typename std::remove_reference<T>::type
		>::type type;
	};

	template<typename T>
	struct add_const_also_to_ref {
		typedef T const type;
	};

	template<typename T>
	struct add_const_also_to_ref<T&> {
		typedef T const& type;
	};

	template<typename T>
	struct add_const_also_to_ref<T&&> {
		typedef T const&& type;
	};

	template<typename T>
	struct specialize_decay : boost::mpl::identity<T> {};

	template<typename T>
	struct decay : tc::specialize_decay<typename std::decay<T>::type> {};

	//	auto a=b; uses std::decay
	// <=>
	//	auto a=make_copy(b); uses tc::decay
	template<typename T>
	typename tc::decay<T&&>::type make_copy( T&& t ) {
		return typename tc::decay<T&&>::type( std::forward<T>(t) );
	}
}

