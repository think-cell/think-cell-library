#pragma once

#include <type_traits>

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
}

