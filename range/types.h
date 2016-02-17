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

#include "range_defines.h"

namespace tc {

	template<bool, typename Base, typename T0, typename T1>
	struct base_or_common_type;

	template<typename Base, typename T0, typename T1>
	struct base_or_common_type<true, Base, T0, T1> {
		using type = Base;
	};

	template<typename Base, typename T0, typename T1>
	struct base_or_common_type<false, Base, T0, T1> {
		using type = std::common_type_t<T0, T1>;
	};

	template<
		typename T0,
		typename T1
	>
	struct reference_type final {
		using T0Value = std::remove_reference_t<T0>;
		using T1Value = std::remove_reference_t<T1>;

		template<typename ValueType>
		using constness = std::conditional_t<
			std::is_const<T0Value>::value ||
			std::is_const<T1Value>::value,
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
				std::is_same<std::remove_cv_t<T0Value>, std::remove_cv_t<T1Value>>::value,
				T0Value,
				std::conditional_t<
					std::is_base_of<T0Value, T1Value>::value,
					T0Value,
					std::conditional_t<
						std::is_base_of<T1Value, T0Value>::value,
						T1Value,
						void
					>
				>
			>
		>;

		using type = typename base_or_common_type<
			std::is_lvalue_reference<T0>::value &&
			std::is_lvalue_reference<T1>::value &&
			!std::is_same<base, void>::value,
			std::add_lvalue_reference_t<
				constness<
					volatileness<base>
				>
			>,
			T0,
			T1
		>::type;
	};

	template<typename Rng>
	using traversal_t =
		typename boost::iterator_traversal<
			typename boost::range_iterator<std::remove_reference_t<Rng>>::type
		>::type
	;


}