
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt


#include "assert_defs.h"
#include "enum.h"
#include "explicit_cast.h"
#include "return_decltype.h"
#include "utility.h"
#include "type_traits.h"

namespace {
	namespace make_integer_sequence_test {
		STATICASSERTSAME((tc::make_integer_sequence<int, -1, 3>),( std::integer_sequence<int, -1, 0, 1, 2>));
		STATICASSERTSAME((tc::make_integer_sequence<int, 2, 2>), std::integer_sequence<int>);

		STATICASSERTSAME((tc::make_reverse_integer_sequence<int, -1, 3>), (std::integer_sequence<int, 2, 1, 0, -1>));
		STATICASSERTSAME((tc::make_reverse_integer_sequence<int, 2, 2>), std::integer_sequence<int>);
	}

	namespace is_contiguous_integer_sequence_test {
		static_assert(tc::is_contiguous_integer_sequence<std::make_index_sequence<0>>::value);
		static_assert(tc::is_contiguous_integer_sequence<std::make_index_sequence<1>>::value);
		static_assert(tc::is_contiguous_integer_sequence<std::make_index_sequence<2>>::value);
		static_assert(tc::is_contiguous_integer_sequence<std::make_index_sequence<10>>::value);

		static_assert(tc::is_contiguous_integer_sequence<tc::make_integer_sequence<int, 1, 1>>::value);
		static_assert(tc::is_contiguous_integer_sequence<tc::make_integer_sequence<int, 1, 5>>::value);
		static_assert(!tc::is_contiguous_integer_sequence<tc::make_reverse_integer_sequence<int, 1, 3>>::value);

		static_assert(tc::is_contiguous_integer_sequence<std::integer_sequence<int, -2, -1, 0, 1, 2, 3>>::value);
		static_assert(!tc::is_contiguous_integer_sequence<std::integer_sequence<int, 0, 2>>::value);
		static_assert(!tc::is_contiguous_integer_sequence<std::integer_sequence<int, 0, 2, 3>>::value);

		static_assert(!tc::is_contiguous_integer_sequence<int>::value);
	}

	namespace constant_test {
		static_assert( tc::constant<2>() == 2 );
		static_assert( std::is_same<tc::constant<2>, std::integral_constant<int,2>>::value );

		STATICASSERTSAME( (tc::common_type_t<tc::constant<2>, long long>), (long long) );
		STATICASSERTSAME( (tc::common_type_t<tc::constant<2>, tc::constant<3ll>>), (long long) );
		STATICASSERTSAME( (tc::common_type_t<tc::constant<2>, tc::constant<2ll>>), (tc::constant<2ll>) );

		DEFINE_ENUM(MyEnum,myenum,(ONE)(TWO))

		static_assert( tc::constant<myenumONE>() == myenumONE );
		static_assert( std::is_same<tc::constant<myenumONE>, std::integral_constant<MyEnum,myenumONE>>::value );

		STATICASSERTSAME( (tc::common_type_t<tc::constant<myenumONE>, tc::constant<myenumONE>>), (tc::constant<myenumONE>) );
		STATICASSERTSAME( (tc::common_type_t<tc::constant<myenumONE>, tc::constant<myenumTWO>>), (MyEnum) );

		static_assert( tc::explicit_cast<tc::constant<myenumONE>>(myenumONE) == myenumONE );
		STATICASSERTSAME( decltype(tc::explicit_cast<tc::constant<myenumONE>>(myenumONE)), tc::constant<myenumONE> );
	}

	namespace decltype_return_test {
		struct A {
			int a;
			void access_a() & noexcept {
				STATICASSERTSAME(decltype(a), int);
				STATICASSERTSAME(decltype((a)), int&);
			}
			int& b;
			void access_b() & noexcept {
				STATICASSERTSAME(decltype(b), int&);
				STATICASSERTSAME(decltype((b)), int&);
			}
			int&& c;
			void access_c() & noexcept {
				STATICASSERTSAME(decltype(c), int&&);
				STATICASSERTSAME(decltype((c)), int&);
			}
		};
	}
}
