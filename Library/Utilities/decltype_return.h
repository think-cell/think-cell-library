#pragma once
#include <type_traits>

namespace tc {
	template< typename T >
	struct return_type {
		// lambdas returns always by value and std::decay their return type, so for values, we do the same
		typedef typename std::conditional<
			std::is_reference<T>::value,
			T,
			typename std::decay<T>::type
		>::type type;
	};
}

// Neither decltype((x)) nor decltype(x) determine the right type in all cases.
// So we static_assert that they agree, and if not, let the user disambiguate.
namespace {
	struct Foo {
		int const m_n;
		int const& Func() {
			static_assert( std::is_same< tc::return_type< decltype(m_n) >::type, int >::value, "" );
			static_assert( std::is_same< tc::return_type< decltype((m_n)) >::type, int const& >::value, "" );
			return m_n;
		}
		int Func2(int const n) {
			static_assert( std::is_same< tc::return_type< decltype(n) >::type, int >::value, "" );
			static_assert( std::is_same< tc::return_type< decltype((n)) >::type, int const& >::value, "" );
			return n;
		}
	};
}

template< typename T >
typename tc::return_type< T >::type AvoidTypename();

#define return_decltype(...) ->decltype( AvoidTypename< decltype(__VA_ARGS__) >() ) { \
	static_assert( std::is_same< decltype(__VA_ARGS__), decltype((__VA_ARGS__)) >::value, "Return value or reference?" ); \
	return (__VA_ARGS__); \
}
#define return_decltype_val(...) ->decltype( AvoidTypename< decltype(__VA_ARGS__) >() ) { \
	static_assert( !std::is_same< decltype(__VA_ARGS__), decltype((__VA_ARGS__)) >::value, "Return value or reference?" ); \
	static_assert( !std::is_reference< typename tc::return_type< decltype(__VA_ARGS__) >::type >::value, "" ); \
	return (__VA_ARGS__); \
}
#define return_decltype_ref(...) ->decltype( AvoidTypename< decltype((__VA_ARGS__)) >() ) { \
	static_assert( !std::is_same< decltype(__VA_ARGS__), decltype((__VA_ARGS__)) >::value, "Return value or reference?" ); \
	static_assert( std::is_reference< typename tc::return_type< decltype((__VA_ARGS__)) >::type >::value, "" ); \
	return (__VA_ARGS__); \
}
#define enable_if_return_decltype(cond,...) ->typename std::enable_if< (cond), typename tc::return_type< decltype(__VA_ARGS__) >::type >::type { \
	static_assert( std::is_same< decltype(__VA_ARGS__), decltype((__VA_ARGS__)) >::value, "Return value or reference?" ); \
	return (__VA_ARGS__); \
}
#define code_return_decltype(code,...) ->decltype( AvoidTypename< decltype(__VA_ARGS__) >() ) { \
	static_assert( std::is_same< decltype(__VA_ARGS__), decltype((__VA_ARGS__)) >::value, "Return value or reference?" ); \
	{code} \
	return (__VA_ARGS__); \
}
#define return_ctor(T,params) ->T { return T params ; }
#define enable_if_decltype_ctor(cond,T,params) -> typename std::enable_if< (cond), T >::type { return T params ; }
