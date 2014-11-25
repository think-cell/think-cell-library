#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "Library/Utilities/remove_cvref.h"

#include <boost/range/value_type.hpp>

#include <type_traits>
#include <string>
#include <ctype.h>

namespace RANGE_PROPOSAL_NAMESPACE {
	template<typename T>
	std::size_t strlen( T const* pt ) {
		return std::char_traits<T>::length(pt);
	}

	// cast may not be necessary, but let's avoid problems even in case of user-defined T
	template< typename T >
	bool isasciidigit( T ch ) {
		return static_cast<T>('0')<=ch && ch<=static_cast<T>('9');
	}

	template< typename T >
	bool isasciiupper( T ch ) {
		return static_cast<T>('A')<=ch && ch<=static_cast<T>('Z');
	}

	template< typename T >
	bool isasciilower( T ch ) {
		return static_cast<T>('a')<=ch && ch<=static_cast<T>('z');
	}

	template< typename T >
	T toasciiupper( T ch ) {
		if( isasciilower(ch) ) {
			return static_cast<T>( ch-('a'-'A') );
		} else {
			return ch;
		}
	}
	template< typename T >
	T toasciilower( T ch ) {
		if( isasciiupper(ch) ) {
			return static_cast<T>( ch+('a'-'A') );
		} else {
			return ch;
		}
	}
}

namespace boost {
	#pragma push_macro("CHAR_RANGE")
	#define CHAR_RANGE( xchar ) \
		template<> \
		struct range_mutable_iterator<xchar*> { \
			typedef xchar* type; \
		}; \
		template<> \
		struct range_const_iterator<xchar*> { \
			typedef std::add_const<xchar>::type* type; \
		}; \
		template<> \
		struct range_mutable_iterator<xchar* const> { \
			typedef xchar* type; \
		}; \
		template<> \
		struct range_const_iterator<xchar* const> { \
			typedef std::add_const<xchar>::type* type; \
		}; \
		template<std::size_t N> \
		struct range_mutable_iterator<xchar[N]> { \
			typedef xchar* type; \
		}; \
		/* support array-of-unknown-bounds incomplete type */ \
		template<> \
		struct range_mutable_iterator<xchar[]> { \
			typedef xchar* type; \
		}; \
		template<std::size_t N> \
		struct range_const_iterator<xchar[N]> { \
			typedef std::add_const<xchar>::type* type; \
		}; \
		/* support array-of-unknown-bounds incomplete type */ \
		template<> \
		struct range_const_iterator<xchar[]> { \
			typedef std::add_const<xchar>::type* type; \
		}; \
		inline xchar* range_begin(xchar* pch) { \
			return pch; \
		} \
		template<std::size_t N> \
		inline xchar* range_begin(xchar (&ach)[N]) { \
			return ach; \
		} \
		inline xchar* range_end(xchar* pch) { \
			return pch+tc::strlen(pch); \
		} \
		template<std::size_t N> \
		inline xchar* range_end(xchar (&ach)[N]) { \
			std::size_t cch=tc::strlen(ach); \
			_ASSERT(cch<N); \
			return ach+cch; \
		}

	CHAR_RANGE(char)
	CHAR_RANGE(char const)
	CHAR_RANGE(wchar_t)
	CHAR_RANGE(wchar_t const)
#ifndef BOOST_NO_CXX11_CHAR16_T
		CHAR_RANGE(char16_t)
		CHAR_RANGE(char16_t const)
#endif
#ifndef BOOST_NO_CXX11_CHAR32_T
		CHAR_RANGE(char32_t)
		CHAR_RANGE(char32_t const)
#endif
	#pragma pop_macro("CHAR_RANGE")
}

// Include the following after overloading range_begin/range_end
// Clang already instantiates some calls to range_begin inside
// iterator_range.hpp and does not see the char* overloads otherwise
#ifdef TC_MAC
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#endif
#include <boost/range/iterator_range.hpp>
#include <boost/range/sub_range.hpp>
#include <boost/range/any_range.hpp>
#ifdef TC_MAC
#pragma clang diagnostic pop
#endif 

#if 105600 < BOOST_VERSION
// TODO: In 1.56.0, this breaks due to an ordering issue check if this is fixed, and the code which is the same 
// except for the namespace can be replaced by using the original from the include
#error "Check if still needed!"
#include <boost/range/has_range_iterator.hpp>
#define BOOST_156_HACK boost
#else
#define BOOST_156_HACK boost::boost_156
namespace boost {
	namespace boost_156 {
		namespace range_detail {
			BOOST_MPL_HAS_XXX_TRAIT_DEF(type)

			template<typename T, class Enabler = void>	struct has_range_iterator_impl : boost::mpl::false_ {};
			template<typename T>						struct has_range_iterator_impl < T, BOOST_DEDUCED_TYPENAME ::boost::enable_if < BOOST_DEDUCED_TYPENAME mpl::eval_if < is_const<T>,
				has_type<range_const_iterator<BOOST_DEDUCED_TYPENAME remove_const<T>::type> >,
				has_type<range_mutable_iterator<T> >
			> ::type > ::type >
			: boost::mpl::true_{};

			template<typename T, class Enabler = void>	struct has_range_const_iterator_impl : boost::mpl::false_ {};
			template<typename T>						struct has_range_const_iterator_impl<T, BOOST_DEDUCED_TYPENAME ::boost::enable_if< has_type<range_const_iterator<T> > >::type> : boost::mpl::true_{};
		} // namespace range_detail

		template<typename T> struct has_range_iterator : range_detail::has_range_iterator_impl < BOOST_DEDUCED_TYPENAME remove_reference<T>::type > {};
		template<typename T> struct has_range_const_iterator : range_detail::has_range_const_iterator_impl < BOOST_DEDUCED_TYPENAME remove_reference<T>::type > {};
	}
}
#endif

namespace RANGE_PROPOSAL_NAMESPACE {
	namespace is_char_detail {
		template< typename T >
		struct is_char: std::false_type {};
		template<>
		struct is_char<char>: std::true_type {};
		template<>
		struct is_char<wchar_t>: std::true_type {};
#ifndef BOOST_NO_CXX11_CHAR16_T
		template<>
		struct is_char<char16_t>: std::true_type {};
#endif
#ifndef BOOST_NO_CXX11_CHAR32_T
		template<>
		struct is_char<char32_t>: std::true_type {};
#endif
	}
	template< typename T >
	struct is_char: is_char_detail::is_char< typename std::remove_cv< T >::type > {};

	namespace is_range_with_iterators_detail {

		template< typename Rng >
		struct is_range_with_iterators : boost::mpl::or_<
				BOOST_156_HACK::has_range_const_iterator<Rng>,
				BOOST_156_HACK::has_range_iterator<Rng>
			>::type
		{};

		template<typename T>
		struct is_range_with_iterators<T*> : tc::is_char<T>::type {};

		template<typename T, std::size_t N>
		struct is_range_with_iterators<T[N]> : std::true_type {};

		/* support array-of-unknown-bounds incomplete type */
		template<typename T>
		struct is_range_with_iterators<T[]> : std::true_type {};
	}
	template<typename Rng>
	struct is_range_with_iterators : is_range_with_iterators_detail::is_range_with_iterators<typename tc::remove_cvref<Rng>::type> {};

	namespace is_char_range_impl {
		template<typename Rng, bool is_range_with_iterators>
		struct is_char_range2;
		
		template<typename Rng>
		struct is_char_range2<Rng, true> : is_char< typename boost::range_value<Rng>::type >::type {};

		template<typename Rng>
		struct is_char_range2<Rng, false> : std::false_type {};

		template<typename Rng>
		struct is_char_range1 : is_char_range2<Rng, is_range_with_iterators<Rng>::value >::type {};
	};
	template<typename Rng>
	struct is_char_range : is_char_range_impl::is_char_range1< typename tc::remove_cvref<Rng>::type > {};

	static_assert( is_char_range<wchar_t const* const>::value, "" );
	
	//////////////////////////////
	// Traversables are either containers or ranges, which are references to consecutive elements in containers.
	//
	//	Ranges are regular types with pointer copy-semantics:
	//	- Modifying a copied range itself leaves the original unchanged.
	//	- Modifying the pointed-to elements modifies the same elements the original is pointing to.
	//	They are expected to be lightweight and passed by value.
	//	Their iterators must not be live-limited to the range,
	//	which outlaws iterators holding pointers to their range.
	//	Otherwise, the following would be illegal:
	//	auto lowerbound( Range, t ) { return std::begin(equal_range( Range, t )); }
	//
	//	Containers are expected to be heavy and passed by reference.
	//	Their iterator life may be limited to the life of the container.
	//	They may not be copyable.
	//	Containers have deep constness, so iterators of const containers are const_iterators.

	// const_range<T>::type is the equivalent of T* -> T const*
	// It makes the pointee const.
	template< typename T >
	struct const_range;

	// boost::iterator_range and derived are ranges
	template<typename T, bool is_range_with_iterators>
	struct is_range_impl2;
	template<typename T>
	struct is_range_impl2<T, true> : std::integral_constant< bool, std::is_base_of< boost::iterator_range< typename boost::range_iterator<T>::type >, T >::value > {};
	template<typename T>
	struct is_range_impl2<T, false> : std::false_type {};
	template<typename T>
	struct is_range_impl : is_range_impl2<T, tc::is_range_with_iterators<T>::value >::type {};
	template<typename It>
	struct const_range<boost::iterator_range<It>> {
		typedef boost::iterator_range<typename const_iterator_<It>::type> type;
	};
	template< typename T >
	struct const_range<boost::sub_range<T>> {
		typedef boost::sub_range<T const> type;
	};
	template<
		class Value
		, class Traversal
		, class Reference
		, class Difference
		, class Buffer
	>
	struct const_range<boost::any_range<Value,Traversal,Reference,Difference,Buffer>> {
		typedef boost::any_range<Value,Traversal,Reference,Difference,Buffer> type;
	};
	template<
		class Value
		, class Traversal
		, class Reference
		, class Difference
		, class Buffer
	>
	struct const_range<boost::any_range<Value,Traversal,Reference&,Difference,Buffer>> {
		typedef boost::any_range<Value,Traversal,Reference const&,Difference,Buffer> type;
	};

	// pointers to zero-terminated strings are ranges
	template< typename T >
	struct is_range_impl<T*> : is_char<T>::type {};
	template< typename T >
	struct const_range<T*> {
		typedef T const* type;
	};
	// sub_ranges are ranges
	template< typename T >
	struct is_range_impl<sub_range<T>> : std::true_type {};
	template< typename T >
	struct const_range<sub_range<T>> {
		// sub_range of another range
		typedef sub_range<typename const_range<T>::type> type;
	};
	template< typename T >
	struct const_range<sub_range<T&>> {
		// sub_range of a container, which is expected to have deep constness.
		typedef sub_range<T const&> type;
	};
	// iterator_base is range
	template< typename It, typename ConstIt >
	struct is_range_impl<iterator_base<It,ConstIt>> : std::true_type {};
	template< typename It, typename ConstIt >
	struct const_range<iterator_base<It,ConstIt>> {
		typedef iterator_base<ConstIt,ConstIt> type;
	};

	template< typename T >
	struct is_range : is_range_impl< typename std::decay<T>::type > {};

	namespace range_by_value_impl {
		template< typename T >
		struct range_by_value_for_range {
			typedef typename std::decay<T>::type type;
		};
		template< typename T >
		struct range_by_value_for_range<T const&> {
			typedef typename tc::const_range< typename std::decay<T>::type >::type type;
		};
	}
	template< typename T >
	struct range_by_value {
		static_assert( !std::is_rvalue_reference<T>::value, "" );
		typedef typename boost::mpl::eval_if_c< is_range<T>::value
			, range_by_value_impl::range_by_value_for_range<T>
			, boost::mpl::identity<T>
		>::type type;
	};
}
