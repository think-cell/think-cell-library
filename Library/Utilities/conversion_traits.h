#pragma once

#include "remove_cvref.h"

#include <boost/numeric/conversion/conversion_traits.hpp>

#include <boost/mpl/if.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/zip_view.hpp>
#include <boost/mpl/unpack_args.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/min_element.hpp>

#include <type_traits>

// Use as type of constructor arguments that are required for enabling / disabling constructor through SFINAE.
// To be replaced by template parameter default when Visual C++ supports template parameter defaults for functions.
struct unused_arg {
	unused_arg() {}
};

/////////////
// conversion traits

// Both boost::is_base_of<X,X> and std::is_base_of<X,X> inherit from true_type if and only if X is a class type.
// Also, std::is_base_of requires Base and Derived to be complete.
namespace tc {
	template<typename Base, typename Derived>
	struct is_base_of :
		boost::mpl::or_<
			std::is_same< typename std::remove_cv<Base>::type, typename std::remove_cv<Derived>::type >,
			std::is_base_of<Base, Derived>
		>::type
	{};

	// true, if type can be converted without loss of precision
	// - conversion of builtin numeric type to user-defined type is allowed
	// - conversion of user-defined type to builtin numeric type is forbidden
	// if you want to enable conversion from a specific udt into a builtin,
	// specialize is_lossless_convertible
	template<typename TSource, typename TTarget>
	struct is_lossless_convertible :
		boost::mpl::and_<
			// cannot use VC's std::is_convertible, because it seems to be broken (is_convertible<int&, int> returns false)
			// Note: Still using std::is_convertible won't reduce compilation times anyway.
			std::is_convertible<TSource, TTarget>,  
			boost::mpl::not_<
				typename boost::numeric::conversion_traits<
					typename std::remove_reference< TTarget >::type,
					typename std::remove_reference< TSource >::type
				>::subranged
			>
		> {};

	// do not allow initialization of an A{const}& member by B{{const}&}, unless A is same or base of B
	// conversion would create a reference to a temporary object
	template<typename TSource, typename TTarget>
	struct creates_no_reference_to_temporary :
		boost::mpl::or_<
			boost::mpl::not_< std::is_reference<TTarget> >,
	#if !defined(_MSC_VER) || 1600 < _MSC_VER
			tc::is_base_of< 
				typename tc::remove_cvref<TTarget>::type,
				typename tc::remove_cvref<TSource>::type
			>
	#else
			boost::mpl::and_<
				tc::is_base_of< 
					typename tc::remove_cvref<TTarget>::type,
					typename tc::remove_cvref<TSource>::type
				>,
				// initializing rvalue reference to a non-class type creates a reference to temporary
				// https://connect.microsoft.com/VisualStudio/feedback/details/681998/#details
				boost::mpl::or_<
					boost::mpl::not_< std::is_rvalue_reference<TTarget> >,
					std::is_class< typename std::remove_reference<TTarget>::type >
				>
			>
	#endif
		> {};
}

////////////////////////////////////////////////
// initialization of TTarget by TSource

typedef boost::mpl::int_<0> forbidden_initialization_tag; // would initialize a reference to a temporary
typedef boost::mpl::int_<1> explicit_initialization_tag;
typedef boost::mpl::int_<3> implicit_initialization_tag; // we want to bit-and these values in order to find the minimum

template<typename TTarget, typename TSource>
struct initialization_tag {
	typedef typename boost::mpl::if_<
		tc::creates_no_reference_to_temporary< TSource, TTarget >,
		typename boost::mpl::if_<
			boost::mpl::and_<
				// Only true_, if implicit conversion exists.
				tc::is_lossless_convertible<TSource, TTarget>,
				// in expressions like ((condition) ? (expression of type T&) : (expression of type T))
				// prefer conversion from T& to T over conversion from T to T& 
				boost::mpl::or_<
					boost::mpl::not_< std::is_reference<TTarget> >,
					std::is_reference<TSource>
				>
			>,
			implicit_initialization_tag,
			explicit_initialization_tag
		>::type,
		forbidden_initialization_tag
	>::type type;
};


