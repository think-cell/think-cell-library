#pragma once

#ifndef BOOST_RESULT_OF_USE_DECLTYPE
	#define BOOST_RESULT_OF_USE_DECLTYPE
#endif
#include <boost/utility/result_of.hpp>

namespace tc {

#ifdef HAS_VARIADIC_TEMPLATES
	
	template<typename Func> struct result_of {
		static_assert(std::is_same< typename boost::result_of<Func>::type, 
									typename std::result_of<Func>::type
								  >::value, 
					  "Results of boost::result_of and std::result_of diverge (i.e. boost::result_of is most likely wrong). Please investigate before continuing");	
		typedef typename std::result_of<Func>::type type;
	};

#else
	
	template<typename Func> struct result_of;

	#define RESULT_OF_(z, n, _) \
		template<typename Func BOOST_PP_ENUM_TRAILING_PARAMS(n, typename A)> \
		struct result_of<Func(BOOST_PP_ENUM_PARAMS(n, A))> { \
			typedef typename boost::result_of< typename std::remove_reference<Func>::type(BOOST_PP_ENUM_PARAMS(n, A)) >::type type; \
		};
	
	BOOST_PP_REPEAT( BOOST_RESULT_OF_NUM_ARGS, RESULT_OF_, _ )

	#undef RESULT_OF_

#endif
}
