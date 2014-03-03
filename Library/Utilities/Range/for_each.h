#pragma once

#include "range_defines.h"

#include "break_or_continue.h"
#include "index_range.h"
#include "meta.h"

#include <vector>
#include <boost/optional.hpp>

namespace RANGE_PROPOSAL_NAMESPACE {

	//-------------------------------------------------------------------------------------------------------------------------
	// for_each

	template<typename Enum>
	struct prohibit_conversion {
#ifdef _DEBUG
		class type {
			enum ambiguous_enum { ambiguous_enum_value };
			operator ambiguous_enum() const {
				return ambiguous_enum_value;
			}
			Enum m_e;
		public:
			type(Enum e)
			: m_e(e) {}
			operator Enum() const {
				return m_e;
			}
		};
#else
		typedef Enum type;
#endif
	};

	template< typename Rng, typename Func >
	prohibit_conversion<break_or_continue>::type
	for_each(Rng && rng, Func && func) {
		return continue_if_void( ensure_index_range(std::forward<Rng>(rng)), std::forward<Func>(func));
	}

	template< typename Rng, typename Func >
	break_or_continue reverse_for_each(Rng && rng, Func && func) {
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type const itBegin=boost::begin(rng);
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itEnd=boost::end(rng);
		while( itEnd!=itBegin ) {
			--itEnd;
			if( break_==continue_if_void(func, *itEnd) ) return break_;
		}
		return continue_;
	}

	/////////////////////////////////////////////////////
	// for_each_may_remove_current

	template< typename T >
	struct is_vector : std::false_type {};

	template< typename Ty, typename Alloc >
	struct is_vector< std::vector<Ty, Alloc> > : std::true_type {};

	// enable_if to ensure that removal preserves iterators would be nice, but is difficult for adapted ranges.
	template< typename Rng, typename Func >
	break_or_continue for_each_may_remove_current(Rng && rng, Func func) {
		static_assert( !is_vector< typename std::remove_reference<Rng>::type >::value, "" ); // std::vector is among the types that cannot be used with for_each_may_remove_current
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it=boost::begin(rng);
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type const itEnd=boost::end(rng);
		while( it!=itEnd ) {
			if( break_==continue_if_void(func, *it++) ) return break_;
		}
		return continue_;
	}

	template< typename Rng, typename Func >
	break_or_continue reverse_for_each_may_remove_current(Rng && rng, Func && func) {
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type const itBegin=boost::begin(rng);
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it=boost::end(rng);
		if( it!=itBegin ) {
			--it;
			while( it!=itBegin ) {
				if( break_==continue_if_void(func, *it--) ) return break_;
			}
			return continue_if_void(func, *it);
		}
		return continue_;
	}

	namespace accumulate_detail {
		template< typename T, typename AccuOp >
		struct Fn {
			T * m_pt;
			AccuOp * m_paccuop;
			Fn( T & t, AccuOp & accuop )
			:  m_pt(std::addressof(t)), m_paccuop(std::addressof(accuop))
			{}
			template< typename S >
			break_or_continue operator()( S && s ) const {
				return continue_if_void( *m_paccuop, *m_pt, std::forward<S>(s) );
			}
		};
	}

	template< typename Rng, typename T, typename AccuOp >
	T accumulate( Rng && rng, T t, AccuOp accuop ) {
		ensure_index_range(std::forward<Rng>(rng))(accumulate_detail::Fn<T,AccuOp>(t,accuop));
		return t;
	}

	namespace accumulate_with_front_detail {
		template< typename T, typename AccuOp >
		struct Fn {
			boost::optional<T> & m_t;
			AccuOp & m_accuop;
			Fn( boost::optional<T> & t, AccuOp & accuop )
			:  m_t(t), m_accuop(accuop)
			{}
			template< typename S >
			break_or_continue operator()( S && s ) {
				if( m_t ) {
					return continue_if_void( m_accuop, *m_t, std::forward<S>(s) );
				} else {
					m_t=std::forward<S>(s);
					return continue_;
				}
			}
		};
	}

	template< typename Rng, typename AccuOp >
	boost::optional< typename boost::range_value< typename std::remove_reference<Rng>::type >::type > accumulate_with_front( Rng && rng, AccuOp accuop ) {
		typedef typename boost::range_value< typename std::remove_reference<Rng>::type >::type T;
		boost::optional<T> t;
		accumulate_with_front_detail::Fn<T,AccuOp> fn(t,accuop);
		ensure_index_range(std::forward<Rng>(rng))(fn);
		return t;
	}
}
