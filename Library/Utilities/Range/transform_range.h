#pragma once

#include "range_defines.h"
#include "range_fwd.h"

#include "range_adaptor.h"
#include "meta.h"

#include "Library/ErrorReporting/tc_move.h"
#include "transform.h"

namespace RANGE_PROPOSAL_NAMESPACE {
	namespace transform_range_impl {
		class transform_range_access {
		public:
			template< typename Func, typename Rng, bool bHasIterator >
			static Func && get_func( transform_range<Func,Rng,bHasIterator> && rng ) {
				return tc_move(tc_move(rng).m_func);
			}
		};

		template< typename Func, typename Rng >
		class transform_range<Func,Rng,false> : public range_adaptor<transform_range<Func,Rng>, Rng, typename std::is_empty<Func>::type > {
			typedef range_adaptor<transform_range<Func,Rng>, Rng, typename std::is_empty<Func>::type > base_;

		protected:
			typedef base_ range_adaptor;

			static_assert( !std::is_reference<Func>::value, "" );
			static_assert( !std::is_const<Func>::value, "" );
			Func m_func;

		private:
			friend class range_adaptor_impl::range_adaptor_access;
			friend class transform_range_impl::transform_range_access;
	#define PART1() \
			template< typename Apply, 
	#define PART2() \
				> break_or_continue apply( Apply && apply, 
	#define PART3() ) const { \
					return continue_if_void( std::forward<Apply>(apply), m_func(
	#define PART4() ) ); \
				}
	PERFECT_FORWARD
	#undef PART1
	#undef PART2
	#undef PART3
	#undef PART4

		public:
			typedef void ctor_const_overload_support;
		
			// default ctor
			transform_range() {}

			// Range adaptors other than sub_ranges should not be copyable.
			// Otherwise, copying a range adaptor may have value or reference semantics,
			// depending whether the base range is by-value or by-reference.
			// Instead, the caller should explicit decide, and either
			// - store a reference or sub_range of the range,
			// - or copy the values into another container.

			transform_range( transform_range && rng )
				: base_(tc_move(rng).base_range_move(), aggregate_tag())
				, m_func(tc_move(rng).m_func)
			{}

			// templated copy ctors
/*			template< typename RngOther, typename FuncOther, bool bHasIteratorOther >
			transform_range( transform_range<FuncOther,RngOther,bHasIteratorOther> & rng, ctor_const_overload=ctor_const_overload() )
				: base_(rng.base_range(), aggregate_tag())
				, m_func(rng.m_func)
			{}

			template< typename RngOther, typename FuncOther, bool bHasIteratorOther >
			transform_range( transform_range<FuncOther,RngOther,bHasIteratorOther> && rng, ctor_const_overload=ctor_const_overload() )
				: base_(tc_move(rng).base_range_move(), aggregate_tag())
				, m_func(tc_move(rng).m_func)
			{}

			template< typename RngOther, typename FuncOther, bool bHasIteratorOther >
			transform_range( transform_range<FuncOther,RngOther,bHasIteratorOther> const& rng, ctor_const_overload )
				: base_(rng.base_range(), aggregate_tag())
				, m_func(rng.m_func)
			{}

			template< typename RngOther, typename FuncOther, bool bHasIteratorOther >
			transform_range( transform_range<FuncOther,RngOther,bHasIteratorOther> const& rng, typename std::enable_if<
				base_::template is_const_compatible_range<transform_range<FuncOther,RngOther,bHasIteratorOther> const&>::value
			, unused_arg>::type=unused_arg() )
				: base_(rng.base_range(), aggregate_tag())
				, m_func(rng.m_func)
			{}

			// some user-defined copy ctor to disable implicit one, with same semantics as templated copy ctor
			transform_range( typename base_::template const_compatible_range<transform_range>::type rng )
				: base_(rng.base_range(), aggregate_tag())
				, m_func(rng.m_func)
			{}*/

			// other ctors
			template< typename RngOther, typename FuncOther >
			transform_range( RngOther && rng, FuncOther && func, typename std::enable_if< !std::is_same<typename std::remove_reference<FuncOther>::type, ctor_const_overload>::value, unused_arg>::type=unused_arg() )
				: base_(std::forward<RngOther>(rng), aggregate_tag())
				, m_func(std::forward<FuncOther>(func))
			{}
		};

		template< typename Func, typename Rng >
		class transform_range<Func,Rng,true> : public transform_range<Func,Rng,false> {
			typedef transform_range<Func,Rng,false> base_;
			typedef typename base_::range_adaptor range_adaptor;

			friend class range_adaptor_impl::range_adaptor_access;
		public:
			typedef void ctor_const_overload_support;
			using typename base_::index;

			// default ctor
			transform_range() {}

			transform_range( transform_range && rng ) 
				: base_(tc_move(rng))
			{}

/*			// templated copy ctors
			template< typename RngOther, typename FuncOther >
			transform_range( transform_range<FuncOther,RngOther,true> & rng, ctor_const_overload=ctor_const_overload() )
				: base_(rng)
			{}

			template< typename RngOther, typename FuncOther >
			transform_range( transform_range<FuncOther,RngOther,true> && rng, ctor_const_overload=ctor_const_overload() )
				: base_(tc_move(rng))
			{}

			template< typename RngOther, typename FuncOther >
			transform_range( transform_range<FuncOther,RngOther,true> const& rng, ctor_const_overload )
				: base_(rng,ctor_const_overload())
			{}

			template< typename RngOther, typename FuncOther >
			transform_range( transform_range<FuncOther,RngOther,true> const& rng, typename std::enable_if<
				base_::template is_const_compatible_range<transform_range<FuncOther,RngOther,true> const&>::value
			, unused_arg>::type=unused_arg() )
				: base_(rng)
			{}

			// some user-defined copy ctor to disable implicit one, with same semantics as templated copy ctor
			transform_range( typename base_::template const_compatible_range<transform_range>::type rng )
				: base_(rng)
			{}*/

			// other ctors

			// ctor from range and functor
			template< typename RngOther, typename FuncOther >
			transform_range( RngOther && rng, FuncOther && func, typename std::enable_if< !std::is_same<typename std::remove_reference<FuncOther>::type, ctor_const_overload>::value, unused_arg>::type=unused_arg() )
				: base_(std::forward<RngOther>(rng),std::forward<FuncOther>(func))
			{}

			// ctors forwarding to sub_range
			template< typename RngOther, typename FuncOther >
			transform_range( transform_range< FuncOther, RngOther, true > && rng
				, typename boost::range_iterator< transform_range< FuncOther, RngOther, true > >::type itBegin
				, typename boost::range_iterator< transform_range< FuncOther, RngOther, true > >::type itEnd
			)
				: base_(tc::slice(tc_move(rng).base_range_move(),itBegin.base(),itEnd.base()), transform_range_access::get_func(tc_move(rng)))
			{}

			template< typename RngOther, typename FuncOther >
			transform_range( transform_range< FuncOther, RngOther, true > && rng
				, typename boost::range_size< transform_range< FuncOther, RngOther, true > >::type iBegin
				, typename boost::range_size< transform_range< FuncOther, RngOther, true > >::type iEnd
			)
				: base_(tc::slice(tc_move(rng).base_range_move(),iBegin,iEnd), transform_range_access::get_func(tc_move(rng)))
			{}

			using base_::make_iterator;

			typename base_::iterator make_iterator(typename boost::range_iterator< typename base_::BaseRange >::type it) {
				return base_::make_iterator(base_::base_range().iterator2index(it));
			}

			typename base_::const_iterator make_iterator(typename boost::range_const_iterator< typename base_::BaseRange >::type it) const {
				return base_::make_iterator(base_::base_range().iterator2index(it));
			}

		public:
			auto dereference_index(index const& idx) -> decltype( make_const(THIS_IN_DECLTYPE m_func)(std::declval<range_adaptor &>().dereference_index(idx)) ) {
				// always call operator() const, which is assumed to be thread-safe
				return make_const(this->m_func)(base_::dereference_index(idx));
			}

			auto dereference_index(index const& idx) const -> decltype( make_const(THIS_IN_DECLTYPE m_func)(std::declval<range_adaptor const&>().dereference_index(idx)) ) {
				// always call operator() const, which is assumed to be thread-safe
				return make_const(this->m_func)(base_::dereference_index(idx));
			}
		};
	}


}

