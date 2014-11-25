#pragma once

#include "range_defines.h"
#include "range_fwd.h"

#include "range_adaptor.h"
#include "sub_range.h"
#include "meta.h"

#include "Library/ErrorReporting/tc_move.h"
#include "transform.h"

namespace RANGE_PROPOSAL_NAMESPACE {
	namespace transform_adaptor_impl {
		class transform_adaptor_access {
		public:
			template< typename Func, typename Rng, bool bHasIterator >
			static Func && get_func( transform_adaptor<Func,Rng,bHasIterator> && rng ) {
				return tc_move(tc_move(rng).m_func);
			}
		};

		template<typename T>
		struct transform_adaptor_dereference_type {
			using type=T;
		};

		template<typename T>
		struct transform_adaptor_dereference_type<T&&> {
			using type=typename std::decay<T&&>::type;
		};

		template< typename Func, typename Rng >
		class transform_adaptor<Func,Rng,false> : public range_adaptor<transform_adaptor<Func,Rng>, Rng > {
			typedef range_adaptor<transform_adaptor<Func,Rng>, Rng > base_;

		protected:
			typedef base_ range_adaptor;

			static_assert( !std::is_reference<Func>::value, "" );
			static_assert( !std::is_const<Func>::value, "" );
			Func m_func;

		private:
			friend struct range_adaptor_impl::range_adaptor_access;
			friend class transform_adaptor_impl::transform_adaptor_access;
			template< typename Apply, typename... Args>
			auto apply( Apply && apply, Args&&... args) const return_decltype (
				std::forward<Apply>(apply)( m_func(std::forward<Args>(args)...) )
			)

		public:
			// default ctor
			transform_adaptor() {}

			// Range adaptors other than sub_ranges should not be copyable.
			// Otherwise, copying a range adaptor may have value or reference semantics,
			// depending whether the base range is by-value or by-reference.
			// Instead, the caller should explicit decide, and either
			// - store a reference or sub_range of the range,
			// - or copy the values into another container.

			transform_adaptor( transform_adaptor && rng ) 
				: base_(tc_move(rng).base_range_move(), aggregate_tag())
				, m_func(tc_move(rng).m_func)
			{}

			transform_adaptor& operator=( transform_adaptor && rng ) {
				base_::operator=(tc_move(rng).base_range_move(), aggregate_tag());
				m_func=tc_move(rng).m_func;
				return *this;
			}

		protected:
			transform_adaptor( transform_adaptor const& rng ) 
				: base_(rng.base_range(), aggregate_tag())
				, m_func(rng.m_func)
			{}

			transform_adaptor& operator=( transform_adaptor const& rng ) {
				base_::operator=(rng.base_range(), aggregate_tag());
				m_func=rng.m_func;
				return *this;
			}

		public:
			// other ctors
			template< typename RngOther, typename FuncOther >
			transform_adaptor( RngOther && rng, FuncOther && func )
				: base_(std::forward<RngOther>(rng), aggregate_tag())
				, m_func(std::forward<FuncOther>(func))
			{}
		};

		template< typename Func, typename Rng >
		class transform_adaptor<Func,Rng,true> : public transform_adaptor<Func,Rng,false> {
			static_assert( 
				std::is_same< Rng, typename range_by_value<Rng>::type >::value,
				"adaptors must hold ranges by value"
			);

			typedef transform_adaptor<Func,Rng,false> base_;
			typedef typename base_::range_adaptor range_adaptor;

			friend struct range_adaptor_impl::range_adaptor_access;
		public:
			using typename base_::index;

			// default ctor
			transform_adaptor() {}

			transform_adaptor( transform_adaptor && rng ) 
				: base_(tc::base_cast<base_>(tc_move(rng)))
			{}

			transform_adaptor& operator=( transform_adaptor && rng ) {
				base_::operator=(tc::base_cast<base_>(tc_move(rng)));
				return *this;
			}

		protected:
			transform_adaptor( transform_adaptor const& rng ) 
				: base_(tc::base_cast<base_>(rng))
			{}

			transform_adaptor& operator=( transform_adaptor const& rng ) {
				base_::operator=(tc::base_cast<base_>(rng));
				return *this;
			}

		public:
			// other ctors

			// ctor from range and functor
			template< typename RngOther, typename FuncOther >
			transform_adaptor( RngOther && rng, FuncOther && func )
				: base_(std::forward<RngOther>(rng),std::forward<FuncOther>(func))
			{}

			// ctors forwarding to sub_range
			template< typename RngOther, typename FuncOther >
			transform_adaptor( transform_adaptor< FuncOther, RngOther, true > && rng
				, typename boost::range_iterator< transform_adaptor< FuncOther, RngOther, true > >::type itBegin
				, typename boost::range_iterator< transform_adaptor< FuncOther, RngOther, true > >::type itEnd
			)
				: base_(tc::slice(tc_move(rng).base_range_move(),itBegin.base(),itEnd.base()), transform_adaptor_access::get_func(tc_move(rng)))
			{}

			template< typename RngOther, typename FuncOther >
			transform_adaptor( transform_adaptor< FuncOther, RngOther, true > && rng
				, typename boost::range_size< transform_adaptor< FuncOther, RngOther, true > >::type iBegin
				, typename boost::range_size< transform_adaptor< FuncOther, RngOther, true > >::type iEnd
			)
				: base_(tc::slice(tc_move(rng).base_range_move(),iBegin,iEnd), transform_adaptor_access::get_func(tc_move(rng)))
			{}

			using base_::make_iterator;

			typename base_::iterator make_iterator(typename boost::range_iterator< typename base_::BaseRange >::type it) {
				return base_::make_iterator(base_::base_range().iterator2index(it));
			}

			typename base_::const_iterator make_iterator(typename boost::range_const_iterator< typename base_::BaseRange >::type it) const {
				return base_::make_iterator(base_::base_range().iterator2index(it));
			}

		public:
			auto dereference_index(index const& idx) -> typename transform_adaptor_dereference_type<decltype( make_const(THIS_IN_DECLTYPE m_func)(std::declval<range_adaptor &>().dereference_index(idx)) )>::type {
				// always call operator() const, which is assumed to be thread-safe
				return make_const(this->m_func)(base_::dereference_index(idx));
			}

			auto dereference_index(index const& idx) const -> typename transform_adaptor_dereference_type<decltype( make_const(THIS_IN_DECLTYPE m_func)(std::declval<range_adaptor const&>().dereference_index(idx)) )>::type {
				// always call operator() const, which is assumed to be thread-safe
				return make_const(this->m_func)(base_::dereference_index(idx));
			}
		};
	}



	namespace replace_if_impl {
		template< typename Func, typename T >
		struct replace_if {
		private:
			typename std::decay<Func>::type m_func;
			typename std::decay<T>::type m_t;
			
		public:
			replace_if( Func && func, T && t )
				: m_func(std::forward<Func>(func))
				, m_t(std::forward<T>(t))
			{}
			template< typename S >
			auto operator()(S && s) const
				return_decltype( m_func(s) ? m_t : std::forward<S>(s) )
		};
	}

	template<typename Rng, typename Func, typename T>
	auto replace_if( Rng && rng, Func func, T && t )
		return_decltype( tc::transform( std::forward<Rng>(rng), replace_if_impl::replace_if<Func,T>(std::forward<Func>(func),std::forward<T>(t) ) ) )

	template<typename Rng, typename S, typename T>
	auto replace( Rng && rng, S && s, T && t )
		return_decltype( tc::replace_if( std::forward<Rng>(rng), boost::bind<bool>( fn_equal_to(), _1, std::forward<S>(s) ), std::forward<T>(t) ) )
}

