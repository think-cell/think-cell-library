#pragma once

#include "range_defines.h"
#include "range_fwd.h"

#include "Library/ErrorReporting/tc_move.h"
#include "range_adaptor.h"
#include "container_traits.h"
#include "meta.h"
#include "size.h"

#include <boost/bind.hpp>
#include <boost/range/detail/range_return.hpp>
#include <type_traits>

namespace RANGE_PROPOSAL_NAMESPACE {
	//-------------------------------------------------------------------------------------------------------------------------
	// meta function to determine the correct type
	// may_aggregate<Rng>::type does not work because sub_range::iterator must be an iterator of the underlying range.
	// Being an iterator of a copy of the underlying range is not sufficient.
	template< typename Rng > 
	struct make_sub_range_result {
		typedef sub_range<Rng> type;
	};

	// collapse sub_range< sub_range< ... > > to single sub_range
	template< typename Rng > 
	struct make_sub_range_result< sub_range<Rng> > {
		typedef typename make_sub_range_result<Rng>::type type;
	};
	template< typename Rng > 
	struct make_sub_range_result< sub_range<Rng> & > {
		typedef typename make_sub_range_result<
			typename std::add_lvalue_reference<Rng>::type
		>::type type;
	};
	template< typename Rng > 
	struct make_sub_range_result< sub_range<Rng> const& > {
		typedef typename make_sub_range_result<
			typename std::add_lvalue_reference<
				typename add_const_also_to_ref<Rng>::type
			>::type
		>::type type;
	};

	// put transform_adaptor outside of sub_range (to allow tc::equal_range( tc::transform( rng, func ) ).base_range())
	template< typename Func, typename Rng > 
	struct make_sub_range_result< transform_adaptor<Func,Rng,true> > {
		typedef transform_adaptor<Func, typename make_sub_range_result<
			Rng
		>::type, true > type;
	};

	//-------------------------------------------------------------------------------------------------------------------------

	template<typename Rng>
	auto ptr_begin(Rng && rng) enable_if_return_decltype(
		std::is_pointer< typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type >::value,
		boost::begin(rng)
	)
	template<typename Rng>
	auto ptr_end(Rng && rng) enable_if_return_decltype(
		std::is_pointer< typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type >::value,
		boost::end(rng)
	)

	template<typename Rng>
	auto ptr_begin(Rng && rng) enable_if_return_decltype(
		!std::is_pointer< typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type >::value,
		rng.data()
	)
	template<typename Rng>
	auto ptr_end(Rng && rng) enable_if_return_decltype(
		!std::is_pointer< typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type >::value,
		rng.data()+rng.size()
	)
	
	//-------------------------------------------------------------------------------------------------------------------------
	// fwd decls 
	template< typename Cont >
	Cont& head_inplace( Cont & cont, typename boost::range_iterator< typename std::remove_reference<Cont>::type >::type it );

	template< typename Cont >
	Cont& head_inplace( Cont& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n );

	template< typename Cont >
	Cont& tail_inplace( Cont & cont, typename boost::range_iterator< typename std::remove_reference<Cont>::type >::type it );

	template< typename Cont >
	Cont& tail_inplace( Cont& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n );
	
	template< typename It >
	typename std::make_unsigned< typename boost::iterator_difference< typename tc::remove_cvref<It>::type >::type >::type advance_forward_bounded(
		It&& it,
		typename std::make_unsigned< typename boost::iterator_difference< typename tc::remove_cvref<It>::type >::type >::type n,
		typename tc::remove_cvref<It>::type const& itBound
	);

	//-------------------------------------------------------------------------------------------------------------------------

	namespace sub_range_impl {
		template< typename Rng >
		struct whole_range_sub_range_helper_base {
			typedef range_adaptor< sub_range<Rng>, Rng, std::false_type > base_;

			template<typename Rhs>
			static auto base_range(Rhs && rhs)
				return_decltype( std::forward<Rhs>(rhs) )
			template<typename Rhs>
			static auto begin_index(base_ & base, Rhs &&)
				return_decltype( base.begin_index() )
			template<typename Rhs>
			static auto end_index(base_ & base, Rhs &&)
				return_decltype( base.end_index() )
		};

		template< typename It >
		struct whole_range_sub_range_helper_base<iterator_base<It>> {
			typedef range_adaptor< sub_range<iterator_base<It>>, iterator_base<It>, std::false_type > base_;

			template<typename Rhs>
			static auto base_range(Rhs &&)
				return_decltype( iterator_base<It>() )
			template<typename Rhs>
			static auto begin_index(base_ &, Rhs && rng)
				return_decltype( iterator_base<It>::iterator2index(boost::begin(rng)) )
			template<typename Rhs>
			static auto end_index(base_ &, Rhs && rng)
				return_decltype( iterator_base<It>::iterator2index(boost::end(rng)) )
		};

		template< typename T >
		struct whole_range_sub_range_helper_base<iterator_base<T*>> {
			typedef range_adaptor< sub_range<iterator_base<T*>>, iterator_base<T*>, std::false_type > base_;

			template<typename Rhs>
			static auto base_range(Rhs &&)
				return_decltype( iterator_base<T*>() )
			template<typename Rhs>
			static auto begin_index(base_ &, Rhs && rng)
				return_decltype( iterator_base<T*>::iterator2index(ptr_begin(rng)) )
			template<typename Rhs>
			static auto end_index(base_ &, Rhs && rng)
				return_decltype( iterator_base<T*>::iterator2index(ptr_end(rng)) )
		};

		template< typename Rng >
		struct whole_range_sub_range_helper : whole_range_sub_range_helper_base<Rng> {
			using typename whole_range_sub_range_helper_base<Rng>::base_;
			using whole_range_sub_range_helper_base<Rng>::base_range;
			using whole_range_sub_range_helper_base<Rng>::begin_index;
			using whole_range_sub_range_helper_base<Rng>::end_index;

			template<typename Rhs>
			static auto base_range(sub_range<sub_range<Rhs>> && rhs)
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( tc_move(rhs).base_range_move().base_range_move() ) )
			template<typename Rhs>
			static auto base_range(sub_range<Rhs> && rhs)
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( tc_move(rhs).base_range_move() ) )
			template<typename Rhs>
			static auto begin_index(base_ &, sub_range<Rhs> && rhs)
				code_return_decltype( static_assert( std::is_reference<Rng>::value || sub_range<Rhs>::index_valid_after_copy BOOST_PP_COMMA() "operation invalidates indices" );, rhs.begin_index() )
			template<typename Rhs>
			static auto end_index(base_ &, sub_range<Rhs> && rhs)
				code_return_decltype( static_assert( std::is_reference<Rng>::value || sub_range<Rhs>::index_valid_after_copy BOOST_PP_COMMA() "operation invalidates indices" );, rhs.end_index() )

			template<typename Rhs>
			static auto base_range(sub_range<Rhs> const& rhs)
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( rhs.base_range() ) )
			template<typename Rhs>
			static auto begin_index(base_ &, sub_range<Rhs> const& rhs)
				code_return_decltype( static_assert( std::is_reference<Rng>::value || sub_range<Rhs>::index_valid_after_copy BOOST_PP_COMMA() "operation invalidates indices" );, rhs.begin_index() )
			template<typename Rhs>
			static auto end_index(base_ &, sub_range<Rhs> const& rhs)
				code_return_decltype( static_assert( std::is_reference<Rng>::value || sub_range<Rhs>::index_valid_after_copy BOOST_PP_COMMA() "operation invalidates indices" );, rhs.end_index() )

			template<typename Rhs>
			static auto base_range(sub_range<Rhs> & rhs)
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( rhs.base_range() ) )
			template<typename Rhs>
			static auto begin_index(base_ &, sub_range<Rhs> & rhs)
				code_return_decltype( static_assert( std::is_reference<Rng>::value || sub_range<Rhs>::index_valid_after_copy BOOST_PP_COMMA() "operation invalidates indices" );, rhs.begin_index() )
			template<typename Rhs>
			static auto end_index(base_ &, sub_range<Rhs> & rhs)
				code_return_decltype( static_assert( std::is_reference<Rng>::value || sub_range<Rhs>::index_valid_after_copy BOOST_PP_COMMA() "operation invalidates indices" );, rhs.end_index() )
		};

		template< typename Rng >
		class sub_range : public range_adaptor< sub_range<Rng>, Rng, std::false_type > {
			typedef range_adaptor< sub_range<Rng>, Rng, std::false_type > base_;
			using typename base_::BaseRange;

		public:
			using typename base_::index;

		private:
			index m_idxBegin;
			index m_idxEnd;

		public:
			// default ctor (for deferred initialization)
			sub_range()
			{}

			// templated copy ctors
			template<typename RngOther> 
			sub_range( RngOther && rng, ctor_const_overload )
				: base_( whole_range_sub_range_helper<Rng>::base_range(
					ctor_base_cast<sub_range,sub_range>( std::forward<RngOther>(rng) ) 
				), aggregate_tag())
				, m_idxBegin(whole_range_sub_range_helper<Rng>::begin_index(
					base_cast<base_>(*this),
					ctor_base_cast<sub_range,sub_range>( std::forward<RngOther>(rng) ) 
				))
				, m_idxEnd(whole_range_sub_range_helper<Rng>::end_index(
					base_cast<base_>(*this),
					ctor_base_cast<sub_range,sub_range>( std::forward<RngOther>(rng) ) 
				))
			{}

			template<typename RngOther> 
			sub_range( RngOther && rng, typename std::enable_if<
				base_::template is_const_compatible_range<RngOther>::value
			, unused_arg>::type=unused_arg() )
				: base_( whole_range_sub_range_helper<Rng>::base_range(
					ctor_base_cast<sub_range,sub_range>( std::forward<RngOther>(rng) ) 
				), aggregate_tag())
				, m_idxBegin(whole_range_sub_range_helper<Rng>::begin_index(
					base_cast<base_>(*this),
					ctor_base_cast<sub_range,sub_range>( std::forward<RngOther>(rng) ) 
				))
				, m_idxEnd(whole_range_sub_range_helper<Rng>::end_index(
					base_cast<base_>(*this),
					ctor_base_cast<sub_range,sub_range>( std::forward<RngOther>(rng) ) 
				))
			{}

			// some user-defined copy ctor to disable implicit one, with same semantics as templated copy ctor
			sub_range( typename base_::template const_compatible_range<sub_range>::type rng)
				: base_( whole_range_sub_range_helper<Rng>::base_range(
					ctor_base_cast<sub_range,sub_range>(rng)
				), aggregate_tag())
				, m_idxBegin(whole_range_sub_range_helper<Rng>::begin_index(
					base_cast<base_>(*this),
					ctor_base_cast<sub_range,sub_range>(rng)
				))
				, m_idxEnd(whole_range_sub_range_helper<Rng>::end_index(
					base_cast<base_>(*this),
					ctor_base_cast<sub_range,sub_range>(rng)
				))
			{}

			// other ctors
			template<typename Rhs>
			sub_range( Rhs && rng,
				typename boost::range_size< typename std::remove_reference<Rng>::type >::type iBegin,
				typename boost::range_size< typename std::remove_reference<Rng>::type >::type iEnd
			)	: base_(whole_range_sub_range_helper<Rng>::base_range(
					std::forward<Rhs>(rng)
				), aggregate_tag())
				, m_idxBegin(whole_range_sub_range_helper<Rng>::begin_index(
					base_cast<base_>(*this),
					std::forward<Rhs>(rng)
				))
				, m_idxEnd(m_idxBegin)
			{
				base_::advance_index(m_idxBegin,tc_move(iBegin));
				base_::advance_index(m_idxEnd,tc_move(iEnd));
			}

			template<typename Rhs>
			sub_range( Rhs && rng,
				index idxBegin,
				index idxEnd )
			: base_(whole_range_sub_range_helper<Rng>::base_range(
				std::forward<Rhs>(rng)
			), aggregate_tag())
			, m_idxBegin(tc_move(idxBegin))
			, m_idxEnd(tc_move(idxEnd))
			{}

			template<typename Rhs>
			sub_range( Rhs && rng,
				typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itBegin,
				typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itEnd )
			: base_(whole_range_sub_range_helper<Rng>::base_range(
				std::forward<Rhs>(rng)
			), aggregate_tag())
			, m_idxBegin(std::remove_reference<typename index_range<Rhs>::type>::type::iterator2index(tc_move(itBegin)))
			, m_idxEnd(std::remove_reference<typename index_range<Rhs>::type>::type::iterator2index(tc_move(itEnd)))
			{}

			template< typename Func > break_or_continue operator()(Func func) {
				break_or_continue bc=continue_;
				for( index i=begin_index();
					!this->at_end_index(i) && continue_==(bc=continue_if_void( func, this->dereference_index(i) ));
					this->increment_index(i) );
				return bc;
			}

			template< typename Func > break_or_continue operator()(Func func) const {
				break_or_continue bc=continue_;
				for( index i=begin_index();
					!this->at_end_index(i) && continue_==(bc=continue_if_void( func, this->dereference_index(i) ));
					this->increment_index(i) );
				return bc;
			}

			index begin_index() const {
				return m_idxBegin;
			}

			index end_index() const {
				return m_idxEnd;
			}

			bool at_end_index(index const& idx) const {
				return base_::equal_index( idx, m_idxEnd );
			}

			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			// sub_range::iterator is the same type as the base range iterator:

			typedef typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type iterator;
			typedef typename boost::range_iterator< typename std::remove_reference<Rng>::type const >::type const_iterator;

			template<typename It>
			static auto iterator2index( It && it )
				return_decltype( BaseRange::iterator2index(std::forward<It>(it)) )

			const_iterator make_iterator( index idx ) const {
				return base_::base_range().make_iterator(tc_move(idx));
			}

			const_iterator begin() const {
				return make_iterator(begin_index());
			}

			const_iterator end() const {
				return make_iterator(end_index());
			}

			iterator make_iterator( index idx ) {
				return base_::base_range().make_iterator(tc_move(idx));
			}

			iterator begin() {
				return make_iterator(begin_index());
			}

			iterator end() {
				return make_iterator(end_index());
			}

			template< typename It >
			friend void head_inplace_impl( sub_range& rng, It && it ) {
				rng.m_idxEnd=iterator2index( std::forward<It>(it) );
			}

			template< typename It >
			friend void tail_inplace_impl( sub_range& rng, It && it ) {
				rng.m_idxBegin=iterator2index( std::forward<It>(it) );
			}

			template< typename Delimiter >
			friend sub_range&& head( sub_range&& rng, Delimiter && del ) {
				tc::head_inplace(rng,std::forward<Delimiter>(del));
				return tc_move(rng);
			}

			template< typename Delimiter >
			friend sub_range&& tail( sub_range&& rng, Delimiter && del ) {
				tc::tail_inplace(rng,std::forward<Delimiter>(del));
				return tc_move(rng);
			}
		};
	}
	using sub_range_impl::sub_range;

	//-------------------------------------------------------------------------------------------------------------------------
	// slice

	// slice from range + iterator pair
	// slice from range + difference
	template< typename Rng, typename Begin, typename End >
	typename make_sub_range_result< Rng >::type slice( Rng && rng, Begin && begin, End && end ) {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), std::forward<Begin>(begin), std::forward<End>(end) );
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// head

	template< typename Cont, typename It >
	void head_inplace_impl( Cont & cont, It && it ) {
		cont.erase( it, cont.end() );
	}

	template< typename Cont >
	Cont& head_inplace( Cont & cont, typename boost::range_iterator< typename std::remove_reference<Cont>::type >::type it ) {
		head_inplace_impl(cont,tc_move(it)); // allow ADL
		return cont;
	}

	template< typename Cont >
	Cont& head_inplace( Cont& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n ) {
		auto it=cont.begin();
		_ASSERT( n <= boost::implicit_cast< typename boost::range_size< typename std::remove_reference<Cont>::type >::type >(boost::distance(cont)) );
		std::advance( it, n );
		tc::head_inplace( cont, it );
		return cont;
	}

	template< typename Rng >
	typename make_sub_range_result< Rng >::type head_impl( Rng && rng,
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itEnd
	) {
		return make_sub_range_result< Rng >::type( std::forward<Rng>(rng), boost::begin(rng), tc_move(itEnd) );
	}

	template< typename Rng >
	typename make_sub_range_result< Rng >::type head_impl( Rng && rng,
		typename boost::range_size< typename std::remove_reference<Rng>::type >::type iEnd
	) {
		return make_sub_range_result< Rng >::type( std::forward<Rng>(rng), 0, tc_move(iEnd) );
	}
	
	template< typename C, typename T, typename A, typename Delimiter >
	std::basic_string<C,T,A> && head( std::basic_string<C,T,A> && rng, Delimiter && del ) {
		tc::head_inplace(rng,std::forward<Delimiter>(del));
		return tc_move(rng);
	}
	
	template< typename Rng, typename Delimiter >
	auto head( Rng && rng, Delimiter && del ) return_decltype(
		head_impl( std::forward<Rng>(rng), std::forward<Delimiter>(del) )
	)
	
	//-------------------------------------------------------------------------------------------------------------------------
	// tail

	// tail_inplace
/*	template< typename Cont, typename It >
	void tail_inplace_impl( Cont & cont, It && it ) {
		cont.erase( cont.begin(), it );
	}*/

	template< typename T >
	struct is_char_ptr : std::integral_constant<bool,
		std::is_pointer<
			typename std::decay<T>::type
		>::value &&
		tc::is_char<
			typename std::remove_pointer<typename std::decay<T>::type>::type
		>::value
	>::type {};

	template< typename CharPtr, typename It >
	typename std::enable_if< tc::is_char_ptr<CharPtr>::value, void >::type tail_inplace_impl( CharPtr& pch, It && it ) {
		pch=std::forward<It>(it);
	}

	template< typename Cont >
	Cont& tail_inplace( Cont & cont, typename boost::range_iterator< typename std::remove_reference<Cont>::type >::type it ) {
		tail_inplace_impl(cont,tc_move(it));
		return cont;
	}

	template< typename Cont >
	Cont& tail_inplace( Cont& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n ) {
		auto it=boost::begin(cont);
		_ASSERT( n <= boost::implicit_cast< typename boost::range_size< typename std::remove_reference<Cont>::type >::type >(boost::distance(cont)) );
		std::advance( it, n );
		tc::tail_inplace( cont, it );
		return cont;
	}

	template< typename Rng >
	typename std::enable_if< !tc::is_char_ptr< Rng >::value,
	typename make_sub_range_result< Rng >::type >::type tail_impl( Rng && rng,
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itBegin
	) {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), itBegin, boost::end(rng) );
	}

	template< typename Rng >
	typename std::enable_if< !tc::is_char_ptr< Rng >::value,
	typename make_sub_range_result< Rng >::type >::type tail_impl( Rng && rng,
		typename boost::range_size< typename std::remove_reference<Rng>::type >::type iBegin
	) {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), tc_move(iBegin), tc::size(rng) );
	}

	// C strings have efficient in-place tail
	template< typename CharPtr, typename Delimiter >
	typename std::enable_if< tc::is_char_ptr< CharPtr >::value,
	typename std::decay<CharPtr>::type >::type tail_impl( CharPtr && pch, Delimiter && del ) {
		typename std::decay<CharPtr>::type pchCopy=std::forward<CharPtr>(pch);
		tc::tail_inplace( pchCopy, std::forward<Delimiter>(del) );
		return pchCopy;
	}

	template< typename Rng, typename Delimiter >
	auto tail( Rng && rng, Delimiter && del ) return_decltype(
		tail_impl( std::forward<Rng>(rng), std::forward<Delimiter>(del) )
	)

	//-------------------------------------------------------------------------------------------------------------------------
	// truncate

	template< typename Cont >
	Cont& truncate_inplace( Cont& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n ) {
		auto it=cont.begin();
		advance_forward_bounded( it, n, cont.end() );
		tc::head_inplace( cont, it );
		return cont;
	}

	template< typename Rng >
	auto truncate( Rng && rng, typename boost::range_size< typename std::remove_reference<Rng>::type >::type n )
	->decltype(tc::head( std::forward<Rng>(rng), boost::begin(rng) )) {
		auto it=boost::begin(rng);
		tc::advance_forward_bounded( it, n, boost::end(rng) );
		return tc::head( std::forward<Rng>(rng), tc_move(it) );
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// make_head_range_bounded

	namespace sub_range_detail {
		template< typename It >
		typename std::make_unsigned< typename boost::iterator_difference< typename tc::remove_cvref<It>::type >::type >::type advance_forward_bounded_impl(
			It&& it,
			typename std::make_unsigned< typename boost::iterator_difference< typename tc::remove_cvref<It>::type >::type >::type n,
			typename tc::remove_cvref<It>::type const& itBound,
			boost::single_pass_traversal_tag
		) {
			_ASSERT( 0 <= n );
			typename std::make_unsigned< typename boost::iterator_difference< typename tc::remove_cvref<It>::type >::type >::type nCount = 0;
			while( nCount!=n && it!=itBound ) {
				++nCount;
				++it;
			}
			return nCount;
		}

		template< typename It >
		typename std::make_unsigned< typename boost::iterator_difference< typename tc::remove_cvref<It>::type >::type >::type advance_forward_bounded_impl(
			It&& it,
			typename std::make_unsigned< typename boost::iterator_difference< typename tc::remove_cvref<It>::type >::type >::type n,
			typename tc::remove_cvref<It>::type const& itBound,
			boost::random_access_traversal_tag
		) {
			_ASSERT( 0 <= n );
			if( assign_better( n, tc::make_size_proxy(itBound-it), fn_less_equal() ) ) {
				it=itBound;
			} else {
				it+=n;
			}
			return n;
		}
	}

	template< typename It >
	typename std::make_unsigned< typename boost::iterator_difference< typename tc::remove_cvref<It>::type >::type >::type advance_forward_bounded(
		It&& it,
		typename std::make_unsigned< typename boost::iterator_difference< typename tc::remove_cvref<It>::type >::type >::type n,
		typename tc::remove_cvref<It>::type const& itBound
	) {
		return sub_range_detail::advance_forward_bounded_impl( std::forward<It>(it), n, itBound, typename boost::iterator_traversal< typename tc::remove_cvref<It>::type >::type() );
	}

	template< typename Rng >
	typename tc::make_sub_range_result<Rng>::type make_head_range_bounded( 
		Rng && rng, 
		typename boost::range_size<Rng>::type nEnd )
	{
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itEnd=boost::begin(rng);
		advance_forward_bounded( itEnd, nEnd, boost::end(rng) );
		return tc::head( std::forward<Rng>(rng), itEnd );
	};

	//-------------------------------------------------------------------------------------------------------------------------
	// make iterator range

	// sub_range from iterator pair
	template< typename It >
	auto make_iterator_range_impl( It itBegin, It itEnd )
		return_ctor( tc::sub_range<tc::iterator_base<It>>, ( tc::iterator_base<It>(), tc_move(itBegin), tc_move(itEnd) ) )

	// There is an other make_iterator_range_impl overload for range adaptor based iterarors in range_adaptor.h

	// make sure ADL lookup of common_iterator::make_iterator_range works
	template< typename ItBegin, typename ItEnd >
	auto make_iterator_range(ItBegin && itBegin, ItEnd && itEnd)
		return_decltype( make_iterator_range_impl( std::forward<ItBegin>(itBegin), std::forward<ItEnd>(itEnd) ) )

	template< typename T >
	auto make_empty_range()
		return_decltype( make_iterator_range( static_cast<T*>(nullptr), static_cast<T*>(nullptr) ) )

	template< typename T >
	auto make_singleton_range( T && t )
		return_decltype( make_iterator_range( std::addressof(t), std::addressof(t)+1 ) )

	//-------------------------------------------------------------------------------------------------------------------------
	// boost::range_return with tc::sub_range

	enum range_return_value
	{
		// (*) indicates the most common values
		return_iterator_or_end, // the resulting iterator, singleton is end()
		return_iterator, // the resulting iterator, singleton not allowed
		return_prev_iterator, // prior(found) iterator, singleton not allowed
		return_next_iterator, // prior(found) iterator, singleton not allowed
		return_head, // [begin, found) range, singleton not allowed
		return_head_next, // [begin, next(found)) range, singleton not allowed
		return_tail, // [found, end) range, singleton not allowed
		return_index_or_npos, // return index of found, singleton is -1
		return_index, // return index of found, singleton not allowed
		return_index_or_size // return index of found, singleton is size
	};

	template< typename Rng, range_return_value >
	struct range_return;

	template< typename Rng >
	struct range_return< Rng, return_iterator_or_end >
	{
		typedef typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type type;

		static type pack(type found, Rng&&) {
			return found;
		}
		static type pack_singleton(Rng&& rng) {
			return boost::end(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_iterator >
	{
		typedef typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type type;

		static type pack(type found, Rng&& rng) {
			_ASSERT( found!=boost::end(rng) );
			return found;
		}
		static type pack_singleton(Rng&& rng) {
			_ASSERTFALSE;
			return boost::begin(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_prev_iterator >
	{
		typedef typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type type;

		static type pack(type found, Rng&& rng) {
			_ASSERT(found != boost::begin(rng));
			return boost::prior(found);
		}
		static type pack_singleton(Rng && rng) {
			_ASSERTFALSE;
			return boost::begin(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_next_iterator >
	{
		typedef typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type type;

		static type pack(type found, Rng&& rng) {
			_ASSERT(found != boost::end(rng));
			return boost::next(found);
		}
		static type pack_singleton(Rng && rng) {
			_ASSERTFALSE;
			return boost::end(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_head >
	{
		typedef typename make_sub_range_result<Rng>::type type;

		static type pack(typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type found, Rng&& rng) {
			return tc::head( std::forward<Rng>(rng), found);
		}
		static type pack_singleton(Rng && rng) {
			_ASSERTFALSE;
			return tc::head( std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_tail >
	{
		typedef decltype(tc::tail( std::declval<Rng&&>(), boost::begin(std::declval<Rng&>()) )) type;

		static type pack(typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type found, Rng&& rng) {
			return tc::tail( std::forward<Rng>(rng), found );
		}
		static type pack_singleton(Rng && rng) {
			_ASSERTFALSE;
			return tc::tail( std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_index_or_npos > {
		// static_cast<int>(npos) must be -1. Anything else is error-prone. So use range_difference instead of range_size for now.
		// Alternatively, we could return a special type that casts npos to -1.
		typedef tc::size_proxy< typename boost::range_difference< typename std::remove_reference<Rng>::type >::type > type;

		static type pack(typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it, Rng&& rng) {
			return it==boost::end(rng)
				? pack_singleton(std::forward<Rng>(rng))
				: type( it-boost::begin(rng) );
		}
		static type pack_singleton(Rng &&) {
			return type( static_cast<typename boost::range_difference< typename std::remove_reference<Rng>::type >::type>(-1) );
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_index > {
		typedef tc::size_proxy< typename boost::range_size< typename std::remove_reference<Rng>::type >::type > type;

		static type pack(typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it, Rng&& rng) {
			_ASSERT( it!=boost::end(rng) );
			return type( it-boost::begin(rng) );
		}
		static type pack_singleton(Rng &&) {
			_ASSERTFALSE;
			return type(0);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_index_or_size > {
		typedef tc::size_proxy< typename boost::range_size< typename std::remove_reference<Rng>::type >::type > type;

		static type pack(typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it, Rng&& rng) {
			return type( it-boost::begin(rng) );
		}
		static type pack_singleton(Rng && rng) {
			return tc::size(std::forward<Rng>(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_head_next > {
		typedef typename make_sub_range_result<Rng>::type type;

		static type pack(typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it, Rng&& rng) {
			_ASSERT( it!=boost::end(rng) );
			return tc::head( std::forward<Rng>(rng), boost::next(it) );
		}
		static type pack_singleton(Rng && rng) {
			_ASSERTFALSE;
			return std::forward<Rng>(rng);
		}
	};

	//-------------------------------------------------------------------------------------------------------------------------
	// as_pointers
	// get a consecutive block of memory from range and return an iterator_range of pointers

	template< typename Rng >
	auto as_pointers(Rng && rng)->tc::sub_range<
		tc::iterator_base<
			decltype( ptr_begin( std::declval<Rng&&>() ) )
		>
	> {
		return std::forward<Rng>(rng);
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// as_array

	template< typename T, std::size_t N >
	auto as_array(T (&at)[N] )
		enable_if_return_decltype( 
			is_char<T>::value,
			tc::make_iterator_range( std::addressof(at[0]), std::addressof(at[0])+N )
		)
}
