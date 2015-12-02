#pragma once

#include "range_defines.h"
#include "range_fwd.h"

#include "Library/ErrorReporting/tc_move.h"
#include "range_adaptor.h"
#include "container_traits.h"
#include "meta.h"
#include "size.h"
#include "../assign.h"

#include <boost/bind.hpp>
#include <boost/range/detail/range_return.hpp>
#include <type_traits>

namespace RANGE_PROPOSAL_NAMESPACE {
	//-------------------------------------------------------------------------------------------------------------------------
	// meta function to determine the correct type
	// may_aggregate<Rng>::type does not work because sub_range::iterator must be an iterator of the underlying range.
	// Being an iterator of a copy of the underlying range is not sufficient.
	template< typename Rng, typename Enable >
	struct make_sub_range_result {
		using type = sub_range< Rng >;
	};
	
	template< typename Rng >
	struct make_sub_range_result< Rng, typename std::enable_if<
		!std::is_same< Rng, typename range_by_value<Rng>::type >::value
	>::type > {
		using type = typename make_sub_range_result< typename range_by_value<Rng>::type >::type;
	};

	// collapse sub_range< sub_range< ... > > to single sub_range
	template< typename Rng >
	struct make_sub_range_result< sub_range<Rng> > {
		using type = typename make_sub_range_result<Rng>::type;
	};

	// put transform_adaptor outside of sub_range (to allow tc::equal_range( tc::transform( rng, func ) ).base_range())
	template< typename Func, typename Rng > 
	struct make_sub_range_result< transform_adaptor<Func,Rng,true> > {
		using type = transform_adaptor<Func, typename make_sub_range_result<
			Rng
		>::type, true >;
	};

	//-------------------------------------------------------------------------------------------------------------------------
	template<typename T>
	T* raw_ptr(T* t) { return t; } // overloaded e.g. for boost::interprocess::offset_ptr
	
	template<typename Rng, std::enable_if_t<std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value>* = nullptr>
	auto ptr_begin(Rng&& rng) return_decltype(
		boost::begin(rng)
	)
	template<typename Rng, std::enable_if_t<std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value>* = nullptr>
	auto ptr_end(Rng&& rng) return_decltype(
		boost::end(rng)
	)

	template< typename T >
	struct is_basic_string : std::false_type {};

	template< typename Char, typename Alloc >
	struct is_basic_string<std::basic_string<Char, std::char_traits<Char>, Alloc> > : std::true_type {};

	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value 
			&& !(is_basic_string<tc::remove_cvref_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value )
		>* = nullptr
	>
	auto ptr_begin(Rng&& rng) return_decltype(
		raw_ptr( rng.data() )
	)
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value 
			&& !(is_basic_string<tc::remove_cvref_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value )
		>* = nullptr
	>
	auto ptr_end(Rng&& rng) return_decltype(
		raw_ptr( rng.data() ) + rng.size()
	)
	
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value 
			&& is_basic_string<tc::remove_cvref_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value
		>* = nullptr
	>
	auto ptr_begin(Rng&& rng) return_decltype(
		tc::make_mutable_ptr(raw_ptr( rng.data() ))
	)
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value 
			&& is_basic_string<tc::remove_cvref_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value
		>* = nullptr
	>
	auto ptr_end(Rng&& rng) return_decltype(
		tc::make_mutable_ptr(raw_ptr( rng.data() )) + rng.size()
	)
	
	//-------------------------------------------------------------------------------------------------------------------------
	// fwd decls 
	template< typename Cont >
	Cont& take_inplace( Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it );

	template< typename Cont >
	Cont& drop_inplace( Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it );

	template< typename It >
	std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > advance_forward_bounded(
		It&& it,
		std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > n,
		tc::remove_cvref_t<It> const& itBound
	);

	//-------------------------------------------------------------------------------------------------------------------------

	template<typename Rng>
	struct range_traversal {
		using type = typename boost::iterator_traversal< 
			typename boost::range_iterator< 
				std::remove_reference_t<Rng> 
			>::type 
		>::type;
	};

	namespace sub_range_impl {
		template< typename Rng >
		struct whole_range_sub_range_helper_base {
			using base_ = range_adaptor< sub_range<Rng>, Rng >;

			template<typename Rhs>
			static auto base_range(Rhs&& rhs)
				return_decltype_rvalue_by_ref( std::forward<Rhs>(rhs) )
			template<typename Rhs>
			static auto begin_index(base_ & base, Rhs&&)
				return_decltype( base.STATIC_VIRTUAL_METHOD_NAME(begin_index)() )
			template<typename Rhs>
			static auto end_index(base_ & base, Rhs&&)
				return_decltype( base.STATIC_VIRTUAL_METHOD_NAME(end_index)() )
		};

		template< typename It >
		struct whole_range_sub_range_helper_base<iterator_base<It>> {
			using base_ = range_adaptor< sub_range<iterator_base<It>>, iterator_base<It> >;

			template<typename Rhs>
			static auto base_range(Rhs &&)
				return_decltype( iterator_base<It>() )
			template<typename Rhs>
			static auto begin_index(base_ &, Rhs&& rng)
				return_decltype( tc::iterator2index(boost::begin(rng)) )
			template<typename Rhs>
			static auto end_index(base_ &, Rhs&& rng)
				return_decltype( tc::iterator2index(boost::end(rng)) )
		};

		template< typename T >
		struct whole_range_sub_range_helper_base<iterator_base<T*>> {
			using base_ = range_adaptor< sub_range<iterator_base<T*>>, iterator_base<T*> >;

			template<typename Rhs>
			static auto base_range(Rhs &&)
				return_decltype( iterator_base<T*>() )
			template<typename Rhs>
			static auto begin_index(base_ &, Rhs&& rng)
				return_decltype( tc::iterator2index(ptr_begin(rng)) )
			template<typename Rhs>
			static auto end_index(base_ &, Rhs&& rng)
				return_decltype( tc::iterator2index(ptr_end(rng)) )
		};

		template< typename Rng >
		struct whole_range_sub_range_helper : whole_range_sub_range_helper_base<Rng> {
			using typename whole_range_sub_range_helper_base<Rng>::base_;
			using whole_range_sub_range_helper_base<Rng>::base_range;
			using whole_range_sub_range_helper_base<Rng>::begin_index;
			using whole_range_sub_range_helper_base<Rng>::end_index;

			template<typename Rhs>
			static auto base_range(sub_range<sub_range<Rhs>>&& rhs)
				return_decltype_rvalue_by_ref( whole_range_sub_range_helper<Rng>::base_range( tc_move(rhs).base_range_move().base_range_move() ) )
			template<typename Rhs>
			static auto base_range(sub_range<Rhs>&& rhs)
				return_decltype_rvalue_by_ref( whole_range_sub_range_helper<Rng>::base_range( tc_move(rhs).base_range_move() ) )
			template<typename Rhs>
			static auto begin_index(base_ &, sub_range<Rhs>&& rhs)
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			static auto end_index(base_ &, sub_range<Rhs>&& rhs)
				return_decltype( rhs.end_index() )

			template<typename Rhs>
			static auto base_range(sub_range<Rhs> const& rhs)
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( rhs.base_range() ) )
			template<typename Rhs>
			static auto begin_index(base_ &, sub_range<Rhs> const& rhs)
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			static auto end_index(base_ &, sub_range<Rhs> const& rhs)
				return_decltype( rhs.end_index() )

			template<typename Rhs>
			static auto base_range(sub_range<Rhs> & rhs)
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( rhs.base_range() ) )
			template<typename Rhs>
			static auto begin_index(base_ &, sub_range<Rhs> & rhs)
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			static auto end_index(base_ &, sub_range<Rhs> & rhs)
				return_decltype( rhs.end_index() )
		};

		template< typename Rng >
		struct sub_range : range_adaptor< sub_range<Rng>, Rng > {
			static_assert(
				!tc::is_range<Rng>::value || std::is_same< Rng, typename range_by_value<Rng>::type >::value,
				"sub_range must hold ranges by value."
			);
			static_assert(
				tc::is_range<Rng>::value || std::is_reference<Rng>::value,
				"sub_range must hold containers by reference."
			);

		private:
			using this_type = sub_range;
			using base_ = range_adaptor< sub_range<Rng>, Rng >;
			using typename base_::BaseRange;

		public:
			using typename base_::index;

		private:
			index m_idxBegin;
			index m_idxEnd;

			/* 
			TODO: The sub_range(RngOther&&) ctor below is not properly enable_if'ed.
			It should use the is_compatible_ctor predicate below, but that crashes
			MSVC2013. Try again with MSVC2015.
			This needs a fallback implementation of whole_range_sub_range_helper<Rng>::begin_index like
			 
			 struct incompatible_index {};
			 static incompatible_index begin_index(base_ &, ...);
			 
			to work. 
			
			template<typename RngOther>
			struct delayed_test_conversion_to_index {
				using type=std::is_convertible<
					decltype( whole_range_sub_range_helper<Rng>::begin_index(
						std::declval<base_&>(),
						ctor_base_cast<sub_range,sub_range>(std::declval<RngOther&&>())
					) ),
					index>;
			};
			
			template<typename RngOther>
			using is_compatible_range =
				typename boost::mpl::eval_if_c<
					is_range_with_iterators<RngOther>::value,
					delayed_test_conversion_to_index<RngOther>,
					std::false_type
				>::type;
			*/
		public:
			// default ctor (for deferred initialization)
			sub_range()
			{}

			template<typename RngOther> 
			sub_range( RngOther&& rng, typename std::enable_if<
				is_range_with_iterators<RngOther>::value // TODO: Replace with is_compatible_range above
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
			sub_range( sub_range const& rng)
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

			template<typename Rhs>
			sub_range( Rhs&& rng,
				index idxBegin,
				index idxEnd )
			: base_(whole_range_sub_range_helper<Rng>::base_range(
				std::forward<Rhs>(rng)
			), aggregate_tag())
			, m_idxBegin(tc_move(idxBegin))
			, m_idxEnd(tc_move(idxEnd))
			{}

			template<typename Rhs>
			sub_range( Rhs&& rng,
				typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin,
				typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd )
			: base_(whole_range_sub_range_helper<Rng>::base_range(
				std::forward<Rhs>(rng)
			), aggregate_tag())
			, m_idxBegin(tc::iterator2index(tc_move(itBegin)))
			, m_idxEnd(tc::iterator2index(tc_move(itEnd)))
			{}

			template< typename Func > break_or_continue operator()(Func func) {
				break_or_continue bc=continue_;
				for( index i=this->begin_index();
					!this->at_end_index(i) && continue_==(bc=continue_if_not_break( func, this->dereference_index(i) ));
					this->increment_index(i) );
				return bc;
			}

			template< typename Func > break_or_continue operator()(Func func) const {
				break_or_continue bc=continue_;
				for( index i=this->begin_index();
					!this->at_end_index(i) && continue_==(bc=continue_if_not_break( func, this->dereference_index(i) ));
					this->increment_index(i) );
				return bc;
			}

			STATIC_FINAL(begin_index)() const -> index {
				return m_idxBegin;
			}

			STATIC_FINAL(end_index)() const -> index {
				return m_idxEnd;
			}

			STATIC_FINAL(at_end_index)(index const& idx) const -> bool {
				return base_::equal_index( idx, m_idxEnd );
			}

			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			// sub_range::iterator is the same type as the base range iterator:

			using iterator = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;
			using const_iterator = typename boost::range_iterator< std::remove_reference_t<Rng> const >::type;

			const_iterator make_iterator( index idx ) const {
				return base_::base_range().make_iterator(tc_move(idx));
			}

			const_iterator begin() const {
				return make_iterator(this->begin_index());
			}

			const_iterator end() const {
				return make_iterator(this->end_index());
			}

			iterator make_iterator( index idx ) {
				return base_::base_range().make_iterator(tc_move(idx));
			}

			iterator begin() {
				return make_iterator(this->begin_index());
			}

			iterator end() {
				return make_iterator(this->end_index());
			}

			template< typename It >
			friend void take_inplace_impl( sub_range& rng, It&& it ) {
				rng.m_idxEnd=tc::iterator2index( std::forward<It>(it) );
			}

			template< typename It >
			friend void drop_inplace_impl( sub_range& rng, It&& it ) {
				rng.m_idxBegin=tc::iterator2index( std::forward<It>(it) );
			}

			template< typename It >
			friend sub_range&& take( sub_range&& rng, It&& it ) {
				tc::take_inplace(rng,std::forward<It>(it));
				return tc_move(rng);
			}

			template< typename It >
			friend sub_range&& drop( sub_range&& rng, It&& it ) {
				tc::drop_inplace(rng,std::forward<It>(it));
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
	typename make_sub_range_result< Rng >::type slice(Rng&& rng, Begin&& begin, End&& end) {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), std::forward<Begin>(begin), std::forward<End>(end) );
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// take

	template< typename Cont, typename It >
	void take_inplace_impl( Cont & cont, It&& it ) {
		cont.erase( it, boost::end(cont) );
	}

	template< typename Cont >
	Cont& take_inplace( Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it ) {
		take_inplace_impl(cont,tc_move(it)); // allow ADL
		return cont;
	}

	template< typename Rng >
	typename make_sub_range_result< Rng >::type take_impl(Rng&& rng,
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd
	) {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), boost::begin(rng), tc_move(itEnd) );
	}

	template< typename C, typename T, typename A, typename It >
	std::basic_string<C,T,A> && take( std::basic_string<C,T,A>&& rng, It&& it ) {
		tc::take_inplace(rng,std::forward<It>(it));
		return tc_move(rng);
	}
	
	template< typename Rng, typename It >
	auto take(Rng&& rng, It&& it) return_decltype(
		take_impl( std::forward<Rng>(rng), std::forward<It>(it) )
	)

	//-------------------------------------------------------------------------------------------------------------------------
	// drop
	template< typename T >
	struct is_char_ptr : std::integral_constant<bool,
		std::is_pointer<
			std::decay_t<T>
		>::value &&
		tc::is_char<
			std::remove_pointer_t<std::decay_t<T>>
		>::value
	>::type {};

	// drop_inplace
	template< typename Cont, typename It >
	typename std::enable_if< !tc::is_char_ptr<Cont>::value, void >::type drop_inplace_impl( Cont & cont, It&& it ) {
		cont.erase( boost::begin(cont), it );
	}

	template< typename CharPtr, typename It >
	typename std::enable_if< tc::is_char_ptr<CharPtr>::value, void >::type drop_inplace_impl( CharPtr& pch, It&& it ) {
		pch=std::forward<It>(it);
	}

	template< typename Cont >
	Cont& drop_inplace( Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it ) {
		drop_inplace_impl(cont,tc_move(it));
		return cont;
	}

	template< typename Rng >
	typename std::enable_if< !tc::is_char_ptr< Rng >::value,
	typename make_sub_range_result< Rng >::type >::type drop_impl(Rng&& rng,
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin
	) {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), itBegin, boost::end(rng) );
	}

	// C strings have efficient in-place drop
	template< typename CharPtr, typename It >
	typename std::enable_if< tc::is_char_ptr< CharPtr >::value,
	std::decay_t<CharPtr> >::type drop_impl( CharPtr&& pch, It&& it ) {
		std::decay_t<CharPtr> pchCopy=std::forward<CharPtr>(pch);
		tc::drop_inplace( pchCopy, std::forward<It>(it) );
		return pchCopy;
	}

	template< typename Rng, typename It >
	auto drop(Rng&& rng, It&& it) return_decltype(
		drop_impl( std::forward<Rng>(rng), std::forward<It>(it) )
	)

	////////////////////////////////
	// front/back on ranges

	template< typename It, typename Rng >
	It && verify_not_end(It&& it, Rng const& rng) {
		_ASSERT(boost::end(rng) != it);
		return std::forward<It>(it);
	}

	namespace begin_next_adl_barrier {
		template< typename Rng >
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type
		begin_next(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::forward_traversal_tag
		) {
			_ASSERT(0 <= n);
			_ASSERTNOTIFY(n <= 2);
			typename boost::range_iterator< std::remove_reference_t<Rng> >::type it=boost::begin(rng);
#ifdef _CHECKS
			typename boost::range_iterator< std::remove_reference_t<Rng> >::type const itBound=boost::end(rng);
#endif
			while (0<n) {
				_ASSERT(it!=itBound);
				--n;
				++it;
			}
			return it;
		}

		template< typename Rng >
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type
		begin_next(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::random_access_traversal_tag
		) {
			_ASSERT(0 <= n);
			_ASSERT(n<=tc::size(rng));
			return boost::begin(rng)+n;
		}
	}

	template< typename Rng >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type
	begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) {
		return begin_next_adl_barrier::begin_next(std::forward<Rng>(rng), n, typename boost::range_traversal< std::remove_reference_t<Rng> >::type());
	}

	namespace end_prev_adl_barrier {
		template< typename Rng >
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type
		end_prev(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::bidirectional_traversal_tag
		) {
			_ASSERT(0 <= n);
			_ASSERTNOTIFY(n <= 2);
			typename boost::range_iterator< std::remove_reference_t<Rng> >::type it=boost::end(rng);
#ifdef _CHECKS
			typename boost::range_iterator< std::remove_reference_t<Rng> >::type const itBound=boost::begin(rng);
#endif
			while (0<n) {
				_ASSERT(it!=itBound);
				--n;
				--it;
			}
			return it;
		}

		template< typename Rng >
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type
		end_prev(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::random_access_traversal_tag
		) {
			_ASSERT(0 <= n);
			_ASSERT(n<=tc::size(rng));
			return boost::end(rng)-n;
		}
	}

	template< typename Rng >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type
	end_prev(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) {
		return end_prev_adl_barrier::end_prev(std::forward<Rng>(rng), n, typename boost::range_traversal< std::remove_reference_t<Rng> >::type());
	}

	template< typename Rng >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type
	begin_not_end(
		Rng&& rng
	) {
		return tc::verify_not_end(boost::begin(rng), rng);
	}

	template< typename Rng >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type
	begin_next_not_end(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n = 1
	) {
		return tc::verify_not_end(tc::begin_next(rng, n), rng);
	}

	// Write as macros to keep temporary iterators alive.
	// By standard, the lifetime of a reference is limited to the lifetime of the iterator.
	#define tc_front(rng) (*tc::begin_not_end(rng))
	#define tc_back(rng) (*tc::end_prev(rng))
	#define tc_at(rng, i) (*tc::begin_next_not_end(rng,(i)))
	#define tc_reverse_at(rng, i) (*tc::end_prev((rng),(i)+1))

	////////////////////////////////////////////////////////////////////////////////////
	// take/drop_*_first/last_n

	template< typename Cont >
	Cont& take_first_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) {
		tc::take_inplace(cont, tc::begin_next(cont,n));
		return cont;
	}

	template< typename Rng >
	auto take_first(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) return_decltype_rvalue_by_ref(
		tc::take(std::forward<Rng>(rng), tc::begin_next(rng,n))
	)

	template< typename Cont >
	Cont& drop_first_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n) {
		tc::drop_inplace(cont, tc::begin_next(cont, n));
		return cont;
	}

	template< typename Cont >
	std::enable_if_t<!has_mem_fn_pop_front<Cont>::value, Cont&>
	drop_first_inplace(Cont& cont) {
		tc::drop_inplace(cont, tc::begin_next(cont));
		return cont;
	}

	template< typename Cont >
	std::enable_if_t<has_mem_fn_pop_front<Cont>::value, Cont&>
	drop_first_inplace(Cont& cont) {
		cont.pop_front();
		return cont;
	}

	template< typename Rng >
	auto drop_first(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) return_decltype(
		tc::drop(std::forward<Rng>(rng), tc::begin_next(rng, n))
	)

	template< typename Cont >
	Cont& take_last_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) {
		tc::drop_inplace(cont, tc::end_prev(cont, n));
		return cont;
	}

	template< typename Rng >
	auto take_last(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) return_decltype(
		tc::drop(std::forward<Rng>(rng), tc::end_prev(rng, n))
	)

	template< typename Cont >
	Cont& drop_last_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n) {
		tc::take_inplace(cont, tc::end_prev(cont, n));
		return cont;
	}

	template< typename Cont >
	std::enable_if_t<!has_mem_fn_pop_back<Cont>::value, Cont&>
	drop_last_inplace(Cont& cont) {
		tc::take_inplace(cont, tc::end_prev(cont));
		return cont;
	}

	template< typename Cont >
	std::enable_if_t<has_mem_fn_pop_back<Cont>::value, Cont&>
	drop_last_inplace(Cont& cont) {
		cont.pop_back();
		return cont;
	}

	template< typename Rng >
	auto drop_last(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) return_decltype(
		tc::take(std::forward<Rng>(rng), tc::end_prev(rng, n))
	)

	template< typename Rng >
	auto slice_by_index(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type nFrom, typename boost::range_size< std::remove_reference_t<Rng> >::type nTo) return_decltype(
		tc::slice(std::forward<Rng>(rng), tc::begin_next(rng, nFrom), tc::begin_next(rng, nTo))
	)

	//-------------------------------------------------------------------------------------------------------------------------
	// truncate

	namespace advance_forward_bounded_adl_barrier {
		template< typename It >
		std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > advance_forward_bounded_impl(
			It&& it,
			std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > n,
			tc::remove_cvref_t<It> const& itBound,
			boost::single_pass_traversal_tag
			) {
			_ASSERT(0 <= n);
			decltype(n) nCount = 0;
			while (nCount != n && it != itBound) {
				++nCount;
				++it;
			}
			return nCount;
		}

		template< typename It >
		std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > advance_forward_bounded_impl(
			It&& it,
			std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > n,
			tc::remove_cvref_t<It> const& itBound,
			boost::iterators::random_access_traversal_tag
			) {
			_ASSERT(0 <= n);
			if (tc::assign_better(n, tc::make_size_proxy(itBound - it), tc::fn_less_equal())) {
				it = itBound;
			}
			else {
				it += n;
			}
			return n;
		}
	}

	template< typename It >
	std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > advance_forward_bounded(
		It&& it,
		std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > n,
		tc::remove_cvref_t<It> const& itBound
		) {
		return advance_forward_bounded_adl_barrier::advance_forward_bounded_impl(std::forward<It>(it), n, itBound, typename boost::iterator_traversal< tc::remove_cvref_t<It> >::type());
	}

	template< typename Cont >
	Cont& truncate_inplace( Cont& rng, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) {
		auto it=boost::begin(rng);
		tc::advance_forward_bounded( it, n, boost::end(rng) );
		return tc::take_inplace( rng, tc_move(it) );
	}

	template< typename Rng >
	auto truncate(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n)
	->decltype(tc::take( std::forward<Rng>(rng), boost::begin(rng) )) {
		auto it=boost::begin(rng);
		tc::advance_forward_bounded( it, n, boost::end(rng) );
		return tc::take( std::forward<Rng>(rng), tc_move(it) );
	}



	//-------------------------------------------------------------------------------------------------------------------------
	// make iterator range

	// sub_range from iterator pair
	template< typename It >
	auto make_iterator_range_impl( It itBegin, It itEnd )
		return_ctor( tc::sub_range<tc::iterator_base<It>>, ( tc::iterator_base<It>(), tc_move(itBegin), tc_move(itEnd) ) )

	// There is an other make_iterator_range_impl overload for range adaptor based iterarors in range_adaptor.h

	// make sure ADL lookup of index_iterator::make_iterator_range works
	template< typename ItBegin, typename ItEnd >
	auto make_iterator_range(ItBegin&& itBegin, ItEnd&& itEnd)
		return_decltype( make_iterator_range_impl( std::forward<ItBegin>(itBegin), std::forward<ItEnd>(itEnd) ) )

	//-------------------------------------------------------------------------------------------------------------------------
	// make empty range

	template< typename T >
	auto make_empty_range()
		return_decltype( make_iterator_range( boost::implicit_cast<T*>(nullptr), boost::implicit_cast<T*>(nullptr) ) )

	//-------------------------------------------------------------------------------------------------------------------------
	// make counted range

	template< typename It, typename Count >
	auto make_counted_range( It const& it, Count&& count )
		return_decltype( make_iterator_range( it, it+std::forward<Count>(count) ) )

	//-------------------------------------------------------------------------------------------------------------------------
	// make singleton range
	template<typename T>
	struct singleton_range {
		singleton_range(T&& t, aggregate_tag)
			: m_pt(std::forward<T>(t), aggregate_tag())
		{}

		using iterator = std::remove_reference_t<typename tc::reference_or_value<T>::reference>*;
		using const_iterator = std::remove_reference_t<typename tc::reference_or_value<T>::const_reference>*;

		iterator begin() {
			return std::addressof(*m_pt);
		}

		const_iterator begin() const {
			return std::addressof(*m_pt);
		}

		iterator end() {
			return begin() + 1;
		}

		const_iterator end() const {
			return begin() + 1;
		}
		
		auto size() const return_decltype(
			1U
		)

	private:
		tc::reference_or_value<T> m_pt;
	};

	template< typename T >
	auto make_singleton_range(T&& t)
		return_ctor(singleton_range<T>, (std::forward<T>(t), aggregate_tag()))

	//-------------------------------------------------------------------------------------------------------------------------
	// boost::range_return with tc::sub_range

	enum range_return_value
	{
		// (*) indicates the most common values
		return_void, // always void
		return_bool, // true, singleton is false
		return_iterator_or_end, // the resulting iterator, singleton is end()
		return_iterator_or_singleton, // the resulting iterator, singleton is iterator(), only works with suitable iterators, e.g., pointers and index_iterators
		return_iterator, // the resulting iterator, singleton not allowed
		return_prev_iterator, // prior(found) iterator, singleton not allowed
		return_next_iterator, // next(found) iterator, singleton not allowed
		return_next_iterator_or_begin, // next(found) iterator, singleton is begin()
		return_iterator_or_begin, // the resulting iterator, singleton is begin()
		return_head, // [begin, found) range, singleton not allowed
		return_head_or_empty, // [begin, found) range, singleton is [begin, begin)
		return_head_or_all, // [begin, found) range, singleton is [begin, end)
		return_head_next, // [begin, next(found)) range, singleton not allowed
		return_head_next_or_empty, // [begin, next(found)) range, singleton is [begin, begin)
		return_tail, // [found, end) range, singleton not allowed
		return_tail_or_empty, // [found, end) range, singleton is [end, end)
		return_tail_next, // (found, end) range, singleton not allowed
		return_tail_next_or_all, // (found, end) range, singleton is [begin, end)
		return_index_or_npos, // return index of found, singleton is -1
		return_index, // return index of found, singleton not allowed
		return_index_or_size // return index of found, singleton is size
	};

	template< typename T >
	typename std::enable_if< tc::is_non_char_integral<T>::value && std::is_signed<T>::value,
	bool >::type npos(T t) {
		return -1 == t;
	}

	template< typename T >
	typename std::enable_if< tc::is_non_char_integral<T>::value && !std::is_signed<T>::value,
	bool >::type npos(T t) {
		return std::numeric_limits<T>::max() == t;
	}

	template< typename T >
	typename std::enable_if< !std::is_integral<T>::value,
	bool >::type npos(T const& t) {
		return tc::npos(ConvertToUnderlying(t));
	}

	template< typename Rng, range_return_value >
	struct range_return;

	template< typename Rng >
	struct range_return< Rng, return_void >
	{
		using type = void;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type, Rng&&) {}
		static type pack_singleton(Rng&& rng) {}
	};

	template< typename Rng >
	struct range_return< Rng, return_bool >
	{
		using type = bool;

		template<typename Anything>
		static type pack(Anything&&, Rng&&) {
			return true;
		}
		static type pack_singleton(Rng&& rng) {
			return false;
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_iterator_or_end >
	{
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&&) {
			return it;
		}
		static type pack_singleton(Rng&& rng) {
			return boost::end(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_iterator_or_singleton >
	{
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&&) {
			_ASSERTDEBUG(it);
			return it;
		}
		static type pack_singleton(Rng&& rng) {
			type it{}; // value initialization to initialize pointers to nullptr
			_ASSERTDEBUG(!it);
			return it;
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_iterator >
	{
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return it;
		}
		static type pack_singleton(Rng&& rng) {
			_ASSERTFALSE;
			return boost::begin(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_prev_iterator >
	{
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			_ASSERT(it != boost::begin(rng));
			return boost::prior(it);
		}
		static type pack_singleton(Rng&& rng) {
			_ASSERTFALSE;
			return boost::begin(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_next_iterator >
	{
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			_ASSERT(it != boost::end(rng));
			return boost::next(it);
		}
		static type pack_singleton(Rng&& rng) {
			_ASSERTFALSE;
			return boost::end(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_next_iterator_or_begin >
	{
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			_ASSERT(it != boost::end(rng));
			return boost::next(it);
		}
		static type pack_singleton(Rng&& rng) {
			return boost::begin(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_iterator_or_begin > {
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return it;
		}
		static type pack_singleton(Rng&& rng) {
			return boost::begin(rng);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_head >
	{
		using type = typename make_sub_range_result<Rng>::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return tc::take( std::forward<Rng>(rng), it);
		}
		static type pack_singleton(Rng&& rng) {
			_ASSERTFALSE;
			return tc::take( std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_head_or_empty >
	{
		using type = typename make_sub_range_result<Rng>::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return tc::take( std::forward<Rng>(rng), it);
		}
		static type pack_singleton(Rng&& rng) {
			return tc::take( std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_head_or_all >
	{
		using type = typename make_sub_range_result<Rng>::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return tc::take( std::forward<Rng>(rng), it);
		}
		static type pack_singleton(Rng&& rng) {
			return tc::take( std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_tail >
	{
		using type = decltype(tc::drop( std::declval<Rng&&>(), boost::begin(std::declval<Rng&>()) ));

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return tc::drop( std::forward<Rng>(rng), it );
		}
		static type pack_singleton(Rng&& rng) {
			_ASSERTFALSE;
			return tc::drop( std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_tail_or_empty >
	{
		using type = decltype(tc::drop( std::declval<Rng&&>(), boost::begin(std::declval<Rng&>()) ));

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return tc::drop( std::forward<Rng>(rng), it );
		}
		static type pack_singleton(Rng&& rng) {
			return tc::drop( std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_tail_next >
	{
		using type = decltype(tc::drop( std::declval<Rng&&>(), boost::begin(std::declval<Rng&>()) ));

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			_ASSERT(it != boost::end(rng));
			return tc::drop( std::forward<Rng>(rng), boost::next(it) );
		}
		static type pack_singleton(Rng&& rng) {
			_ASSERTFALSE;
			return tc::drop(std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_tail_next_or_all >
	{
		using type = decltype(tc::drop( std::declval<Rng&&>(), boost::begin(std::declval<Rng&>()) ));

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			_ASSERT(it != boost::end(rng));
			return tc::drop( std::forward<Rng>(rng), boost::next(it) );
		}
		static type pack_singleton(Rng&& rng) {
			return tc::drop(std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_index_or_npos > {
		// static_cast<int>(npos) must be -1. Anything else is error-prone. So use range_difference instead of range_size for now.
		// Alternatively, we could return a special type that casts npos to -1.
		using type = tc::size_proxy< typename boost::range_difference< std::remove_reference_t<Rng> >::type >;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return tc::verify_class<type>( it-boost::begin(rng) );
		}
		static type pack_singleton(Rng&&) {
			return type( static_cast<typename boost::range_difference< std::remove_reference_t<Rng> >::type>(-1) );
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_index > {
		using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return tc::verify_class<type>( it-boost::begin(rng) );
		}
		static type pack_singleton(Rng&&) {
			_ASSERTFALSE;
			return type(0);
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_index_or_size > {
		using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			return tc::verify_class<type>( it-boost::begin(rng) );
		}
		static type pack_singleton(Rng&& rng) {
			return tc::size(std::forward<Rng>(rng));
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_head_next > {
		using type = typename make_sub_range_result<Rng>::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			_ASSERT( it!=boost::end(rng) );
			return tc::take( std::forward<Rng>(rng), boost::next(it) );
		}
		static type pack_singleton(Rng&& rng) {
			_ASSERTFALSE;
			return tc::take( std::forward<Rng>(rng), boost::begin(rng) );
		}
	};

	template< typename Rng >
	struct range_return< Rng, return_head_next_or_empty > {
		using type = typename make_sub_range_result<Rng>::type;

		static type pack(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) {
			_ASSERT( it!=boost::end(rng) );
			return tc::take( std::forward<Rng>(rng), boost::next(it) );
		}
		static type pack_singleton(Rng&& rng) {
			return tc::take( std::forward<Rng>(rng), boost::begin(rng) );
		}
	};

	//-------------------------------------------------------------------------------------------------------------------------
	// as_pointers
	// get a consecutive block of memory from range and return an iterator_range of pointers

	template< typename Rng >
	auto as_pointers(Rng&& rng)->tc::sub_range<
		tc::iterator_base<
			decltype( ptr_begin( std::declval<Rng&&>() ) )
		>
	> {
		return std::forward<Rng>(rng);
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// as_array

	template< typename T, std::size_t N, std::enable_if_t<is_char<T>::value>* = nullptr >
	auto as_array(T (&at)[N] ) return_decltype(
		tc::make_counted_range( std::addressof(at[0]), N )
	)

	template< typename T >
	using ptr_range = sub_range < iterator_base<T*> >;

	//--------------------------------------------------------------------------------------------------------------------------
	// as_blob
	// reinterprets a range of items as a range of bytes

	// We use unsigned char for uninterpreted memory.
	// - The type used must support aliasing, so the only candidates are char, signed char and unsigned char.
	// - char is bad because we interpret char as UTF-8 and char* as zero-terminated range.
	// - unsigned char is better than signed char because the binary representation of signs may vary between platforms.
	// - char is bad because it is either signed or unsigned, so it has the same problem.
	// - unsigned char is better than std::uint8_t because the latter must be 8 bit, but we mean the smallest addressable unit, which is char and may be larger (or smaller?) on other platforms.

	template<typename T>
	typename std::enable_if< tc::is_range_with_iterators< T >::value,
	tc::ptr_range<unsigned char const> >::type as_blob(T const& t) {
		auto const& rng=tc::as_pointers(t);
		static_assert( std::is_trivially_copyable< typename tc::range_value< std::remove_reference_t< decltype( rng ) > >::type >::value, "as_blob only works on std::is_trivially_copyable types" );
		return tc::make_iterator_range( 
			reinterpret_cast<unsigned char const*>( boost::begin(rng) ),
			reinterpret_cast<unsigned char const*>( boost::end(rng) )
		);
	}

	template<typename T>
	tc::ptr_range<T const> as_typed_range(tc::ptr_range<unsigned char const> rng) {
		static_assert( tc::is_decayed<T>::value, "" );
		static_assert( std::is_trivially_copyable<T>::value, "" );
		_ASSERT( 0==tc::size(rng)%sizeof(T) );
		return tc::make_iterator_range( 
			reinterpret_cast<T const*>( boost::begin(rng) ),
			reinterpret_cast<T const*>( boost::end(rng) )
		);
	}

	template<typename T>
	typename std::enable_if< !tc::is_range_with_iterators< T >::value,
	tc::ptr_range<unsigned char const> >::type as_blob(T const& t) {
		return as_blob( tc::make_singleton_range(t) );
	}

	template<typename Func, typename Rng>
	auto untransform(sub_range<transform_adaptor<Func, Rng, true>&> rngsubtrans) return_decltype(
		slice(rngsubtrans.base_range().base_range(), rngsubtrans.begin_index(), rngsubtrans.end_index())
	)

	template<typename Func, typename Rng>
	auto untransform(sub_range<transform_adaptor<Func, Rng, true> const&> rngsubtrans) return_decltype(
		slice(rngsubtrans.base_range().base_range(), rngsubtrans.begin_index(), rngsubtrans.end_index())
	)

	template<typename Func, typename Rng>
	auto untransform(transform_adaptor<Func, Rng, true>&& rngtrans) -> Rng {
		return tc_move(rngtrans).base_range();
	}

	template<typename Func, typename Rng>
	auto untransform(transform_adaptor<Func, Rng, true> const& rngtrans) -> std::conditional_t<std::is_lvalue_reference<Rng>::value, Rng, Rng const&> {
		return rngtrans.base_range();
	}

	template<typename Func, typename Rng>
	auto untransform(transform_adaptor<Func, Rng, true> & rngtrans) -> std::add_lvalue_reference_t<Rng> {
		return rngtrans.base_range();
	}
}
