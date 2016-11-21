//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"

#include "tc_move.h"
#include "range_adaptor.h"
#include "container_traits.h"
#include "meta.h"
#include "size.h"
#include "assign.h"

#include <boost/optional.hpp>

#include <type_traits>

namespace tc {
	//-------------------------------------------------------------------------------------------------------------------------
	// meta function to determine the correct type
	// may_aggregate<Rng>::type does not work because sub_range::iterator must be an iterator of the underlying range.
	// Being an iterator of a copy of the underlying range is not sufficient.
	template< typename Rng, typename Enable >
	struct make_sub_range_result final {
		using type = sub_range< Rng >;
	};
	
	template< typename Rng >
	struct make_sub_range_result< Rng, std::enable_if_t<
		!std::is_same< Rng, view_by_value_t<Rng> >::value
	> > final {
		using type = typename make_sub_range_result< view_by_value_t<Rng> >::type;
	};

	// collapse sub_range< sub_range< ... > > to single sub_range
	template< typename Rng >
	struct make_sub_range_result< sub_range<Rng> > final {
		using type = typename make_sub_range_result<Rng>::type;
	};

	// put transform_adaptor outside of sub_range (to allow tc::equal_range( tc::transform( rng, func ) ).base_range())
	template< typename Func, typename Rng > 
	struct make_sub_range_result< transform_adaptor<Func,Rng,true> > final {
		using type = transform_adaptor<Func, typename make_sub_range_result<
			Rng
		>::type, true >;
	};

	//-------------------------------------------------------------------------------------------------------------------------
	template<typename T>
	T* raw_ptr(T* t) noexcept { return t; } // overloaded e.g. for boost::interprocess::offset_ptr
	
	template<typename Rng, std::enable_if_t<std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value>* = nullptr>
	auto ptr_begin(Rng&& rng) noexcept return_decltype(
		boost::begin(rng) // not std::forward<Rng>(rng) : there is no overload for boost::begin(Rng&&), rvalues bind to boost::begin(Rng const&)
	)
	template<typename Rng, std::enable_if_t<std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value>* = nullptr>
	auto ptr_end(Rng&& rng) noexcept return_decltype(
		boost::end(rng) // not std::forward<Rng>(rng) : there is no overload for boost::end(Rng&&), rvalues bind to boost::end(Rng const&)
	)

	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value 
			&& !(tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value )
		>* = nullptr
	>
	auto ptr_begin(Rng&& rng) noexcept return_decltype(
		raw_ptr( rng.data() )
	)
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value 
			&& !(tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value )
		>* = nullptr
	>
	auto ptr_end(Rng&& rng) noexcept return_decltype(
		raw_ptr( rng.data() ) + rng.size()
	)
	
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value 
			&& tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value
		>* = nullptr
	>
	auto ptr_begin(Rng&& rng) noexcept return_decltype(
		tc::make_mutable_ptr(raw_ptr( rng.data() ))
	)
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >::value 
			&& tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value
		>* = nullptr
	>
	auto ptr_end(Rng&& rng) noexcept return_decltype(
		tc::make_mutable_ptr(raw_ptr( rng.data() )) + rng.size()
	)
	
	//-------------------------------------------------------------------------------------------------------------------------
	// fwd decls 
	template< typename Cont >
	Cont& take_inplace( Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it ) noexcept;

	template< typename Cont >
	Cont& drop_inplace( Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it ) noexcept;

	template< typename It >
	std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > advance_forward_bounded(
		It&& it,
		std::make_unsigned_t< typename boost::iterator_difference< tc::remove_cvref_t<It> >::type > n,
		tc::remove_cvref_t<It> const& itBound
	) noexcept;

	//-------------------------------------------------------------------------------------------------------------------------

	namespace sub_range_impl {
		template< typename Rng >
		struct whole_range_sub_range_helper_base {
			template<typename Rhs>
			static auto base_range(Rhs&& rhs) noexcept
				return_decltype_rvalue_by_ref( std::forward<Rhs>(rhs) )
			template<typename Rhs>
			static auto begin_index(sub_range<Rng>& lhs, Rhs&&) noexcept
				return_decltype( tc::base_cast<range_adaptor< sub_range<Rng>, Rng > >(lhs).STATIC_VIRTUAL_METHOD_NAME(begin_index)() )
			template<typename Rhs>
			static auto end_index(sub_range<Rng>& lhs, Rhs&&) noexcept
				return_decltype( tc::base_cast<range_adaptor< sub_range<Rng>, Rng > >(lhs).STATIC_VIRTUAL_METHOD_NAME(end_index)() )
		};

		template< typename It >
		struct whole_range_sub_range_helper_base<iterator_base<It>> {
			template<typename Rhs>
			static auto base_range(Rhs &&) noexcept
				return_decltype( iterator_base<It>() )
			template<typename Rhs>
			static auto begin_index(sub_range<iterator_base<It>>&, Rhs&& rng) noexcept
				return_decltype( tc::iterator2index(boost::begin(rng)) )
			template<typename Rhs>
			static auto end_index(sub_range<iterator_base<It>>&, Rhs&& rng) noexcept
				return_decltype( tc::iterator2index(boost::end(rng)) )
		};

		template< typename T >
		struct whole_range_sub_range_helper_base<iterator_base<T*>> {
			template<typename Rhs>
			static auto base_range(Rhs &&) noexcept
				return_decltype( iterator_base<T*>() )
			template<typename Rhs>
			static auto begin_index(sub_range<iterator_base<T*>>&, Rhs&& rng) noexcept
				return_decltype( tc::iterator2index(ptr_begin(rng)) )
			template<typename Rhs>
			static auto end_index(sub_range<iterator_base<T*>>&, Rhs&& rng) noexcept
				return_decltype( tc::iterator2index(ptr_end(rng)) )
		};

		template< typename Rng >
		struct whole_range_sub_range_helper final : whole_range_sub_range_helper_base<Rng> {
			using whole_range_sub_range_helper_base<Rng>::base_range;
			using whole_range_sub_range_helper_base<Rng>::begin_index;
			using whole_range_sub_range_helper_base<Rng>::end_index;

			template<typename Rhs>
			static auto base_range(sub_range<Rhs>&& rhs) noexcept
				return_decltype_rvalue_by_ref( whole_range_sub_range_helper<Rng>::base_range( tc_move(rhs).base_range_move() ) )
			template<typename Rhs>
			static auto begin_index(sub_range<Rng>&, sub_range<Rhs>&& rhs) noexcept
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			static auto end_index(sub_range<Rng>&, sub_range<Rhs>&& rhs) noexcept
				return_decltype( rhs.end_index() )

			template<typename Rhs>
			static auto base_range(sub_range<Rhs> const& rhs) noexcept
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( rhs.base_range() ) )
			template<typename Rhs>
			static auto begin_index(sub_range<Rng>&, sub_range<Rhs> const& rhs) noexcept
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			static auto end_index(sub_range<Rng>&, sub_range<Rhs> const& rhs) noexcept
				return_decltype( rhs.end_index() )

			template<typename Rhs>
			static auto base_range(sub_range<Rhs>& rhs) noexcept
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( rhs.base_range() ) )
			template<typename Rhs>
			static auto begin_index(sub_range<Rng>&, sub_range<Rhs>& rhs) noexcept
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			static auto end_index(sub_range<Rng>&, sub_range<Rhs>& rhs) noexcept
				return_decltype( rhs.end_index() )

			// Fallback for is_compatible_range below
			struct incompatible_index final {};
			static incompatible_index begin_index(sub_range<Rng>&, ...) noexcept;
		};

		template< typename Rng >
		struct sub_range : range_adaptor< sub_range<Rng>, Rng > {
			static_assert(
				!tc::is_view<Rng>::value || std::is_same< Rng, view_by_value_t<Rng> >::value,
				"sub_range must hold views by value."
			);
			static_assert(
				tc::is_view<Rng>::value || std::is_reference<Rng>::value,
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
			
			template<typename RngOther>
			struct delayed_test_conversion_to_index {
				using type=std::is_constructible<
					index,
					decltype( whole_range_sub_range_helper<Rng>::begin_index(
						std::declval<this_type&>(),
						std::declval<RngOther>()
					) )>;
			};

#ifdef _MSC_VER // compiler bug ?
			template<std::size_t N>
			struct delayed_test_conversion_to_index<sub_range(&)[N]> : std::false_type {};

			template<std::size_t N>
			struct delayed_test_conversion_to_index<sub_range const(&)[N]> : std::false_type {};
#endif
			
			template<typename RngOther>
			using is_compatible_range =
				typename boost::mpl::eval_if_c<
					is_range_with_iterators<RngOther>::value,
					delayed_test_conversion_to_index<RngOther>,
					std::false_type
				>::type;
		public:
			// default ctor (for deferred initialization)
			sub_range() noexcept
			{}

			template<typename RngOther, std::enable_if_t< is_compatible_range<RngOther>::value >* =nullptr> 
			sub_range( RngOther&& rng ) noexcept
				: base_( aggregate_tag(), whole_range_sub_range_helper<Rng>::base_range(
					std::forward<RngOther>(rng) 
				) )
				, m_idxBegin(whole_range_sub_range_helper<Rng>::begin_index(
					*this,
					std::forward<RngOther>(rng) 
				))
				, m_idxEnd(whole_range_sub_range_helper<Rng>::end_index(
					*this,
					std::forward<RngOther>(rng) 
				))
			{}

			// some user-defined copy ctor to disable implicit one, with same semantics as templated copy ctor
			sub_range( sub_range const& rng) noexcept
				: base_( aggregate_tag(), whole_range_sub_range_helper<Rng>::base_range(
					rng
				) )
				, m_idxBegin(whole_range_sub_range_helper<Rng>::begin_index(
					*this,
					rng
				))
				, m_idxEnd(whole_range_sub_range_helper<Rng>::end_index(
					*this,
					rng
				))
			{}

			template<typename Rhs>
			explicit sub_range( Rhs&& rng,
				index idxBegin,
				index idxEnd ) noexcept
			: base_(aggregate_tag(), whole_range_sub_range_helper<Rng>::base_range(
				std::forward<Rhs>(rng)
			))
			, m_idxBegin(tc_move(idxBegin))
			, m_idxEnd(tc_move(idxEnd))
			{}

			template<typename Rhs>
			explicit sub_range( Rhs&& rng,
				typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin,
				typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd ) noexcept
			: base_(aggregate_tag(), whole_range_sub_range_helper<Rng>::base_range(
				std::forward<Rhs>(rng)
			))
			, m_idxBegin(tc::iterator2index(tc_move(itBegin)))
			, m_idxEnd(tc::iterator2index(tc_move(itEnd)))
			{}

			template< typename Func > break_or_continue operator()(Func func) /* no & */ MAYTHROW {
				break_or_continue bc=continue_;
				for( index i=this->begin_index();
					!this->at_end_index(i) && continue_==(bc=continue_if_not_break( func, this->dereference_index(i) ));
					this->increment_index(i) );
				return bc;
			}

			template< typename Func > break_or_continue operator()(Func func) const/* no & */ MAYTHROW {
				break_or_continue bc=continue_;
				for( index i=this->begin_index();
					!this->at_end_index(i) && continue_==(bc=continue_if_not_break( func, this->dereference_index(i) ));
					this->increment_index(i) );
				return bc;
			}

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return m_idxBegin;
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return m_idxEnd;
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return base_::equal_index( idx, m_idxEnd );
			}

			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			// sub_range::iterator is the same type as the base range iterator:
			using iterator = typename boost::range_iterator< std::remove_reference_t< typename reference_or_value< Rng >::reference > >::type;
			using const_iterator = typename boost::range_iterator< std::remove_reference_t< typename reference_or_value< Rng >::const_reference > >::type;

			const_iterator make_iterator( index idx ) const &  noexcept {
				return base_::base_range().make_iterator(tc_move(idx));
			}

			const_iterator begin() const& noexcept {
				return make_iterator(this->begin_index());
			}

			const_iterator end() const& noexcept {
				return make_iterator(this->end_index());
			}

			iterator make_iterator( index idx ) & noexcept {
				return base_::base_range().make_iterator(tc_move(idx));
			}

			iterator begin() & noexcept {
				return make_iterator(this->begin_index());
			}

			iterator end() & noexcept {
				return make_iterator(this->end_index());
			}

			template< typename It >
			friend void take_inplace_impl( sub_range& rng, It&& it ) noexcept {
				rng.m_idxEnd=tc::iterator2index( std::forward<It>(it) );
			}

			template< typename It >
			friend void drop_inplace_impl( sub_range& rng, It&& it ) noexcept {
				rng.m_idxBegin=tc::iterator2index( std::forward<It>(it) );
			}

			template< typename It >
			friend sub_range&& take( sub_range&& rng, It&& it ) noexcept {
				tc::take_inplace(rng,std::forward<It>(it));
				return tc_move(rng);
			}

			template< typename It >
			friend sub_range&& drop( sub_range&& rng, It&& it ) noexcept {
				tc::drop_inplace(rng,std::forward<It>(it));
				return tc_move(rng);
			}
		};
	}
	using sub_range_impl::sub_range;

	template< typename Rng >
	auto make_range(Rng&& rng) noexcept return_ctor(
		typename make_sub_range_result< Rng >::type,
		(std::forward<Rng>(rng), boost::begin(rng), boost::end(rng))
	)

	DEFINE_FN(make_range);

	//-------------------------------------------------------------------------------------------------------------------------
	// slice

	// slice from range + iterator pair
	// slice from range + difference
	template< typename Rng, typename Begin, typename End >
	auto slice(Rng&& rng, Begin&& begin, End&& end) noexcept return_ctor(
		typename make_sub_range_result< Rng >::type,
		(std::forward<Rng>(rng), std::forward<Begin>(begin), std::forward<End>(end))
	)

	//-------------------------------------------------------------------------------------------------------------------------
	// take

	template< typename Cont, typename It >
	void take_inplace_impl( Cont & cont, It&& it ) noexcept {
		cont.erase( it, boost::end(cont) );
	}

	template< typename Cont >
	Cont& take_inplace( Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it ) noexcept {
		take_inplace_impl(cont,tc_move(it)); // allow ADL
		return cont;
	}

	template< typename C, typename T, typename A, typename It >
	std::basic_string<C,T,A> && take( std::basic_string<C,T,A>&& rng, It&& it ) noexcept {
		tc::take_inplace(rng,std::forward<It>(it));
		return tc_move(rng);
	}

	template< typename Rng, typename End >
	auto take(Rng&& rng, End&& end) noexcept return_ctor(
		typename make_sub_range_result< Rng >::type,
		(std::forward<Rng>(rng), boost::begin(rng), std::forward<End>(end))
	)

	//-------------------------------------------------------------------------------------------------------------------------
	// drop
	template< typename T >
	using is_char_ptr=std::integral_constant<bool,
		std::is_pointer<
			std::decay_t<T>
		>::value &&
		tc::is_char<
			std::remove_pointer_t<std::decay_t<T>>
		>::value
	>;

	// drop_inplace
	template< typename Cont, typename It, std::enable_if_t<!tc::is_char_ptr<Cont>::value>* = nullptr>
	void drop_inplace_impl( Cont & cont, It&& it ) noexcept {
		cont.erase( boost::begin(cont), it );
	}

	template< typename CharPtr, typename It, std::enable_if_t<tc::is_char_ptr<CharPtr>::value>* = nullptr>
	void drop_inplace_impl( CharPtr& pch, It&& it ) noexcept {
		pch=std::forward<It>(it);
	}

	template< typename Cont >
	Cont& drop_inplace( Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it ) noexcept {
		drop_inplace_impl(cont,tc_move(it));
		return cont;
	}

	template< typename Rng, std::enable_if_t<!tc::is_char_ptr< Rng >::value>* = nullptr>
	typename make_sub_range_result< Rng >::type drop_impl(Rng&& rng,
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin
	) noexcept {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), itBegin, boost::end(rng) );
	}

	// C strings have efficient in-place drop
	template< typename CharPtr, typename It, std::enable_if_t<tc::is_char_ptr< CharPtr >::value>* = nullptr>
	std::decay_t<CharPtr> drop_impl( CharPtr&& pch, It&& it ) noexcept {
		std::decay_t<CharPtr> pchCopy=std::forward<CharPtr>(pch);
		tc::drop_inplace( pchCopy, std::forward<It>(it) );
		return pchCopy;
	}

	template< typename Rng, typename It >
	auto drop(Rng&& rng, It&& it) noexcept return_decltype(
		drop_impl( std::forward<Rng>(rng), std::forward<It>(it) )
	)

	////////////////////////////////
	// front/back on ranges

	template< typename It, typename Rng >
	It && verify_not_end(It&& it, Rng const& rng) noexcept {
		_ASSERT(boost::end(rng) != it);
		return std::forward<It>(it);
	}

	namespace begin_next_adl_barrier {
		template< bool bLinear, typename Rng >
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type
		begin_next(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::forward_traversal_tag
		) noexcept {
			_ASSERT(0 <= n);
			_ASSERTNOTIFY(bLinear || n <= 2);
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

		template< bool /*bLinear*/, typename Rng >
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type
		begin_next(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::random_access_traversal_tag
		) noexcept {
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
	) noexcept {
		return begin_next_adl_barrier::begin_next</*bLinear*/false>(std::forward<Rng>(rng), n, typename boost::range_traversal< std::remove_reference_t<Rng> >::type());
	}

	template< typename Rng >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type
	linear_begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return begin_next_adl_barrier::begin_next</*bLinear*/true>(std::forward<Rng>(rng), n, typename boost::range_traversal< std::remove_reference_t<Rng> >::type());
	}


	namespace end_prev_adl_barrier {
		template< typename Rng >
		typename boost::range_iterator< std::remove_reference_t<Rng> >::type
		end_prev(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::bidirectional_traversal_tag
		) noexcept {
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
		) noexcept {
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
	) noexcept {
		return end_prev_adl_barrier::end_prev(std::forward<Rng>(rng), n, typename boost::range_traversal< std::remove_reference_t<Rng> >::type());
	}

	template< typename Rng >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type
	begin_not_end(
		Rng&& rng
	) noexcept {
		return tc::verify_not_end(boost::begin(rng), rng);
	}

	template< typename Rng >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type
	begin_next_not_end(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n = 1
	) noexcept {
		return tc::verify_not_end(tc::begin_next(rng, n), rng);
	}

	template< typename Rng >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type
	linear_begin_next_not_end(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n
	) noexcept {
		return tc::verify_not_end(tc::linear_begin_next(rng, n), rng);
	}

	// Write as macros to keep temporary iterators alive.
	// By standard, the lifetime of a reference is limited to the lifetime of the iterator.
	#define tc_front(rng) (*tc::begin_not_end(rng))
	#define tc_back(rng) (*tc::end_prev(rng))
	#define tc_at(rng, i) (*tc::begin_next_not_end(rng,(i)))
	#define tc_linear_at(rng, i) (*tc::linear_begin_next_not_end(rng,(i)))
	#define tc_reverse_at(rng, i) (*tc::end_prev((rng),(i)+1))

	////////////////////////////////////////////////////////////////////////////////////
	// take/drop_*_first/last_n

	template< typename Cont >
	Cont& take_first_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) noexcept {
		tc::take_inplace(cont, tc::begin_next(cont,n));
		return cont;
	}

	template< typename Rng >
	auto take_first(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) noexcept return_decltype_rvalue_by_ref(
		tc::take(std::forward<Rng>(rng), tc::begin_next(rng,n))
	)

	template< typename Cont >
	Cont& drop_first_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n) noexcept {
		tc::drop_inplace(cont, tc::begin_next(cont, n));
		return cont;
	}

	template< typename Cont, std::enable_if_t<!has_mem_fn_pop_front<Cont>::value>* = nullptr>
	Cont& drop_first_inplace(Cont& cont) noexcept {
		tc::drop_inplace(cont, tc::begin_next(cont));
		return cont;
	}

	template< typename Cont, std::enable_if_t<has_mem_fn_pop_front<Cont>::value>* = nullptr>
	Cont& drop_first_inplace(Cont& cont) noexcept {
		cont.pop_front();
		return cont;
	}

	template< typename Rng >
	auto drop_first(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) noexcept return_decltype(
		tc::drop(std::forward<Rng>(rng), tc::begin_next(rng, n))
	)

	template< typename Cont >
	Cont& take_last_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) noexcept {
		tc::drop_inplace(cont, tc::end_prev(cont, n));
		return cont;
	}

	template< typename Rng >
	auto take_last(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) noexcept return_decltype(
		tc::drop(std::forward<Rng>(rng), tc::end_prev(rng, n))
	)

	template< typename Cont >
	Cont& drop_last_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n) noexcept {
		tc::take_inplace(cont, tc::end_prev(cont, n));
		return cont;
	}

	template< typename Cont, std::enable_if_t<!has_mem_fn_pop_back<Cont>::value>* = nullptr>
	Cont& drop_last_inplace(Cont& cont) noexcept {
		tc::take_inplace(cont, tc::end_prev(cont));
		return cont;
	}

	template< typename Cont, std::enable_if_t<has_mem_fn_pop_back<Cont>::value>* = nullptr>
	Cont& drop_last_inplace(Cont& cont) noexcept {
		cont.pop_back();
		return cont;
	}

	template< typename Rng >
	auto drop_last(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) noexcept return_decltype(
		tc::take(std::forward<Rng>(rng), tc::end_prev(rng, n))
	)

	template< typename Rng >
	auto slice_by_index(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type nFrom, typename boost::range_size< std::remove_reference_t<Rng> >::type nTo) noexcept return_decltype(
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
			) noexcept {
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
			) noexcept {
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
	) noexcept {
		return advance_forward_bounded_adl_barrier::advance_forward_bounded_impl(std::forward<It>(it), n, itBound, typename boost::iterator_traversal< tc::remove_cvref_t<It> >::type());
	}

	template< typename Cont >
	Cont& truncate_inplace( Cont& rng, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
		auto it=boost::begin(rng);
		tc::advance_forward_bounded( it, n, boost::end(rng) );
		return tc::take_inplace( rng, tc_move(it) );
	}

	template< typename Rng >
	auto truncate(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n) noexcept
	->decltype(tc::take( std::forward<Rng>(rng), boost::begin(rng) )) {
		auto it=boost::begin(rng);
		tc::advance_forward_bounded( it, n, boost::end(rng) );
		return tc::take( std::forward<Rng>(rng), tc_move(it) );
	}



	//-------------------------------------------------------------------------------------------------------------------------
	// make iterator range

	// sub_range from iterator pair
	template< typename It >
	auto make_iterator_range_impl( It itBegin, It itEnd ) noexcept
		return_ctor( tc::sub_range<tc::iterator_base<It>>, ( tc::iterator_base<It>(), tc_move(itBegin), tc_move(itEnd) ) )

	// There is an other make_iterator_range_impl overload for range adaptor based iterarors in range_adaptor.h

	// make sure ADL lookup of index_iterator::make_iterator_range works
	template< typename ItBegin, typename ItEnd >
	auto make_iterator_range(ItBegin&& itBegin, ItEnd&& itEnd) noexcept
		return_decltype( make_iterator_range_impl( std::forward<ItBegin>(itBegin), std::forward<ItEnd>(itEnd) ) )

	//-------------------------------------------------------------------------------------------------------------------------
	// make empty range

	template< typename T >
	auto make_empty_range() noexcept
		return_decltype( make_iterator_range( boost::implicit_cast<T*>(nullptr), boost::implicit_cast<T*>(nullptr) ) )

	//-------------------------------------------------------------------------------------------------------------------------
	// make counted range

	template< typename It, typename Count >
	auto make_counted_range( It const& it, Count&& count ) noexcept
		return_decltype( make_iterator_range( it, it+std::forward<Count>(count) ) )

	//-------------------------------------------------------------------------------------------------------------------------
	// make singleton range
	template<typename T>
	struct singleton_range {
		singleton_range(aggregate_tag, T&& t) noexcept
			: m_pt(aggregate_tag(), std::forward<T>(t))
		{}

		using iterator = std::remove_reference_t<typename tc::reference_or_value<T>::reference>*;
		using const_iterator = std::remove_reference_t<typename tc::reference_or_value<T>::const_reference>*;

		iterator begin() & noexcept {
			return std::addressof(*m_pt);
		}

		const_iterator begin() const& noexcept {
			return std::addressof(*m_pt);
		}

		iterator end() & noexcept {
			return begin() + 1;
		}

		const_iterator end() const& noexcept {
			return begin() + 1;
		}
		
		auto size() const& noexcept return_decltype(
			1U
		)

	private:
		tc::reference_or_value<T> m_pt;
	};

	template< typename T >
	auto make_singleton_range(T&& t) noexcept
		return_ctor(singleton_range<T>, (aggregate_tag(), std::forward<T>(t)))

	template< typename T, std::enable_if_t<tc::is_actual_integer<T>::value && std::is_signed<T>::value>* = nullptr>
	bool npos(T t) noexcept {
		return -1 == t;
	}

	template< typename T, std::enable_if_t<tc::is_actual_integer<T>::value && !std::is_signed<T>::value>* = nullptr>
	bool npos(T t) noexcept {
		return std::numeric_limits<T>::max() == t;
	}

	template< typename T, std::enable_if_t<!std::is_integral<T>::value>* = nullptr>
	bool npos(T const& t) noexcept {
		return tc::npos(ConvertToUnderlying(t));
	}

	/////////////////////////////////////
	// no return

	template< typename Rng >
	struct return_void final {
		using type = void;

		static type pack_bound(typename boost::range_iterator< std::remove_reference_t<Rng> >::type, Rng&&) noexcept {}
		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type, Rng&&, Ref&&) noexcept {}
		static type pack_no_element(Rng&& rng) noexcept {}
	};

	/////////////////////////////////////
	// controlling bound return

	// returning element

	template< typename Rng >
	struct return_element_before final {
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		static type pack_bound(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
			_ASSERT(it != boost::begin(rng));
			return boost::prior(it);
		}
	};

	// returning bound

	template< typename Rng >
	struct return_border final {
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		static type pack_bound(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
			return it;
		}
	};

	template< typename Rng >
	struct return_border_index final {
		using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;

		static type pack_bound(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
			return tc::verify_class<type>(it - boost::begin(rng));
		}
	};

	// returning range

	template< typename Rng >
	struct return_take final {
		using type = typename make_sub_range_result<Rng>::type;

		static type pack_bound(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
			return tc::take(std::forward<Rng>(rng), it);
		}
	};

	template< typename Rng >
	struct return_drop final {
		using type = decltype(tc::drop(std::declval<Rng>(), boost::begin(std::declval<Rng&>())));

		static type pack_bound(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
			return tc::drop(std::forward<Rng>(rng), it);
		}
	};

	/////////////////////////////////////
	// controlling element return

	template< typename Rng >
	struct return_bool {
		using type = bool;

		template<typename Anything, typename Ref>
		static type pack_element(Anything&&, Rng&&, Ref&&) noexcept {
			return true;
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return false;
		}
	};

	// returning element

	template< typename Rng >
	struct return_element final {
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return it;
		}
		static type pack_no_element(Rng&& rng) noexcept {
			_ASSERTFALSE;
			return boost::begin(rng);
		}
	};

	template< typename Rng >
	struct return_element_or_null final {
		using type = element_t< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&&, Ref&&) noexcept {
			return static_cast<type>(it);
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return type{}; // value initialization to initialize pointers to nullptr
		}
	};

	template< typename Rng >
	struct return_element_or_front final {
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return it;
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return boost::begin(rng);
		}
	};

	template< typename Rng >
	struct return_value final {
		using type = tc::decay_t< typename boost::range_reference< std::remove_reference_t<Rng> >::type >;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type, Rng&&, Ref&& ref) noexcept {
			return std::forward<Ref>(ref);
		}
		static type pack_no_element(Rng&&) noexcept {
			_ASSERTFALSE;
			return ConstructDefaultOrTerminate<type>();
		}
	};

	template< typename Rng >
	struct return_value_or_default final {
		using type = tc::decay_t< typename boost::range_reference< std::remove_reference_t<Rng> >::type >;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type, Rng&&, Ref&& ref) noexcept {
			return std::forward<Ref>(ref);
		}
		static type pack_no_element(Rng&&) noexcept {
			return{};
		}
	};

	template< typename Rng >
	struct return_value_or_none final {
		using type = boost::optional< tc::decay_t< typename boost::range_reference< std::remove_reference_t<Rng> >::type > >;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type, Rng&&, Ref&& ref) noexcept {
			return std::forward<Ref>(ref);
		}
		static type pack_no_element(Rng&&) noexcept {
			return boost::none;
		}
	};

	template< typename Rng >
	struct return_element_index final {
		using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::verify_class<type>(it - boost::begin(rng));
		}
		static type pack_no_element(Rng&&) noexcept {
			_ASSERTFALSE;
			return type(0);
		}
	};

	template< typename Rng >
	struct return_element_index_or_npos final {
		// static_cast<int>(npos) must be -1. Anything else is error-prone. So use range_difference instead of range_size for now.
		// Alternatively, we could return a special type that casts npos to -1.
		using type = tc::size_proxy< typename boost::range_difference< std::remove_reference_t<Rng> >::type >;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::verify_class<type>(it - boost::begin(rng));
		}
		static type pack_no_element(Rng&&) noexcept {
			return type(static_cast<typename boost::range_difference< std::remove_reference_t<Rng> >::type>(-1));
		}
	};

	template< typename Rng >
	struct return_element_index_or_size final {
		using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::verify_class<type>(it - boost::begin(rng));
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return tc::size(rng);
		}
	};

	// returning bound

	template< typename Rng >
	struct return_border_after final {
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return boost::next(it);
		}
		static type pack_no_element(Rng&& rng) noexcept {
			_ASSERTFALSE;
			return boost::end(rng);
		}
	};

	template< typename Rng >
	struct return_border_after_or_begin final {
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return boost::next(it);
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return boost::begin(rng);
		}
	};

	template< typename Rng >
	struct return_border_before_or_end final {
		using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&&, Ref&&) noexcept {
			return it;
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return boost::end(rng);
		}
	};

	// returning range

	template< typename Rng >
	struct return_take_before final {
		using type = typename make_sub_range_result<Rng>::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::take(std::forward<Rng>(rng), it);
		}
		static type pack_no_element(Rng&& rng) noexcept {
			_ASSERTFALSE;
			// safe choice is empty because result may be empty
			return tc::take(std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct return_take_before_or_empty final {
		using type = typename make_sub_range_result<Rng>::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::take(std::forward<Rng>(rng), it);
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return tc::take(std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct return_take_before_or_all final {
		using type = typename make_sub_range_result<Rng>::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::take(std::forward<Rng>(rng), it);
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return tc::take(std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct return_take_after final {
		using type = typename make_sub_range_result<Rng>::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::take(std::forward<Rng>(rng), boost::next(it));
		}
		static type pack_no_element(Rng&& rng) noexcept {
			_ASSERTFALSE;
			// safe choice is all because result is never empty
			return tc::take(std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct return_take_after_or_empty final {
		using type = typename make_sub_range_result<Rng>::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::take(std::forward<Rng>(rng), boost::next(it));
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return tc::take(std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct return_take_after_or_all final {
		using type = typename make_sub_range_result<Rng>::type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::take(std::forward<Rng>(rng), boost::next(it));
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return tc::take(std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct return_drop_before final {
		using type = decltype(tc::drop(std::declval<Rng>(), boost::begin(std::declval<Rng&>())));

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::drop(std::forward<Rng>(rng), it);
		}
		static type pack_no_element(Rng&& rng) noexcept {
			_ASSERTFALSE;
			// safe choice is all because result is never empty
			return tc::drop(std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct return_drop_before_or_empty final {
		using type = decltype(tc::drop( std::declval<Rng>(), boost::begin(std::declval<Rng&>()) ));

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::drop( std::forward<Rng>(rng), it );
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return tc::drop( std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct return_drop_before_or_all final {
		using type = decltype(tc::drop(std::declval<Rng>(), boost::begin(std::declval<Rng&>())));

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::drop(std::forward<Rng>(rng), it);
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return tc::drop(std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct return_drop_after final {
		using type = decltype(tc::drop(std::declval<Rng>(), boost::begin(std::declval<Rng&>())));

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) {
			return tc::drop(std::forward<Rng>(rng), boost::next(it));
		}
		static type pack_no_element(Rng&& rng) {
			_ASSERTFALSE;
			// safe choice is empty because result may be empty
			return tc::drop(std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	template< typename Rng >
	struct return_drop_after_or_empty final {
		using type = decltype(tc::drop( std::declval<Rng>(), boost::begin(std::declval<Rng&>()) ));

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) {
			return tc::drop( std::forward<Rng>(rng), boost::next(it) );
		}
		static type pack_no_element(Rng&& rng) {
			return tc::drop(std::forward<Rng>(rng), boost::end(rng));
		}
	};

	template< typename Rng >
	struct return_drop_after_or_all final {
		using type = decltype(tc::drop( std::declval<Rng>(), boost::begin(std::declval<Rng&>()) ));

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
			return tc::drop( std::forward<Rng>(rng), boost::next(it) );
		}
		static type pack_no_element(Rng&& rng) noexcept {
			return tc::drop(std::forward<Rng>(rng), boost::begin(rng));
		}
	};

	//-------------------------------------------------------------------------------------------------------------------------
	// as_pointers
	// get a consecutive block of memory from range and return an iterator_range of pointers

	template< typename Rng >
	auto as_pointers(Rng&& rng) noexcept ->tc::sub_range<
		tc::iterator_base<
			decltype( ptr_begin( std::declval<Rng>() ) )
		>
	> {
		return std::forward<Rng>(rng);
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// as_array

	template< typename T, std::size_t N, std::enable_if_t<is_char<T>::value>* = nullptr >
	auto as_array(T (&at)[N] ) noexcept return_decltype(
		tc::make_counted_range( std::addressof(at[0]), N )
	)

	template< typename T, std::size_t N, std::enable_if_t<is_char<T>::value && std::is_const<T>::value && !std::is_volatile<T>::value>* = nullptr >
	auto string_literal(T (&at)[N] ) noexcept return_decltype(
		tc::make_counted_range( std::addressof(at[0]), N-1 )
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

	static_assert(!tc::is_range_with_iterators< void const* >::value,"");

	template<typename T, std::enable_if_t<tc::is_range_with_iterators< T >::value>* = nullptr>
	tc::ptr_range<unsigned char const> as_blob(T const& t) noexcept {
		auto const& rng=tc::as_pointers(t);
		static_assert( std::is_trivially_copyable< typename tc::range_value< std::remove_reference_t< decltype( rng ) > >::type >::value, "as_blob only works on std::is_trivially_copyable types" );
		return tc::make_iterator_range( 
			reinterpret_cast<unsigned char const*>( tc::ptr_begin(rng) ),
			reinterpret_cast<unsigned char const*>( tc::ptr_end(rng) )
		);
	}

	template<typename T>
	tc::ptr_range<T const> as_typed_range(tc::ptr_range<unsigned char const> rng) noexcept {
		static_assert( tc::is_decayed<T>::value, "" );
		static_assert( std::is_trivially_copyable<T>::value, "" );
		_ASSERT( 0==tc::size(rng)%sizeof(T) );
		return tc::make_iterator_range( 
			reinterpret_cast<T const*>(tc::ptr_begin(rng) ),
			reinterpret_cast<T const*>(tc::ptr_end(rng) )
		);
	}

	template<typename T, std::enable_if_t<!tc::is_range_with_iterators< T >::value>* = nullptr>
	tc::ptr_range<unsigned char const> as_blob(T const& t) noexcept {
		return as_blob( tc::make_singleton_range(t) );
	}

	template<typename Func, typename Rng>
	auto untransform(sub_range<transform_adaptor<Func, Rng, true>&> rngsubtrans) noexcept return_decltype(
		slice(rngsubtrans.base_range().base_range(), rngsubtrans.begin_index(), rngsubtrans.end_index())
	)

	template<typename Func, typename Rng>
	auto untransform(sub_range<transform_adaptor<Func, Rng, true> const&> rngsubtrans) noexcept return_decltype(
		slice(rngsubtrans.base_range().base_range(), rngsubtrans.begin_index(), rngsubtrans.end_index())
	)

	template<typename Func, typename Rng>
	auto untransform(transform_adaptor<Func, Rng, true>&& rngtrans) noexcept -> Rng {
		return tc_move(rngtrans).base_range();
	}

	template<typename Func, typename Rng>
	auto untransform(transform_adaptor<Func, Rng, true> const& rngtrans) noexcept -> std::conditional_t<std::is_lvalue_reference<Rng>::value, Rng, Rng const&> {
		return rngtrans.base_range();
	}

	template<typename Func, typename Rng>
	auto untransform(transform_adaptor<Func, Rng, true> & rngtrans) noexcept -> std::add_lvalue_reference_t<Rng> {
		return rngtrans.base_range();
	}
}
