//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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
	// may_aggregate<Rng>::type does not work because boost::range_iterator<sub_range>::type must be an iterator of the underlying range.
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

	TC_HAS_EXPR(ptr_begin, (T), tc::ptr_begin(std::declval<T>()))
	
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

			// Fallback for test_conversion_to_index below
			struct incompatible_index final {};
			static incompatible_index begin_index(sub_range<Rng>&, ...) noexcept;
		};

		template< typename Rng >
		struct sub_range : range_adaptor< sub_range<Rng>, Rng > {
			static_assert(
				!tc::is_view<std::remove_reference_t<Rng>>::value || std::is_same< Rng, view_by_value_t<Rng> >::value,
				"sub_range must hold views by value."
			);
			static_assert(
				tc::is_view<std::remove_reference_t<Rng>>::value || std::is_reference<Rng>::value,
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
			struct test_conversion_to_index : std::is_constructible< // TODO: inline (VS2017 RC4: "C1202 recursive type or function dependency context too complex")
				index,
				decltype( whole_range_sub_range_helper<Rng>::begin_index(
					std::declval<this_type&>(),
					std::declval<RngOther>()
				) )>
			{};

		public:
			// default ctor (for deferred initialization)
			sub_range() noexcept
			{}

			template <typename RngOther,
				std::enable_if_t<is_range_with_iterators<RngOther>::value>* = nullptr,
				std::enable_if_t<test_conversion_to_index<RngOther>::value>* = nullptr
			>
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

			template< typename Func > auto operator()(Func func) /* no & */ MAYTHROW {
				return [&]() MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, this->dereference_index(this->begin_index()))), INTEGRAL_CONSTANT(tc::continue_)> {
					for (index i = this->begin_index(); !this->at_end_index(i); this->increment_index(i)) {
						RETURN_IF_BREAK(tc::continue_if_not_break(func, this->dereference_index(i)));
					}
					return INTEGRAL_CONSTANT(tc::continue_)();
				}();
			}

			template< typename Func > auto operator()(Func func) const /* no & */ MAYTHROW {
				return [&]() MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, this->dereference_index(this->begin_index()))), INTEGRAL_CONSTANT(tc::continue_)> {
					for (index i = this->begin_index(); !this->at_end_index(i); this->increment_index(i)) {
						RETURN_IF_BREAK(tc::continue_if_not_break(func, this->dereference_index(i)));
					}
					return INTEGRAL_CONSTANT(tc::continue_)();
				}();
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

			// boost::range_iterator<sub_range>::type is the same type as the base range iterator:
			using iterator = typename boost::range_iterator< std::remove_reference_t< typename reference_or_value< Rng >::reference > >::type;
			using const_iterator = typename boost::range_iterator< std::remove_reference_t< typename reference_or_value< Rng >::const_reference > >::type;

			const_iterator make_iterator( index idx ) const & noexcept {
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
			auto it=boost::begin(rng);
#ifdef _CHECKS
			auto const itBound=boost::end(rng);
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
			_ASSERT(0<=n);
			_ASSERT(n<=tc::size_bounded(rng, n));
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
			auto it=boost::end(rng);
#ifdef _CHECKS
			auto const itBound=boost::begin(rng);
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
			_ASSERT(n<=tc::size_bounded(rng, n));
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
	linear_end_prev(
		Rng&& rng
	) noexcept {
		static_assert(
			!std::is_convertible<typename boost::range_traversal<std::remove_reference_t<Rng>>::type, boost::iterators::bidirectional_traversal_tag>::value,
			"Use end_prev for bidirectional ranges"
		);
		auto it = boost::begin(rng);
		auto const itEnd = boost::end(rng);
		_ASSERT( itEnd!=it );
		auto itNext = it;
		while (itEnd!=++itNext) {
			it = itNext;
		}
		return it;
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
	only_element(
		Rng&& rng
	) noexcept {
		auto it=boost::begin(rng);
#ifdef _CHECKS
		auto const itEnd=boost::end(rng);
#endif
		_ASSERT(it!=itEnd);
		_ASSERT(boost::next(it)==itEnd);
		return it;
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
	#define tc_only(rng) (*tc::only_element(rng))
	#define tc_back(rng) (*tc::end_prev(rng))
	#define tc_at(rng, i) (*tc::begin_next_not_end(rng,(i)))
	#define tc_linear_at(rng, i) (*tc::linear_begin_next_not_end(rng,(i)))
	#define tc_linear_back(rng) (*tc::linear_end_prev(rng))
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

	namespace return_xxx_adl_barrier {
		/////////////////////////////////////
		// no return

		template< typename Rng >
		struct return_void final {
			using type = void;

			template<typename It>
			static type pack_border(It&&, Rng&&) noexcept {}
			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&&) noexcept {}
			template<typename Ref>
			static type pack_element(Rng&&, Ref&&) noexcept {}
			static type pack_no_element(Rng&& rng) noexcept {}
		};

		/////////////////////////////////////
		// controlling bound return

		// returning element

		template< typename Rng >
		struct return_element_before final {
			using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

			static type pack_border(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
				_ASSERT(it != boost::begin(rng));
				return boost::prior(it);
			}
		};

		template< typename Rng >
		struct return_element_before_or_null final {
			using type = element_t< typename boost::range_iterator< std::remove_reference_t<Rng> >::type >;

			static type pack_border(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
				return it != boost::begin(rng) ? static_cast<type>(boost::prior(it)) : type{}; // value initialization to initialize pointers to nullptr
			}
		};

		// returning bound

		template< typename Rng >
		struct return_border final {
			using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

			static type pack_border(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
				return it;
			}
		};

		template< typename Rng >
		struct return_border_index final {
			using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;

			static type pack_border(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
				return tc::verify_class<type>(it - boost::begin(rng));
			}
		};

		// returning range

		template< typename >
		struct return_take final {
			template <typename Rng>
			static auto pack_border(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept return_decltype_rvalue_by_ref(
				tc::take(std::forward<Rng>(rng), it)
			)
		};

		template< typename Rng >
		struct return_drop final {
			using type = decltype(tc::drop(std::declval<Rng>(), boost::begin(std::declval<Rng&>())));

			static type pack_border(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng) noexcept {
				return tc::drop(std::forward<Rng>(rng), it);
			}
		};

		/////////////////////////////////////
		// controlling element return

		template< typename Rng >
		struct return_bool {
			using type = bool;

			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&&) noexcept {
				return true;
			}
			template<typename Ref>
			static type pack_element(Rng&&, Ref&&) noexcept {
				return true;
			}
			template<typename It>
			static type pack_view(Rng&&, It&&, It&&) noexcept {
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
			using type = tc::range_value_t< Rng >;

			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Ref>
			static type pack_element(Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			static type pack_no_element(Rng&&) noexcept {
				_ASSERTFALSE;
				return tc::construct_default_or_terminate<type>();
			}
		};

		template< typename Rng >
		struct return_value_or_default final {
			using type = tc::range_value_t< Rng >;

			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Ref>
			static type pack_element(Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			static type pack_no_element(Rng&&) noexcept {
				return{};
			}
		};

		template< typename Rng >
		struct return_value_or_none final {
			using type = boost::optional< tc::range_value_t< Rng > >;

			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Ref>
			static type pack_element(Rng&&, Ref&& ref) noexcept {
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

		template< typename Rng >
		struct return_singleton_range final {
			using type = typename make_sub_range_result<Rng>::type;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
				return tc::slice(std::forward<Rng>(rng), it, boost::next(it));
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				// safe choice is empty because result may be empty
				return tc::slice(std::forward<Rng>(rng), boost::begin(rng), boost::begin(rng));
			}
		};

		// returning bound

		template< typename Rng >
		struct return_border_after final {
			using type = typename boost::range_iterator< std::remove_reference_t<Rng> >::type;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&&, Ref&&) noexcept {
				return boost::next(it);
			}
			static type pack_view(Rng&&, typename boost::range_iterator< std::remove_reference_t<Rng> >::type, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd) noexcept {
				return itEnd;
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
			static type pack_view(Rng&&, typename boost::range_iterator< std::remove_reference_t<Rng> >::type, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd) noexcept {
				return itEnd;
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
			static type pack_view(Rng&&, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin, typename boost::range_iterator< std::remove_reference_t<Rng> >::type) noexcept {
				return itBegin;
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin, typename boost::range_iterator< std::remove_reference_t<Rng> >::type) noexcept {
				return tc::take(std::forward<Rng>(rng), itBegin);
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin, typename boost::range_iterator< std::remove_reference_t<Rng> >::type) noexcept {
				return tc::take(std::forward<Rng>(rng), itBegin);
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin, typename boost::range_iterator< std::remove_reference_t<Rng> >::type) noexcept {
				return tc::take(std::forward<Rng>(rng), itBegin);
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd) noexcept {
				return tc::take(std::forward<Rng>(rng), itEnd);
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd) noexcept {
				return tc::take(std::forward<Rng>(rng), itEnd);
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd) noexcept {
				return tc::take(std::forward<Rng>(rng), itEnd);
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin, typename boost::range_iterator< std::remove_reference_t<Rng> >::type) noexcept {
				return tc::drop(std::forward<Rng>(rng), itBegin);
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin, typename boost::range_iterator< std::remove_reference_t<Rng> >::type) noexcept {
				return tc::drop(std::forward<Rng>(rng), itBegin);
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin, typename boost::range_iterator< std::remove_reference_t<Rng> >::type) noexcept {
				return tc::drop(std::forward<Rng>(rng), itBegin);
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd) noexcept {
				return tc::drop(std::forward<Rng>(rng), itEnd);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				// safe choice is empty because result may be empty
				return tc::drop(std::forward<Rng>(rng), boost::begin(rng));
			}
		};

		template< typename Rng >
		struct return_drop_after_or_empty final {
			using type = decltype(tc::drop( std::declval<Rng>(), boost::begin(std::declval<Rng&>()) ));

			template<typename Ref>
			static type pack_element(typename boost::range_iterator< std::remove_reference_t<Rng> >::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop( std::forward<Rng>(rng), boost::next(it) );
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd) noexcept {
				return tc::drop(std::forward<Rng>(rng), itEnd);
			}
			static type pack_no_element(Rng&& rng) noexcept {
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
			static type pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd) noexcept {
				return tc::drop(std::forward<Rng>(rng), itEnd);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::drop(std::forward<Rng>(rng), boost::begin(rng));
			}
		};

		template<typename>
		struct return_view {
			template<typename Rng>
			static auto pack_view(Rng&& rng, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itBegin, typename boost::range_iterator< std::remove_reference_t<Rng> >::type itEnd) noexcept {
				return tc::slice(std::forward<Rng>(rng), itBegin, itEnd);
			}

			template<typename Rng>
			static auto pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return tc::slice(std::forward<Rng>(rng), boost::begin(rng), boost::end(rng));
			}
		};
	} // namespace return_xxx_adl_barrier
	using return_xxx_adl_barrier::return_void;
	using return_xxx_adl_barrier::return_element_before;
	using return_xxx_adl_barrier::return_element_before_or_null;
	using return_xxx_adl_barrier::return_border;
	using return_xxx_adl_barrier::return_border_index;
	using return_xxx_adl_barrier::return_take;
	using return_xxx_adl_barrier::return_drop;
	using return_xxx_adl_barrier::return_bool;
	using return_xxx_adl_barrier::return_element;
	using return_xxx_adl_barrier::return_element_or_null;
	using return_xxx_adl_barrier::return_element_or_front;
	using return_xxx_adl_barrier::return_value;
	using return_xxx_adl_barrier::return_value_or_default;
	using return_xxx_adl_barrier::return_value_or_none;
	using return_xxx_adl_barrier::return_element_index;
	using return_xxx_adl_barrier::return_element_index_or_npos;
	using return_xxx_adl_barrier::return_element_index_or_size;
	using return_xxx_adl_barrier::return_singleton_range;
	using return_xxx_adl_barrier::return_border_after;
	using return_xxx_adl_barrier::return_border_after_or_begin;
	using return_xxx_adl_barrier::return_border_before_or_end;
	using return_xxx_adl_barrier::return_take_before;
	using return_xxx_adl_barrier::return_take_before_or_empty;
	using return_xxx_adl_barrier::return_take_before_or_all;
	using return_xxx_adl_barrier::return_take_after;
	using return_xxx_adl_barrier::return_take_after_or_empty;
	using return_xxx_adl_barrier::return_take_after_or_all;
	using return_xxx_adl_barrier::return_drop_before;
	using return_xxx_adl_barrier::return_drop_before_or_empty;
	using return_xxx_adl_barrier::return_drop_before_or_all;
	using return_xxx_adl_barrier::return_drop_after;
	using return_xxx_adl_barrier::return_drop_after_or_empty;
	using return_xxx_adl_barrier::return_drop_after_or_all;
	using return_xxx_adl_barrier::return_view;

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

	template< typename HashAlgorithm, typename T >
	void hash_append(HashAlgorithm& h, tc::ptr_range<T> const& rng) noexcept {
		tc::for_each( rng, [&](auto const& t) noexcept {
			tc::hash_append( h, t );
		});
		tc::hash_append( h, boost::implicit_cast<std::uint64_t>(tc::size(rng)) );
	}

	template<
		typename T1,
		typename T2
	>
	struct common_type_decayed<tc::ptr_range<T1>, tc::ptr_range<T2>> {
		static_assert(sizeof(T1) == sizeof(T2));
		using type = tc::ptr_range<std::remove_pointer_t<tc::common_type_t<T1*, T2*>>>;
	};

	template<
		typename T1,
		typename T2,
		std::size_t N1,
		std::size_t N2
	>
	struct common_reference<T1(&)[N1], T2(&)[N2]> {
		static_assert(!tc::is_char<T1>::value && !tc::is_char<T2>::value);
		static_assert(N1 != N2);
		using type = tc::common_reference_t<tc::ptr_range<T1>, tc::ptr_range<T2>>;
	};

	//--------------------------------------------------------------------------------------------------------------------------
	// as_blob
	// reinterprets a range of items as a range of bytes

	// We use unsigned char for uninterpreted memory.
	// - The type used must support aliasing, so the only candidates are char, signed char and unsigned char.
	// - char is bad because we interpret char as UTF-8 and char* as zero-terminated range.
	// - unsigned char is better than signed char because the binary representation of signs may vary between platforms.
	// - char is bad because it is either signed or unsigned, so it has the same problem.
	// - unsigned char is better than std::uint8_t because the latter must be 8 bit, but we mean the smallest addressable unit, which is char and may be larger (or smaller?) on other platforms.

	static_assert(!tc::is_range_with_iterators< void const* >::value);

	template<typename T, std::enable_if_t<tc::is_range_with_iterators< T >::value>* = nullptr>
	auto as_blob(T&& t) noexcept {
		auto const& rng=tc::as_pointers(std::forward<T>(t));
		static_assert( std::is_trivially_copyable< typename tc::range_value< std::remove_reference_t< decltype( rng ) > >::type >::value, "as_blob only works on std::is_trivially_copyable types" );
		using cv_value_type = std::remove_pointer_t<decltype(tc::ptr_begin(rng))>;
		return tc::make_iterator_range( 
			reinterpret_cast<same_cvref_t<unsigned char, cv_value_type>*>( tc::ptr_begin(rng) ),
			reinterpret_cast<same_cvref_t<unsigned char, cv_value_type>*>( tc::ptr_end(rng) )
		);
	}

	template<typename T>
	tc::ptr_range<T const> as_typed_range(tc::ptr_range<unsigned char const> rng) noexcept {
		static_assert( tc::is_decayed<T>::value );
		static_assert( std::is_trivially_copyable<T>::value );
		_ASSERT( 0==tc::size(rng)%sizeof(T) );
		return tc::make_iterator_range( 
			reinterpret_cast<T const*>(tc::ptr_begin(rng) ),
			reinterpret_cast<T const*>(tc::ptr_end(rng) )
		);
	}

	namespace typed_source_adl_barrier {
		template<typename Sink, typename Char>
		struct typed_source_sink_adapter final {
			typed_source_sink_adapter(Sink& sink) noexcept : m_sink(sink) {}

			struct stream_traits {
				using value_type = unsigned char;

				static void append(typed_source_sink_adapter& adapter, unsigned char byte) MAYTHROW {
					static_assert(sizeof(Char) == sizeof(unsigned char), "not yet implemented for wide Char");
					static_assert(std::is_trivially_copyable<Char>::value);
					adapter.m_sink << reinterpret_cast<Char>(byte); // MAYTHROW
				}

				template<typename Rng>
				static void append_range(typed_source_sink_adapter& adapter, Rng&& rngbyte) MAYTHROW {
					adapter.m_sink << tc::as_typed_range<Char>(std::forward<Rng>(rngbyte)); // MAYTHROW
				}
			};

			friend stream_traits tc_stream_traits(typed_source_sink_adapter&) noexcept;

		private:
			Sink& m_sink;
		};

		template<typename Char, typename Source>
		struct typed_source : tc::noncopyable {
			using native_value_type = Char;

			explicit typed_source(Source&& source) noexcept : m_source(tc::aggregate_tag(), std::forward<Source>(source)) {}

			template<typename Sink, typename TypedSource>
			friend std::enable_if_t<
				std::is_same<std::remove_reference_t<TypedSource>, typed_source<Char, Source>>::value,
				Sink&> operator<<(Sink& sink, TypedSource&& source) MAYTHROW {
				using ::operator<<;
				typed_source_sink_adapter<Sink, Char>(sink) << *std::forward<TypedSource>(source).m_source; // MAYTHROW
				return sink;
			}

		private:
			tc::reference_or_value<Source> m_source;
		};

		template<typename Char, typename Source>
		auto make_typed_source(Source&& source) noexcept {
			return typed_source<Char, Source>(std::forward<Source>(source));
		}
	}
	using typed_source_adl_barrier::typed_source;
	using typed_source_adl_barrier::make_typed_source;

	template<typename T, std::enable_if_t<!tc::is_range_with_iterators< T >::value>* = nullptr>
	auto as_blob(T&& t) noexcept {
		return as_blob( tc::make_singleton_range(/* no std::forward<T> */ t) );
	}

	namespace assert_no_overlap_impl {
		template< typename Lhs, typename Rhs>
		void assert_no_overlap(Lhs const& lhs, Rhs const& rhs) noexcept {
			_ASSERT(
				reinterpret_cast<std::size_t>(tc::ptr_end(lhs)) <= reinterpret_cast<std::size_t>(tc::ptr_begin(rhs)) ||
				reinterpret_cast<std::size_t>(tc::ptr_end(rhs)) <= reinterpret_cast<std::size_t>(tc::ptr_begin(lhs))
			);
		}
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<!has_ptr_begin<Lhs>::value || !has_ptr_begin<Rhs>::value>* = nullptr >
	void assert_no_overlap(Lhs const& lhs, Rhs const& rhs) noexcept {
		assert_no_overlap_impl::assert_no_overlap(tc::as_pointers(tc::make_singleton_range(lhs)), tc::as_pointers(tc::make_singleton_range(rhs)));
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<has_ptr_begin<Lhs>::value && has_ptr_begin<Rhs>::value>* = nullptr >
	void assert_no_overlap(Lhs const& lhs, Rhs const& rhs) noexcept {
		assert_no_overlap_impl::assert_no_overlap(tc::as_pointers(tc::make_singleton_range(lhs)), tc::as_pointers(tc::make_singleton_range(rhs)));
		assert_no_overlap_impl::assert_no_overlap(lhs, rhs);
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
