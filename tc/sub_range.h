
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"

#include "tc_move.h"
#include "trivial_functors.h"
#include "container_traits.h"
#include "meta.h"
#include "size.h"
#include "assign.h"

#include <boost/optional.hpp>
#include <boost/implicit_cast.hpp>

#include <type_traits>

namespace tc {
	//-------------------------------------------------------------------------------------------------------------------------
	// meta function to determine the correct type
	// collapse sub_range< sub_range< ... > > to single sub_range
	template< typename Rng, typename Enable >
	struct make_sub_range_result final {
		static_assert(!std::is_rvalue_reference<Rng>::value);
		using type = sub_range< Rng >;
	};

	namespace no_adl {
		template<typename Rng>
		struct sub_range_param final {};

		template<typename Rng>
		struct sub_range_param<tc::sub_range<Rng>> final {
			using type=Rng;
		};
	}

	template< typename Rng >
	struct make_sub_range_result< Rng, std::enable_if_t<tc::is_instance<tc::sub_range,std::remove_reference_t<Rng>>::value> > final {
		static_assert(!std::is_rvalue_reference<Rng>::value);
	private:
		using InnerRng = typename no_adl::sub_range_param<tc::remove_cvref_t<Rng>>::type;
	public:
		using type=typename make_sub_range_result<
			typename std::conditional_t<
				std::is_empty<tc::decay_t<InnerRng>>::value,
				boost::mpl::identity<tc::decay_t<InnerRng>>,
				std::conditional_t<
					std::is_reference<InnerRng>::value,
					boost::mpl::identity<InnerRng>,
					tc::same_cvref<InnerRng, Rng>
				>
			>::type
		>::type;
	};

	//-------------------------------------------------------------------------------------------------------------------------
	template<typename T>
	T* raw_ptr(T* t) noexcept { return t; } // overloaded e.g. for boost::interprocess::offset_ptr
	
	template<typename Rng, std::enable_if_t<std::is_pointer< typename boost::range_iterator<Rng>::type >::value>* = nullptr>
	auto ptr_begin(Rng&& rng) noexcept return_decltype(
		tc::begin(rng) // not std::forward<Rng>(rng) : there is no overload for tc::begin(Rng&&), rvalues bind to tc::begin(Rng const&)
	)
	template<typename Rng, std::enable_if_t<std::is_pointer< typename boost::range_iterator<Rng>::type >::value>* = nullptr>
	auto ptr_end(Rng&& rng) noexcept return_decltype(
		tc::end(rng) // not std::forward<Rng>(rng) : there is no overload for tc::end(Rng&&), rvalues bind to tc::end(Rng const&)
	)

	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator<Rng>::type >::value 
			&& !(tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value )
		>* = nullptr
	>
	auto ptr_begin(Rng&& rng) noexcept return_decltype(
		raw_ptr( rng.data() )
	)
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator<Rng>::type >::value 
			&& !(tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value )
		>* = nullptr
	>
	auto ptr_end(Rng&& rng) noexcept return_decltype(
		raw_ptr( rng.data() ) + rng.size()
	)
	
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator<Rng>::type >::value 
			&& tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value
		>* = nullptr
	>
	auto ptr_begin(Rng&& rng) noexcept return_decltype(
		tc::make_mutable_ptr(raw_ptr( rng.data() ))
	)
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator<Rng>::type >::value 
			&& tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value
		>* = nullptr
	>
	auto ptr_end(Rng&& rng) noexcept return_decltype(
		tc::make_mutable_ptr(raw_ptr( rng.data() )) + rng.size()
	)

	TC_HAS_EXPR(ptr_begin, (T), tc::ptr_begin(std::declval<T>()))
	
	//-------------------------------------------------------------------------------------------------------------------------

	namespace sub_range_adl {
		template< typename Rng >
		struct whole_range_sub_range_helper_base {
			template<typename Rhs>
			constexpr static auto base_range(Rhs&& rhs) noexcept
				return_decltype_rvalue_by_ref( std::forward<Rhs>(rhs) )
			template<typename Rhs>
			constexpr static auto begin_index(sub_range<Rng>& lhs, Rhs&&) noexcept
				return_decltype( tc::base_cast<range_adaptor< sub_range<Rng>, Rng > >(lhs).STATIC_VIRTUAL_METHOD_NAME(begin_index)() )
			template<typename Rhs>
			constexpr static auto end_index(sub_range<Rng>& lhs, Rhs&&) noexcept
				return_decltype( tc::base_cast<range_adaptor< sub_range<Rng>, Rng > >(lhs).STATIC_VIRTUAL_METHOD_NAME(end_index)() )
		};

		template< typename It >
		struct whole_range_sub_range_helper_base<iterator_base<It>> {
			template<typename Rhs>
			constexpr static auto base_range(Rhs &&) noexcept
				return_decltype( iterator_base<It>() )
			template<typename Rhs>
			constexpr static auto begin_index(sub_range<iterator_base<It>>&, Rhs&& rng) noexcept
				return_decltype_rvalue_by_val( tc::iterator2index(tc::begin(rng)) )
			template<typename Rhs>
			constexpr static auto end_index(sub_range<iterator_base<It>>&, Rhs&& rng) noexcept
				return_decltype_rvalue_by_val( tc::iterator2index(tc::end(rng)) )
		};

		template< typename T >
		struct whole_range_sub_range_helper_base<iterator_base<T*>> {
			template<typename Rhs>
			constexpr static auto base_range(Rhs &&) noexcept
				return_decltype( iterator_base<T*>() )
			template<typename Rhs>
			constexpr static auto begin_index(sub_range<iterator_base<T*>>&, Rhs&& rng) noexcept
				return_decltype( tc::ptr_begin(rng) )
			template<typename Rhs>
			constexpr static auto end_index(sub_range<iterator_base<T*>>&, Rhs&& rng) noexcept
				return_decltype( tc::ptr_end(rng) )
		};

		template< typename Rng >
		struct whole_range_sub_range_helper final : whole_range_sub_range_helper_base<Rng> {
			using whole_range_sub_range_helper_base<Rng>::base_range;
			using whole_range_sub_range_helper_base<Rng>::begin_index;
			using whole_range_sub_range_helper_base<Rng>::end_index;

			template<typename Rhs>
			constexpr static auto base_range(sub_range<Rhs> const&& rhs) noexcept
				return_decltype_rvalue_by_ref(whole_range_sub_range_helper<Rng>::base_range( std::move(rhs).base_range()))
			template<typename Rhs>
			constexpr static auto begin_index(sub_range<Rng>&, sub_range<Rhs> const&& rhs) noexcept
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			constexpr static auto end_index(sub_range<Rng>&, sub_range<Rhs> const&& rhs) noexcept
				return_decltype( rhs.end_index() )

			template<typename Rhs>
			constexpr static auto base_range(sub_range<Rhs>&& rhs) noexcept
				return_decltype_rvalue_by_ref( whole_range_sub_range_helper<Rng>::base_range( std::move(rhs).base_range() ) )
			template<typename Rhs>
			constexpr static auto begin_index(sub_range<Rng>&, sub_range<Rhs>&& rhs) noexcept
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			constexpr static auto end_index(sub_range<Rng>&, sub_range<Rhs>&& rhs) noexcept
				return_decltype( rhs.end_index() )

			template<typename Rhs>
			constexpr static auto base_range(sub_range<Rhs> const& rhs) noexcept
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( rhs.base_range() ) )
			template<typename Rhs>
			constexpr static auto begin_index(sub_range<Rng>&, sub_range<Rhs> const& rhs) noexcept
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			constexpr static auto end_index(sub_range<Rng>&, sub_range<Rhs> const& rhs) noexcept
				return_decltype( rhs.end_index() )

			template<typename Rhs>
			constexpr static auto base_range(sub_range<Rhs>& rhs) noexcept
				return_decltype( whole_range_sub_range_helper<Rng>::base_range( rhs.base_range() ) )
			template<typename Rhs>
			constexpr static auto begin_index(sub_range<Rng>&, sub_range<Rhs>& rhs) noexcept
				return_decltype( rhs.begin_index() )
			template<typename Rhs>
			constexpr static auto end_index(sub_range<Rng>&, sub_range<Rhs>& rhs) noexcept
				return_decltype( rhs.end_index() )

			// Fallback for test_conversion_to_index below
			struct incompatible_index final {};
			static incompatible_index begin_index(sub_range<Rng>&, ...) noexcept;
		};

		template< typename Rng >
		struct sub_range : range_adaptor< sub_range<Rng>, Rng > {
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
			constexpr sub_range() noexcept
			{}

			template <typename RngOther,
				std::enable_if_t<is_range_with_iterators<RngOther>::value>* = nullptr,
				std::enable_if_t<test_conversion_to_index<RngOther>::value>* = nullptr
			>
			constexpr sub_range( RngOther&& rng ) noexcept
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
			constexpr sub_range( sub_range const& rng) noexcept
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

			template<typename Rhs, typename Begin, typename End>
			constexpr explicit sub_range( Rhs&& rng, Begin&& begin, End&& end ) noexcept
			: base_(aggregate_tag(), whole_range_sub_range_helper<Rng>::base_range(
				std::forward<Rhs>(rng)
			))
			, m_idxBegin(tc::iterator2index(std::forward<Begin>(begin)))
			, m_idxEnd(tc::iterator2index(std::forward<End>(end)))
			{}

			template< typename Func > constexpr auto operator()(Func func) /* no & */ MAYTHROW {
				return [&]() MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, this->dereference_index(this->begin_index()))), INTEGRAL_CONSTANT(tc::continue_)> {
					for (index i = this->begin_index(); !this->at_end_index(i); this->increment_index(i)) {
						RETURN_IF_BREAK(tc::continue_if_not_break(func, this->dereference_index(i)));
					}
					return INTEGRAL_CONSTANT(tc::continue_)();
				}();
			}

			template< typename Func > constexpr auto operator()(Func func) const /* no & */ MAYTHROW {
				return [&]() MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, this->dereference_index(this->begin_index()))), INTEGRAL_CONSTANT(tc::continue_)> {
					for (index i = this->begin_index(); !this->at_end_index(i); this->increment_index(i)) {
						RETURN_IF_BREAK(tc::continue_if_not_break(func, this->dereference_index(i)));
					}
					return INTEGRAL_CONSTANT(tc::continue_)();
				}();
			}

			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> index {
				return m_idxBegin;
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> index {
				return m_idxEnd;
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(index const& idx) const& noexcept -> bool {
				return this->equal_index( idx, m_idxEnd );
			}

			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			// boost::range_iterator<sub_range>::type is the same type as the base range iterator:
			using iterator = typename boost::range_iterator< typename reference_or_value< Rng >::reference >::type;
			using const_iterator = typename boost::range_iterator< typename reference_or_value< Rng >::const_reference >::type;

			constexpr const_iterator make_iterator( index idx ) const& noexcept {
				return tc::make_iterator( *this->m_baserng, tc_move(idx));
			}

			constexpr const_iterator begin() const& noexcept {
				return make_iterator(this->begin_index());
			}

			constexpr const_iterator end() const& noexcept {
				return make_iterator(this->end_index());
			}

			constexpr iterator make_iterator( index idx ) & noexcept {
				return tc::make_iterator( *this->m_baserng, tc_move(idx));
			}

			constexpr iterator begin() & noexcept {
				return make_iterator(this->begin_index());
			}

			constexpr iterator end() & noexcept {
				return make_iterator(this->end_index());
			}

			template< typename It >
			constexpr void take_inplace( It&& it ) & noexcept {
				m_idxEnd=tc::iterator2index( std::forward<It>(it) );
			}

			template< typename It >
			constexpr void drop_inplace( It&& it ) & noexcept {
				m_idxBegin=tc::iterator2index( std::forward<It>(it) );
			}

			template< typename It >
			constexpr friend sub_range&& take( sub_range&& rng, It&& it ) noexcept {
				rng.take_inplace(std::forward<It>(it));
				return tc_move(rng);
			}

			template< typename It >
			constexpr friend sub_range&& drop( sub_range&& rng, It&& it ) noexcept {
				rng.drop_inplace(std::forward<It>(it));
				return tc_move(rng);
			}
		};
	}
	using sub_range_adl::sub_range;

	template< typename Rng >
	auto make_view(Rng&& rng) noexcept return_ctor(
		typename make_sub_range_result< Rng >::type,
		(std::forward<Rng>(rng))
	)

	DEFINE_FN(make_view);

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

	namespace no_adl {
		template< typename Cont, typename It, typename Enable=void >
		struct has_mem_fn_erase_from_begin final: std::false_type {};

		template< typename Cont, typename It >
		struct has_mem_fn_erase_from_begin<Cont, It, tc::void_t<decltype(std::declval<Cont&>().erase(tc::begin(std::declval<Cont&>()),std::declval<It&&>()))>> final: std::true_type {};

		template< typename Cont, typename It, typename Enable=void >
		struct has_mem_fn_erase_to_end final: std::false_type {};

		template< typename Cont, typename It >
		struct has_mem_fn_erase_to_end<Cont, It, tc::void_t<decltype(std::declval<Cont&>().erase(std::declval<It&&>(),tc::end(std::declval<Cont&>())))>> final: std::true_type {};

		template< typename Cont, typename It, typename Enable=void >
		struct has_mem_fn_take_inplace final: std::false_type {};

		template< typename Cont, typename It >
		struct has_mem_fn_take_inplace<Cont, It, tc::void_t<decltype(std::declval<Cont&>().take_inplace(std::declval<It&&>()))>> final: std::true_type {};

		template< typename Cont, typename It, typename Enable=void >
		struct has_mem_fn_drop_inplace final: std::false_type {};

		template< typename Cont, typename It >
		struct has_mem_fn_drop_inplace<Cont, It, tc::void_t<decltype(std::declval<Cont&>().drop_inplace(std::declval<It&&>()))>> final: std::true_type {};
	}

	template< typename Cont, typename It, std::enable_if_t<no_adl::has_mem_fn_erase_to_end<Cont,It>::value>* = nullptr >
	constexpr void take_inplace( Cont& cont, It&& it ) noexcept {
		cont.erase(std::forward<It>(it),tc::end(cont));
	}

	template< typename Cont, typename It, std::enable_if_t<no_adl::has_mem_fn_take_inplace<Cont,It>::value>* = nullptr >
	constexpr void take_inplace( Cont& cont, It&& it ) noexcept {
		cont.take_inplace(std::forward<It>(it));
	}

	template< typename C, typename T, typename A, typename It >
	std::basic_string<C,T,A> && take( std::basic_string<C,T,A>&& rng, It&& it ) noexcept {
		tc::take_inplace(rng,std::forward<It>(it));
		return tc_move(rng);
	}

	template< typename Rng, typename End >
	auto take(Rng&& rng, End&& end) noexcept return_ctor(
		typename make_sub_range_result< Rng >::type,
		(std::forward<Rng>(rng), tc::begin(rng), std::forward<End>(end))
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

	template< typename Cont, typename It, std::enable_if_t<no_adl::has_mem_fn_erase_from_begin<Cont,It>::value>* = nullptr >
	constexpr void drop_inplace( Cont & cont, It&& it ) noexcept {
		cont.erase(tc::begin(cont),std::forward<It>(it));
	}

	template< typename Cont, typename It, std::enable_if_t<no_adl::has_mem_fn_drop_inplace<Cont,It>::value>* = nullptr >
	constexpr void drop_inplace( Cont & cont, It&& it ) noexcept {
		cont.drop_inplace(std::forward<It>(it));
	}

	template< typename CharPtr, typename It, std::enable_if_t<tc::is_char_ptr<CharPtr>::value>* = nullptr>
	constexpr void drop_inplace( CharPtr& pch, It&& it ) noexcept {
		pch=std::forward<It>(it);
	}

	template< typename Rng, std::enable_if_t<!tc::is_char_ptr< Rng >::value>* = nullptr>
	typename make_sub_range_result< Rng >::type drop_impl(Rng&& rng,
		typename boost::range_iterator<Rng>::type itBegin
	) noexcept {
		return typename make_sub_range_result< Rng >::type( std::forward<Rng>(rng), itBegin, tc::end(rng) );
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

	namespace begin_next_detail {
		template< bool bLinear, typename Rng >
		auto begin_next(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::forward_traversal_tag
		) noexcept {
			_ASSERTDEBUG(0 <= n);
			_ASSERTNOTIFY(bLinear || n <= 2);
			auto it=tc::begin(rng);
#ifdef _CHECKS
			auto const itBound=tc::end(rng);
#endif
			while (0<n) {
				_ASSERT(it!=itBound);
				--n;
				++it;
			}
			return it;
		}

		template< bool /*bLinear*/, typename Rng >
		auto begin_next(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::random_access_traversal_tag
		) noexcept {
			_ASSERTDEBUG(0<=n && n<=tc::size_raw(rng));
			return tc::begin(rng)+n;
		}
	}

	template< typename Rng >
	auto begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return begin_next_detail::begin_next</*bLinear*/false>(std::forward<Rng>(rng), n, typename boost::range_traversal< std::remove_reference_t<Rng> >::type());
	}

	template< typename Rng >
	auto linear_begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return begin_next_detail::begin_next</*bLinear*/true>(std::forward<Rng>(rng), n, typename boost::range_traversal< std::remove_reference_t<Rng> >::type());
	}


	namespace end_prev_detail {
		template< typename Rng >
		auto end_prev(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::bidirectional_traversal_tag
		) noexcept {
			_ASSERT(0 <= n);
			_ASSERTNOTIFY(n <= 2);
			auto it=tc::end(rng);
#ifdef _CHECKS
			auto const itBound=tc::begin(rng);
#endif
			while (0<n) {
				_ASSERT(it!=itBound);
				--n;
				--it;
			}
			return it;
		}

		template< typename Rng >
		auto end_prev(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::random_access_traversal_tag
		) noexcept {
			_ASSERT(0 <= n && n<=tc::size_raw(rng));
			return tc::end(rng)-n;
		}
	}

	template< typename Rng >
	auto end_prev(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return end_prev_detail::end_prev(std::forward<Rng>(rng), n, typename boost::range_traversal< std::remove_reference_t<Rng> >::type());
	}

	template< typename Rng >
	auto linear_end_prev(
		Rng&& rng
	) noexcept {
		static_assert(
			!std::is_convertible<typename boost::range_traversal<std::remove_reference_t<Rng>>::type, boost::iterators::bidirectional_traversal_tag>::value,
			"Use end_prev for bidirectional ranges"
		);
		auto it = tc::begin(rng);
		auto const itEnd = tc::end(rng);
		_ASSERT( itEnd!=it );
		auto itNext = it;
		while (itEnd!=++itNext) {
			it = itNext;
		}
		return it;
	}

	template< typename Rng >
	auto begin_not_end(
		Rng&& rng
	) noexcept ->decltype(tc::begin(rng)) {
		auto it=tc::begin(rng);
		_ASSERT(tc::end(rng) != it);
		return it;
	}

	template< typename Rng >
	auto begin_next_not_end(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n = 1
	) noexcept {
		auto it=tc::begin_next(rng, n);
		_ASSERTDEBUG(tc::end(rng) != it);
		return it;
	}

	template< typename Rng >
	auto linear_begin_next_not_end(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n
	) noexcept {
		auto it=tc::linear_begin_next(rng, n);
		_ASSERT(tc::end(rng) != it);
		return it;
	}

	// Write as macros to keep temporary iterators alive.
	// By standard, the lifetime of a reference is limited to the lifetime of the iterator.
	#define tc_front(rng) (*tc::begin_not_end(rng))
	#define tc_only(rng) (*tc::only<tc::return_element>(rng))
	#define tc_back(rng) (*tc::end_prev(rng))
	#define tc_at(rng, i) (*tc::begin_next_not_end(rng,(i)))
	#define tc_linear_at(rng, i) (*tc::linear_begin_next_not_end(rng,(i)))
	#define tc_linear_back(rng) (*tc::linear_end_prev(rng))
	#define tc_reverse_at(rng, i) (*tc::end_prev((rng),(i)+1))

	////////////////////////////////////////////////////////////////////////////////////
	// take/drop_*_first/last_n

	template< typename Cont >
	void take_first_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) noexcept {
		tc::take_inplace(cont, tc::begin_next(cont,n));
	}

	template< typename Rng >
	auto take_first(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) noexcept return_decltype_rvalue_by_ref(
		tc::take(std::forward<Rng>(rng), tc::begin_next(rng,n))
	)

	template< typename Cont >
	void drop_first_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n) noexcept {
		tc::drop_inplace(cont, tc::begin_next(cont, n));
	}

	template< typename Cont, std::enable_if_t<!has_mem_fn_pop_front<Cont>::value>* = nullptr>
	void drop_first_inplace(Cont& cont) noexcept {
		tc::drop_inplace(cont, tc::begin_next(cont));
	}

	template< typename Cont, std::enable_if_t<has_mem_fn_pop_front<Cont>::value>* = nullptr>
	void drop_first_inplace(Cont& cont) noexcept {
		cont.pop_front();
	}

	template< typename Rng >
	auto drop_first(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) noexcept return_decltype(
		tc::drop(std::forward<Rng>(rng), tc::begin_next(rng, n))
	)

	template< typename Cont >
	void take_last_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) noexcept {
		tc::drop_inplace(cont, tc::end_prev(cont, n));
	}

	template< typename Rng >
	auto take_last(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) noexcept return_decltype(
		tc::drop(std::forward<Rng>(rng), tc::end_prev(rng, n))
	)

	template< typename Cont >
	void drop_last_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n) noexcept {
		tc::take_inplace(cont, tc::end_prev(cont, n));
	}

	template< typename Cont, std::enable_if_t<!has_mem_fn_pop_back<Cont>::value>* = nullptr>
	void drop_last_inplace(Cont& cont) noexcept {
		tc::take_inplace(cont, tc::end_prev(cont));
	}

	template< typename Cont, std::enable_if_t<has_mem_fn_pop_back<Cont>::value>* = nullptr>
	void drop_last_inplace(Cont& cont) noexcept {
		cont.pop_back();
	}

	template< typename Rng >
	auto drop_last(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) noexcept return_decltype(
		tc::take(std::forward<Rng>(rng), tc::end_prev(rng, n))
	)

	//-------------------------------------------------------------------------------------------------------------------------
	// truncate

	namespace advance_forward_bounded_detail {
		template< typename It, typename T >
		auto advance_forward_bounded_impl(
			It&& it,
			T const n,
			tc::remove_cvref_t<It> const& itBound,
			boost::single_pass_traversal_tag
		) noexcept {
			_ASSERT(0 <= n);
			T nCount = 0;
			while (nCount != n && it != itBound) {
				++nCount;
				++it;
			}
			return nCount;
		}

		template< typename It, typename T >
		auto advance_forward_bounded_impl(
			It&& it,
			T n,
			tc::remove_cvref_t<It> const& itBound,
			boost::iterators::random_access_traversal_tag
		) noexcept {
			_ASSERT(0 <= n);
			if (tc::assign_better(n, tc::make_size_proxy(itBound - it), tc::fn_less_equal())) {
				it = itBound;
			} else {
				it += n;
			}
			return n;
		}
	}

	template< typename It, typename T >
	T advance_forward_bounded(
		It&& it,
		T n,
		tc::remove_cvref_t<It> const& itBound
	) noexcept {
		return advance_forward_bounded_detail::advance_forward_bounded_impl(std::forward<It>(it), n, itBound, typename boost::iterator_traversal< tc::remove_cvref_t<It> >::type());
	}

	template< typename Cont >
	void truncate_inplace( Cont& rng, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
		auto it=tc::begin(rng);
		tc::advance_forward_bounded( it, n, tc::end(rng) );
		tc::take_inplace( rng, tc_move(it) );
	}

	template< typename Rng >
	auto truncate(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n) noexcept
	->decltype(tc::take( std::forward<Rng>(rng), tc::begin(rng) )) {
		auto it=tc::begin(rng);
		tc::advance_forward_bounded( it, n, tc::end(rng) );
		return tc::take( std::forward<Rng>(rng), tc_move(it) );
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// make iterator range

	// sub_range from iterator pair
	template< typename It >
	constexpr auto make_iterator_range_impl( It itBegin, It itEnd ) noexcept
		return_ctor( tc::sub_range<tc::iterator_base<It>>, ( tc::iterator_base<It>(), tc_move(itBegin), tc_move(itEnd) ) )

	// There is an other make_iterator_range_impl overload for range adaptor based iterarors in range_adaptor.h

	// make sure ADL lookup of index_iterator::make_iterator_range works
	template< typename ItBegin, typename ItEnd >
	constexpr auto make_iterator_range(ItBegin&& itBegin, ItEnd&& itEnd) noexcept
		return_decltype( make_iterator_range_impl( std::forward<ItBegin>(itBegin), std::forward<ItEnd>(itEnd) ) )

	//-------------------------------------------------------------------------------------------------------------------------
	// make empty range

	template< typename T >
	auto make_empty_range() noexcept
		return_decltype( make_iterator_range( boost::implicit_cast<T*>(nullptr), boost::implicit_cast<T*>(nullptr) ) )

	//-------------------------------------------------------------------------------------------------------------------------
	// make counted range

	template< typename It, typename Count >
	constexpr auto make_counted_range( It const& it, Count&& count ) noexcept
		return_decltype( make_iterator_range( it, it+std::forward<Count>(count) ) )

	template< typename T, std::enable_if_t<std::is_reference<T>::value>* =nullptr >
	decltype(auto) single(T&& t) noexcept {
		return tc::make_counted_range(std::addressof(t),1);
	}

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

	namespace no_adl {
		/////////////////////////////////////
		// no return

		template< typename Rng >
		struct return_void final {
			using type = void;
			static constexpr bool requires_iterator = false;

			template<typename It>
			static type pack_border(It&&, Rng&&) noexcept {}
			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&&) noexcept {}
			template<typename Ref>
			static type pack_element(Ref&&) noexcept {}
			static type pack_no_element(Rng&&) noexcept {}
			static type pack_no_element() noexcept {}
		};

		/////////////////////////////////////
		// controlling bound return

		// returning element

		template< typename Rng >
		struct return_element_before final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			static type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				_ASSERT(it != tc::begin(rng));
				return boost::prior(it);
			}
		};

		template< typename Rng >
		struct return_element_before_or_null final {
			using type = element_t< typename boost::range_iterator<Rng>::type >;
			static constexpr bool requires_iterator = true;

			static type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				if( it != tc::begin(rng) ) {
					return static_cast<type>(boost::prior(it));
				} else {
					return type{}; // value initialization to initialize pointers to nullptr
				}
			}
		};

		template< typename Rng >
		struct return_element_after final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			static type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				_ASSERT(it != tc::end(rng));
				return it;
			}
		};

		// returning bound

		template< typename Rng >
		struct return_border final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			static type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				return it;
			}
		};

		template< typename Rng >
		struct return_border_index final {
			using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;
			static constexpr bool requires_iterator = true;

			static type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				return tc::explicit_cast<type>(it - tc::begin(rng));
			}
		};

		// returning range

		template< typename >
		struct return_take final {
			static constexpr bool requires_iterator = true;

			template <typename Rng>
			static auto pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept return_decltype_rvalue_by_ref(
				tc::take(std::forward<Rng>(rng), it)
			)
		};

		template< typename Rng >
		struct return_drop final {
			using type = decltype(tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			static type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				return tc::drop(std::forward<Rng>(rng), it);
			}
		};

		/////////////////////////////////////
		// controlling element return

		template< typename Rng >
		struct return_bool {
			using type = bool;
			static constexpr bool requires_iterator = false;

			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&&) noexcept {
				return true;
			}
			template<typename Ref>
			static type pack_element(Ref&&) noexcept {
				return true;
			}
			template<typename It>
			static type pack_view(Rng&&, It&&, It&&) noexcept {
				return true;
			}
			static type pack_no_element(Rng&&) noexcept {
				return false;
			}
			static type pack_no_element() noexcept {
				return false;
			}
		};

		// returning element

		template< typename Rng >
		struct return_element final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return it;
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return tc::begin(rng);
			}
		};

		template< typename Rng >
		struct return_element_or_null final {
			using type = element_t< typename boost::range_iterator<Rng>::type >;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&&, Ref&&) noexcept {
				return static_cast<type>(it);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return type{}; // value initialization to initialize pointers to nullptr
			}
		};

		template< typename Rng >
		struct return_element_or_front final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return it;
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::begin(rng);
			}
		};

		template< typename Rng >
		struct return_value final {
			using type = tc::range_value_t< Rng >;
			static constexpr bool requires_iterator = false;

			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Ref>
			static type pack_element(Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			static type pack_no_element(Rng&&) noexcept {
				_ASSERTFALSE;
				return tc::construct_default_or_terminate<type>();
			}
			static type pack_no_element() noexcept {
				_ASSERTFALSE;
				return tc::construct_default_or_terminate<type>();
			}
		};

		template< typename Rng >
		struct return_value_or_default final {
			using type = tc::range_value_t< Rng >;
			static constexpr bool requires_iterator = false;

			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Ref>
			static type pack_element(Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			static type pack_no_element(Rng&&) noexcept {
				return{};
			}
			static type pack_no_element() noexcept {
				return{};
			}
		};

		template< typename Rng >
		struct return_value_or_none final {
			using type = boost::optional< tc::range_value_t< Rng > >;
			static constexpr bool requires_iterator = false;

			template<typename It, typename Ref>
			static type pack_element(It&&, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Ref>
			static type pack_element(Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			static type pack_no_element(Rng&&) noexcept {
				return boost::none;
			}
			static type pack_no_element() noexcept {
				return boost::none;
			}
		};

		template< typename Rng >
		struct return_element_index final {
			using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::explicit_cast<type>(it - tc::begin(rng));
			}
			static type pack_no_element(Rng&&) noexcept {
				_ASSERTFALSE;
				return type(0);
			}
		};

		template< typename Rng >
		struct return_element_index_or_none final {
			using type=boost::optional<tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >>;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return type(boost::in_place_init, it - tc::begin(rng));
			}
			static type pack_no_element(Rng&&) noexcept {
				return boost::none;
			}
		};

		template< typename Rng >
		struct return_element_index_or_npos final {
			// static_cast<int>(npos) must be -1. Anything else is error-prone. So use range_difference instead of range_size for now.
			// Alternatively, we could return a special type that casts npos to -1.
			using type = tc::size_proxy< typename boost::range_difference<Rng>::type >;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::explicit_cast<type>(it - tc::begin(rng));
			}
			static type pack_no_element(Rng&&) noexcept {
				return type(static_cast<typename boost::range_difference<Rng>::type>(-1));
			}
		};

		template< typename Rng >
		struct return_element_index_or_size final {
			using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::explicit_cast<type>(it - tc::begin(rng));
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::size(rng);
			}
		};

		template< typename Rng >
		struct return_singleton_range final {
			using type = typename make_sub_range_result<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::slice(std::forward<Rng>(rng), it, boost::next(it));
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				// safe choice is empty because result may be empty
				return tc::slice(std::forward<Rng>(rng), tc::begin(rng), tc::begin(rng));
			}
		};

		// returning bound

		template< typename Rng >
		struct return_border_after final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&&, Ref&&) noexcept {
				return boost::next(it);
			}
			static type pack_view(Rng&&, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return itEnd;
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return tc::end(rng);
			}
		};

		template< typename Rng >
		struct return_border_after_or_begin final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return boost::next(it);
			}
			static type pack_view(Rng&&, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return itEnd;
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::begin(rng);
			}
		};

		template< typename Rng >
		struct return_border_before_or_end final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&&, Ref&&) noexcept {
				return it;
			}
			static type pack_view(Rng&&, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return itBegin;
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::end(rng);
			}
		};

		// returning range

		template< typename Rng >
		struct return_take_before final {
			using type = typename make_sub_range_result<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), it);
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::take(std::forward<Rng>(rng), itBegin);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				// safe choice is empty because result may be empty
				return tc::take(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template< typename Rng >
		struct return_take_before_or_empty final {
			using type = typename make_sub_range_result<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), it);
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::take(std::forward<Rng>(rng), itBegin);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template< typename Rng >
		struct return_take_before_or_all final {
			using type = typename make_sub_range_result<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), it);
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::take(std::forward<Rng>(rng), itBegin);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::end(rng));
			}
		};

		template< typename Rng >
		struct return_take_after final {
			using type = typename make_sub_range_result<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), boost::next(it));
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::take(std::forward<Rng>(rng), itEnd);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				// safe choice is all because result is never empty
				return tc::take(std::forward<Rng>(rng), tc::end(rng));
			}
		};

		template< typename Rng >
		struct return_take_after_or_empty final {
			using type = typename make_sub_range_result<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), boost::next(it));
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::take(std::forward<Rng>(rng), itEnd);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template< typename Rng >
		struct return_take_after_or_all final {
			using type = typename make_sub_range_result<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), boost::next(it));
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::take(std::forward<Rng>(rng), itEnd);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::end(rng));
			}
		};

		template< typename Rng >
		struct return_drop_before final {
			using type = decltype(tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop(std::forward<Rng>(rng), it);
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::drop(std::forward<Rng>(rng), itBegin);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				// safe choice is all because result is never empty
				return tc::drop(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template< typename Rng >
		struct return_drop_before_or_empty final {
			using type = decltype(tc::drop( std::declval<Rng>(), tc::begin(std::declval<Rng&>()) ));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop( std::forward<Rng>(rng), it );
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::drop(std::forward<Rng>(rng), itBegin);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::drop( std::forward<Rng>(rng), tc::end(rng));
			}
		};

		template< typename Rng >
		struct return_drop_before_or_all final {
			using type = decltype(tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop(std::forward<Rng>(rng), it);
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::drop(std::forward<Rng>(rng), itBegin);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::drop(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template< typename Rng >
		struct return_drop_after final {
			using type = decltype(tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop(std::forward<Rng>(rng), boost::next(it));
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::drop(std::forward<Rng>(rng), itEnd);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				// safe choice is empty because result may be empty
				return tc::drop(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template< typename Rng >
		struct return_drop_after_or_empty final {
			using type = decltype(tc::drop( std::declval<Rng>(), tc::begin(std::declval<Rng&>()) ));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop( std::forward<Rng>(rng), boost::next(it) );
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::drop(std::forward<Rng>(rng), itEnd);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::drop(std::forward<Rng>(rng), tc::end(rng));
			}
		};

		template< typename Rng >
		struct return_drop_after_or_all final {
			using type = decltype(tc::drop( std::declval<Rng>(), tc::begin(std::declval<Rng&>()) ));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop( std::forward<Rng>(rng), boost::next(it) );
			}
			static type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::drop(std::forward<Rng>(rng), itEnd);
			}
			static type pack_no_element(Rng&& rng) noexcept {
				return tc::drop(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template<typename>
		struct return_view {
			static constexpr bool requires_iterator = true;

			template<typename Rng>
			static auto pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::slice(std::forward<Rng>(rng), itBegin, itEnd);
			}

			template<typename Rng>
			static auto pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return tc::slice(std::forward<Rng>(rng), tc::begin(rng), tc::end(rng));
			}
		};

		template<typename>
		struct return_view_or_empty {
			template<typename Rng>
			static auto pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::slice(std::forward<Rng>(rng), itBegin, itEnd);
			}

			template<typename Rng>
			static auto pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::begin(rng));
			}
		};
	} // namespace no_adl
	using no_adl::return_void;
	using no_adl::return_element_before;
	using no_adl::return_element_before_or_null;
	using no_adl::return_element_after;
	using no_adl::return_border;
	using no_adl::return_border_index;
	using no_adl::return_take;
	using no_adl::return_drop;
	using no_adl::return_bool;
	using no_adl::return_element;
	using no_adl::return_element_or_null;
	using no_adl::return_element_or_front;
	using no_adl::return_value;
	using no_adl::return_value_or_default;
	using no_adl::return_value_or_none;
	using no_adl::return_element_index;
	using no_adl::return_element_index_or_none;
	using no_adl::return_element_index_or_npos;
	using no_adl::return_element_index_or_size;
	using no_adl::return_singleton_range;
	using no_adl::return_border_after;
	using no_adl::return_border_after_or_begin;
	using no_adl::return_border_before_or_end;
	using no_adl::return_take_before;
	using no_adl::return_take_before_or_empty;
	using no_adl::return_take_before_or_all;
	using no_adl::return_take_after;
	using no_adl::return_take_after_or_empty;
	using no_adl::return_take_after_or_all;
	using no_adl::return_drop_before;
	using no_adl::return_drop_before_or_empty;
	using no_adl::return_drop_before_or_all;
	using no_adl::return_drop_after;
	using no_adl::return_drop_after_or_empty;
	using no_adl::return_drop_after_or_all;
	using no_adl::return_view;
	using no_adl::return_view_or_empty;

	//-------------------------------------------------------------------------------------------------------------------------
	// with_only/only

	template<typename Rng, typename Func>
	bool with_only(Rng&& rng, Func func) noexcept {
		bool bCalled=false;
		tc::for_each(std::forward<Rng>(rng), [&](auto&&... args) MAYTHROW {
			VERIFY(tc::change(bCalled, true)); // only a single element is allowed

			RETURNS_VOID( func(std::forward<decltype(args)>(args)...) ); // MAYTHROW

			return INTEGRAL_CONSTANT(tc::continue_)();
		});
		return bCalled;
	}

	namespace only_detail {
		template<template<typename> class RangeReturn, typename Rng, typename Func, std::enable_if_t<RangeReturn<Rng>::requires_iterator>* = nullptr>
		decltype(auto) only(Rng&& rng, Func func) MAYTHROW {
			auto const itEnd = tc::end(rng);
			auto const itBegin = tc::begin(rng);
			if(itEnd != itBegin && (itEnd == boost::next(itBegin) || tc::bool_cast(func()))) {
				return RangeReturn<Rng>::pack_element(itBegin, std::forward<Rng>(rng), *itBegin);
			} else {
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			}
		}

		template<template<typename> class RangeReturn, typename Rng, typename Func, std::enable_if_t<!RangeReturn<Rng>::requires_iterator>* = nullptr>
		typename RangeReturn<Rng>::type only(Rng&& rng, Func func) MAYTHROW {
			boost::optional<typename RangeReturn<Rng>::type> ot;
			auto const breakorcontinue = tc::for_each(std::forward<Rng>(rng), [&](auto&& t) MAYTHROW {
				if(!ot) {
					ot.emplace(RangeReturn<Rng>::pack_element(std::forward<decltype(t)>(t))); // MAYTHROW
					return tc::continue_;
				} else {
					return tc::break_;
				}
			});
			if(ot && (tc::continue_ == breakorcontinue || tc::bool_cast(func()))) {
				return *tc_move(ot);
			} else {
				return RangeReturn<Rng>::pack_no_element();
			}
		}
	}

	template<template<typename> class RangeReturn, typename Rng, typename Func = tc::never_called<INTEGRAL_CONSTANT(true)>>
	decltype(auto) only(Rng&& rng, Func&& func = Func()) MAYTHROW {
		return only_detail::only<RangeReturn>(std::forward<Rng>(rng), std::forward<Func>(func));
	}

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
	constexpr auto string_literal(T (&at)[N] ) noexcept return_decltype(
		tc::make_counted_range( std::addressof(at[0]), N-1 )
	)

	template< typename T >
	using ptr_range = sub_range < iterator_base<T*> >;

	namespace no_adl {
		template<typename T1, typename T2, typename Enable1=void, typename Enable2=void>
		struct common_type_decayed_ptr_range_base {};

		template<typename T1, typename T2>
		struct common_type_decayed_ptr_range_base<T1, T2, std::enable_if_t<sizeof(T1)==sizeof(T2)>, tc::void_t<tc::common_type_t<T1*, T2*>>> {
			using type = tc::ptr_range<std::remove_pointer_t<tc::common_type_t<T1*, T2*>>>;
		};

		template<typename T1, typename T2>
		struct common_type_decayed<tc::ptr_range<T1>, tc::ptr_range<T2>>: common_type_decayed_ptr_range_base<T1, T2> {};

		template<typename T1, typename T2, std::size_t N1, std::size_t N2, typename Enable=void>
		struct common_reference_array_base {};

		template<typename T1, typename T2, std::size_t N1, std::size_t N2>
		struct common_reference_array_base<T1, T2, N1, N2, std::enable_if_t<!tc::is_char<T1>::value && !tc::is_char<T2>::value && N1 != N2>>
		:	tc::common_reference<tc::ptr_range<T1>, tc::ptr_range<T2>> {
		};

		template<typename T1, typename T2, std::size_t N1, std::size_t N2>
		struct common_reference<T1(&)[N1], T2(&)[N2]>: common_reference_array_base<T1, T2, N1, N2> {};
	}

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
		auto_cref( rng, tc::as_pointers(std::forward<T>(t)) );
		static_assert( std::is_trivially_copyable< tc::range_value_t< std::remove_reference_t< decltype( rng ) > > >::value, "as_blob only works on std::is_trivially_copyable types" );
		using cv_value_type = std::remove_pointer_t<decltype(tc::ptr_begin(rng))>;
		return tc::make_iterator_range( 
			reinterpret_cast<same_cvref_t<unsigned char, cv_value_type>*>( tc::ptr_begin(rng) ),
			reinterpret_cast<same_cvref_t<unsigned char, cv_value_type>*>( tc::ptr_end(rng) )
		);
	}

	template<typename T, std::enable_if_t<!tc::is_range_with_iterators< T >::value>* = nullptr>
	auto as_blob(T&& t) noexcept {
		return as_blob( tc::single(/* no std::forward<T> */ t) );
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
		assert_no_overlap_impl::assert_no_overlap(tc::as_pointers(tc::single(lhs)), tc::as_pointers(tc::single(rhs)));
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<has_ptr_begin<Lhs>::value && has_ptr_begin<Rhs>::value>* = nullptr >
	void assert_no_overlap(Lhs const& lhs, Rhs const& rhs) noexcept {
		assert_no_overlap_impl::assert_no_overlap(tc::as_pointers(tc::single(lhs)), tc::as_pointers(tc::single(rhs)));
		assert_no_overlap_impl::assert_no_overlap(lhs, rhs);
	}

#ifdef _CHECKS
	namespace no_adl {
		template< typename Rng >
		struct SSinglePassRange {
			explicit SSinglePassRange(Rng&& rng) noexcept
				: m_rng(tc::aggregate_tag{}, std::forward<Rng>(rng))
			{}

			tc::reference_or_value<Rng&&> m_rng;
			bool mutable m_bFirstPass = true;

			template< typename Func >
			decltype(auto) operator()(Func func) && MAYTHROW {
				_ASSERT( tc::change(m_bFirstPass, false) );
				return tc::for_each(std::forward<Rng>(*m_rng), tc_move(func));
			}
		};
	}

	// Note: The result of assert_single_pass has the same lifetime as its argument
	template< typename Rng >
	auto assert_single_pass(Rng&& rng) noexcept {
		return no_adl::SSinglePassRange<Rng>(std::forward<Rng>(rng));
	}
#else
	template< typename Rng >
	decltype(auto) assert_single_pass(Rng&& rng) noexcept {
		return std::forward<Rng>(rng);
	}
#endif
}
