
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
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

#include <optional>
#include <type_traits>

namespace tc {
	namespace no_adl {
		//-------------------------------------------------------------------------------------------------------------------------
		// meta function to determine the correct type
		// collapse subrange< subrange< ... > > to single subrange
		template< typename Rng, typename Enable >
		struct make_subrange_result final {
			static_assert(!std::is_rvalue_reference<Rng>::value);
			using type = subrange< Rng >;
		};

		template<typename Rng>
		struct subrange_param final {};

		template<typename Rng>
		struct subrange_param<tc::subrange<Rng>> final {
			using type=Rng;
		};

		template< typename Rng >
		struct make_subrange_result< Rng, std::enable_if_t<tc::is_instance<tc::subrange,std::remove_reference_t<Rng>>::value> > final {
			static_assert(!std::is_rvalue_reference<Rng>::value);
		private:
			using InnerRng = typename no_adl::subrange_param<tc::remove_cvref_t<Rng>>::type;
		public:
			using type=make_subrange_result_t<
				typename std::conditional_t<
					std::is_empty<tc::decay_t<InnerRng>>::value,
					boost::mpl::identity<tc::decay_t<InnerRng>>,
					std::conditional_t<
						std::is_reference<InnerRng>::value,
						boost::mpl::identity<InnerRng>,
						tc::same_cvref<InnerRng, Rng>
					>
				>::type
			>;
		};
	}
	template<typename Rng>
	using subrange_param_t = typename no_adl::subrange_param<Rng>::type;

	//-------------------------------------------------------------------------------------------------------------------------
#ifdef _MSC_VER
#define MSVC_CONSTEXPR_WORKAROUND
#else
#define MSVC_CONSTEXPR_WORKAROUND constexpr
#endif
	
	template<typename T>
	MSVC_CONSTEXPR_WORKAROUND T* raw_ptr(T* t) noexcept { return t; } // overloaded e.g. for boost::interprocess::offset_ptr
	
	template<typename Rng, std::enable_if_t<std::is_pointer< typename boost::range_iterator<Rng>::type >::value>* = nullptr>
	MSVC_CONSTEXPR_WORKAROUND auto ptr_begin(Rng&& rng) return_decltype_NOEXCEPT(
		tc::begin(rng) // not std::forward<Rng>(rng) : there is no overload for tc::begin(Rng&&), rvalues bind to tc::begin(Rng const&)
	)
	template<typename Rng, std::enable_if_t<std::is_pointer< typename boost::range_iterator<Rng>::type >::value>* = nullptr>
	MSVC_CONSTEXPR_WORKAROUND auto ptr_end(Rng&& rng) return_decltype_NOEXCEPT(
		tc::end(rng) // not std::forward<Rng>(rng) : there is no overload for tc::end(Rng&&), rvalues bind to tc::end(Rng const&)
	)

	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator<Rng>::type >::value 
			&& !(tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value )
		>* = nullptr
	>
	MSVC_CONSTEXPR_WORKAROUND auto ptr_begin(Rng&& rng) return_decltype_noexcept(
		raw_ptr( rng.data() )
	)
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator<Rng>::type >::value 
			&& !(tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value )
		>* = nullptr
	>
	MSVC_CONSTEXPR_WORKAROUND auto ptr_end(Rng&& rng) return_decltype_noexcept(
		raw_ptr( rng.data() ) + rng.size()
	)
	
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator<Rng>::type >::value 
			&& tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value
		>* = nullptr
	>
	MSVC_CONSTEXPR_WORKAROUND auto ptr_begin(Rng&& rng) return_decltype_noexcept(
		tc::make_mutable_ptr(raw_ptr( rng.data() ))
	)
	template<typename Rng, 
		std::enable_if_t<
			!std::is_pointer< typename boost::range_iterator<Rng>::type >::value 
			&& tc::is_instance<std::basic_string,std::remove_reference_t<Rng>>::value && !std::is_const<std::remove_reference_t<Rng> >::value
		>* = nullptr
	>
	MSVC_CONSTEXPR_WORKAROUND auto ptr_end(Rng&& rng) return_decltype_noexcept(
		tc::make_mutable_ptr(raw_ptr( rng.data() )) + rng.size()
	)

	TC_HAS_EXPR(ptr_begin, (T), tc::ptr_begin(std::declval<T>()))
	
	//-------------------------------------------------------------------------------------------------------------------------

	namespace no_adl {
		template< typename Rng >
		struct whole_range_subrange_helper final {
			template<typename Rhs>
			static constexpr decltype(auto) begin_index(subrange<Rng>& lhs, Rhs&&) noexcept {
				return lhs.template begin_index<range_adaptor< subrange<Rng>, Rng >>();
			}

			template<typename Rhs>
			static constexpr decltype(auto) end_index(subrange<Rng>& lhs, Rhs&&) noexcept {
				return lhs.template end_index<range_adaptor< subrange<Rng>, Rng >>();
			}
		};

		template< typename It >
		struct whole_range_subrange_helper<iterator_base<It>> final {
			template<typename Rhs>
			static constexpr auto begin_index(subrange<iterator_base<It>>&, Rhs&& rng)
				return_decltype_xvalue_by_val_noexcept(tc::iterator2index(tc::begin(rng)))

			template<typename Rhs>
			static constexpr auto end_index(subrange<iterator_base<It>>&, Rhs&& rng)
				return_decltype_xvalue_by_val_noexcept(tc::iterator2index(tc::end(rng)))
		};

		template< typename T >
		struct whole_range_subrange_helper<iterator_base<T*>> final {
			template<typename Rhs>
			static constexpr auto begin_index(subrange<iterator_base<T*>>&, Rhs&& rng)
				return_decltype_noexcept(tc::ptr_begin(rng))

			template<typename Rhs>
			static constexpr auto end_index(subrange<iterator_base<T*>>&, Rhs&& rng)
				return_decltype_noexcept(tc::ptr_end(rng))
		};

		template<typename Rng>
		struct subrange_base_range_helper final {
			template<typename Rhs, std::enable_if_t<!tc::is_instance<tc::subrange, std::remove_reference_t<Rhs>>::value>* = nullptr>
			static constexpr auto base_range(Rhs&& rhs)
				return_decltype_xvalue_by_ref_noexcept(std::forward<Rhs>(rhs))

			template<typename Rhs, std::enable_if_t<tc::is_instance<tc::subrange, std::remove_reference_t<Rhs>>::value>* = nullptr>
			static constexpr decltype(auto) base_range(Rhs&& rhs) noexcept {
				return std::forward<Rhs>(rhs).base_range();
			}
		};

		template<typename It>
		struct subrange_base_range_helper<iterator_base<It>> final {
			template<typename Rhs>
			static constexpr auto base_range(Rhs&&)
				return_decltype_noexcept(iterator_base<It>())
		};
	}

	namespace subrange_adl {
		template< typename Rng >
		struct subrange : range_adaptor< subrange<Rng>, Rng > {
		private:
			using this_type = subrange;
			using base_ = range_adaptor< subrange<Rng>, Rng >;

		public:
			using typename base_::index;

		private:
			index m_idxBegin;
			index m_idxEnd;

		public:
			// default ctor (for deferred initialization)
			constexpr subrange() = default;

			// ctor from whole range
			template<typename Rhs,
				std::enable_if_t<
					is_range_with_iterators<Rhs>::value &&
					!tc::is_instance<tc::subrange, std::remove_reference_t<Rhs>>::value
				>* = nullptr,
				std::enable_if_t<
					tc::is_safely_constructible<Rng, decltype(no_adl::subrange_base_range_helper<Rng>::base_range(std::declval<Rhs>()))>::value &&
					std::is_constructible< // index may not be tc::is_safely_constructible because of slicing (e.g. std::vector::const_iterator/iterator on Windows, or tc::element_t)
						index,
						decltype(no_adl::whole_range_subrange_helper<Rng>::begin_index(
							std::declval<this_type&>(),
							std::declval<Rhs>()
						))
					>::value
				>* = nullptr
			>
			constexpr subrange(Rhs&& rng) noexcept
				: base_(aggregate_tag, no_adl::subrange_base_range_helper<Rng>::base_range(std::forward<Rhs>(rng)))
				, m_idxBegin(no_adl::whole_range_subrange_helper<Rng>::begin_index(
					*this,
					std::forward<Rhs>(rng)
				))
				, m_idxEnd(no_adl::whole_range_subrange_helper<Rng>::end_index(
					*this,
					std::forward<Rhs>(rng)
				))
			{}
		private:
			DEFINE_NESTED_TAG_TYPE(index_tag)

			// ctor from rng/begin/end
			template<typename Rhs, typename IndexBegin, typename IndexEnd>
			constexpr subrange(index_tag_t, Rhs&& rng, IndexBegin&& idxBegin, IndexEnd&& idxEnd) noexcept
				: base_(aggregate_tag, no_adl::subrange_base_range_helper<Rng>::base_range(std::forward<Rhs>(rng)))
				, m_idxBegin(std::forward<IndexBegin>(idxBegin))
				, m_idxEnd(std::forward<IndexEnd>(idxEnd))
			{
				static_assert(
					std::is_lvalue_reference<Rng>::value ||
					(
						!std::is_lvalue_reference<decltype(no_adl::subrange_base_range_helper<Rng>::base_range(std::declval<Rhs>()))>::value &&
						!std::is_const<std::remove_reference_t<decltype(no_adl::subrange_base_range_helper<Rng>::base_range(std::declval<Rhs>()))>>::value &&
						tc::is_index_valid_for_move_constructed_range<Rng>::value
					)
				);
			}

		public:
			// user-defined copy ctor to disable implicit copy/move ctor, with same semantics as templated ctor
			constexpr subrange(subrange const& rng) noexcept
				: subrange(index_tag, rng, rng.begin_index(), rng.end_index())
			{}

			template<typename Rhs, std::enable_if_t<
				tc::is_instance<tc::subrange, std::remove_reference_t<Rhs>>::value &&
				tc::is_safely_constructible<Rng, decltype(no_adl::subrange_base_range_helper<Rng>::base_range(std::declval<Rhs>()))>::value &&
				std::is_constructible<index, decltype(std::declval<Rhs&>().begin_index())>::value
			>* = nullptr>
			constexpr subrange(Rhs&& rng) noexcept
				: subrange(index_tag, std::forward<Rhs>(rng), rng.begin_index(), rng.end_index()) // m_idxBegin/End will not be moved
			{}

			template<typename Rhs, typename Begin, typename End, std::enable_if_t<
				tc::is_safely_constructible<Rng, decltype(no_adl::subrange_base_range_helper<Rng>::base_range(std::declval<Rhs>()))>::value &&
				std::is_constructible<index, decltype(tc::iterator2index(std::declval<Begin>()))>::value &&
				std::is_constructible<index, decltype(tc::iterator2index(std::declval<End>()))>::value
			>* = nullptr>
			constexpr explicit subrange(Rhs&& rng, Begin&& begin, End&& end) noexcept
				: subrange(index_tag, std::forward<Rhs>(rng), tc::iterator2index(std::forward<Begin>(begin)), tc::iterator2index(std::forward<End>(end)))
			{}

			template< typename Func >
			constexpr auto operator()(Func func) /* no & */ MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<tc::range_reference_t<Rng>>())), INTEGRAL_CONSTANT(tc::continue_)> {
				for (index i = this->begin_index(); !this->at_end_index(i); this->increment_index(i)) {
					RETURN_IF_BREAK(tc::continue_if_not_break(func, this->dereference_index(i)));
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}

			template< typename Func >
			constexpr auto operator()(Func func) const /* no & */ MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<tc::range_reference_t<Rng>>())), INTEGRAL_CONSTANT(tc::continue_)> {
				for (index i = this->begin_index(); !this->at_end_index(i); this->increment_index(i)) {
					RETURN_IF_BREAK(tc::continue_if_not_break(func, this->dereference_index(i)));
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
			template<typename This, typename Func>
			constexpr static auto enumerate_reversed(This&& rngThis, Func&& func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<tc::range_reference_t<Rng>>())), INTEGRAL_CONSTANT(tc::continue_)> {
				index idxEnd = rngThis.end_index();
				while (!rngThis.equal_index(idxEnd, rngThis.begin_index())) {
					rngThis.decrement_index(idxEnd);
					RETURN_IF_BREAK(tc::continue_if_not_break(func, std::forward<This>(rngThis).dereference_index(idxEnd)));
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> index {
				return m_idxBegin;
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> index {
				return m_idxEnd;
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(index const& idx) const& noexcept -> bool {
				return this->equal_index( idx, m_idxEnd );
			}
		public:
			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			// boost::range_iterator<subrange>::type is the same type as the base range iterator:
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
			constexpr friend subrange&& take( subrange&& rng, It&& it ) noexcept {
				rng.take_inplace(std::forward<It>(it));
				return tc_move(rng);
			}

			template< typename It >
			constexpr friend subrange&& drop( subrange&& rng, It&& it ) noexcept {
				rng.drop_inplace(std::forward<It>(it));
				return tc_move(rng);
			}
		};
	}

	namespace no_adl {
		template<typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::subrange<Rng>, std::enable_if_t<std::is_lvalue_reference<Rng>::value>> : std::true_type {};

		template<typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::subrange<Rng>, std::enable_if_t<!std::is_reference<Rng>::value>> : tc::is_index_valid_for_move_constructed_range<Rng> {};
	}

	template< typename Rng >
	[[nodiscard]] auto make_view(Rng&& rng) return_ctor_noexcept(
		tc::make_subrange_result_t< Rng >,
		(std::forward<Rng>(rng))
	)

	DEFINE_FN(make_view);

	//-------------------------------------------------------------------------------------------------------------------------
	// slice

	// slice from range + iterator pair
	// slice from range + difference
	template< typename Rng, typename Begin, typename End >
	[[nodiscard]] auto slice(Rng&& rng, Begin&& begin, End&& end) return_ctor_noexcept(
		tc::make_subrange_result_t< Rng >,
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
	[[nodiscard]] std::basic_string<C,T,A> && take( std::basic_string<C,T,A>&& rng, It&& it ) noexcept {
		tc::take_inplace(rng,std::forward<It>(it));
		return tc_move(rng);
	}

	template< typename Rng, typename End >
	[[nodiscard]] auto take(Rng&& rng, End&& end) return_ctor_NOEXCEPT( // boost::iterator_range doesn't have a noexcept constructor
		tc::make_subrange_result_t< Rng >,
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

	template< typename Rng, typename It, std::enable_if_t<!tc::is_char_ptr< Rng >::value>* = nullptr>
	constexpr tc::make_subrange_result_t< Rng > drop_impl( Rng&& rng, It&& itBegin ) noexcept {
		return tc::make_subrange_result_t< Rng >( std::forward<Rng>(rng), std::forward<It>(itBegin), tc::end(rng) );
	}

	// C strings have efficient in-place drop
	template< typename CharPtr, typename It, std::enable_if_t<tc::is_char_ptr< CharPtr >::value>* = nullptr>
	constexpr std::decay_t<CharPtr> drop_impl( CharPtr&& pch, It&& it ) noexcept {
		std::decay_t<CharPtr> pchCopy=std::forward<CharPtr>(pch);
		tc::drop_inplace( pchCopy, std::forward<It>(it) );
		return pchCopy;
	}

	template< typename Rng, typename It >
	[[nodiscard]] constexpr auto drop(Rng&& rng, It&& it) return_decltype_NOEXCEPT(
		drop_impl( std::forward<Rng>(rng), std::forward<It>(it) )
	)

	////////////////////////////////
	// front/back on ranges

	namespace begin_next_detail {
		template< bool bLinear, typename Rng >
		constexpr auto begin_next(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::forward_traversal_tag
		) noexcept {
			_ASSERTDEBUG(0 <= n);
			if constexpr(!bLinear) {
				_ASSERTENOTIFY(n <= 2);
			}
			auto it=tc::begin(rng);
#ifdef _CHECKS
			auto const itBound=tc::end(rng);
#endif
			while (0<n) {
				_ASSERTE(it!=itBound);
				--n;
				++it;
			}
			return it;
		}

		template< bool /*bLinear*/, typename Rng >
		constexpr auto begin_next(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::random_access_traversal_tag
		) noexcept {
			_ASSERTDEBUG(0<=n);
			_ASSERTDEBUG(n<=tc::size_raw(rng));
			return tc::begin(rng)+n;
		}
	}

	template< typename Rng >
	[[nodiscard]] constexpr auto begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return begin_next_detail::begin_next</*bLinear*/false>(std::forward<Rng>(rng), n, typename boost::range_traversal<Rng>::type());
	}

	template< typename Rng >
	[[nodiscard]] auto linear_begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return begin_next_detail::begin_next</*bLinear*/true>(std::forward<Rng>(rng), n, typename boost::range_traversal<Rng>::type());
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
				_ASSERTDEBUG(it!=itBound);
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
			_ASSERTDEBUG(0 <= n && n<=tc::size_raw(rng));
			return tc::end(rng)-n;
		}
	}

	template< typename Rng >
	[[nodiscard]] auto end_prev(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return end_prev_detail::end_prev(std::forward<Rng>(rng), n, typename boost::range_traversal<Rng>::type());
	}

	template< typename Rng >
	[[nodiscard]] auto linear_end_prev(
		Rng&& rng
	) noexcept {
		static_assert(
			!std::is_convertible<typename boost::range_traversal<Rng>::type, boost::iterators::bidirectional_traversal_tag>::value,
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
	[[nodiscard]] constexpr auto begin_next_not_end(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n = 1
	) noexcept {
		auto it=tc::begin_next(rng, n);
		_ASSERTDEBUG(tc::end(rng) != it);
		return it;
	}

	template< typename Rng >
	[[nodiscard]] auto linear_begin_next_not_end(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n
	) noexcept {
		auto it=tc::linear_begin_next(rng, n);
		_ASSERT(tc::end(rng) != it);
		return it;
	}

	////////////////////////////////////////////////////////////////////////////////////
	// take/drop_*_first/last_n

	template< typename Cont >
	void take_first_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) noexcept {
		tc::take_inplace(cont, tc::begin_next(cont,n));
	}

	template< typename Rng, std::enable_if_t<tc::is_range_with_iterators<Rng>::value>* = nullptr >
	[[nodiscard]] auto take_first(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) return_decltype_xvalue_by_ref_MAYTHROW(
		tc::take(std::forward<Rng>(rng), tc::begin_next(rng,n)) // assumes tc::take does not take rng by value
	)

	template< typename It, typename T >
	T advance_forward_bounded(It&& it, T n,	tc::remove_cvref_t<It> const& itBound) noexcept;

	namespace take_first_detail {
		DEFINE_ENUM(ETakePred, etakepred,
			(TAKEANDCONTINUE)
			(TAKEANDBREAK)
			(DONTTAKE)
		)
		namespace no_adl {
			template< typename Sink, typename TakePred >
			struct take_first_sink /*final*/ : tc::sink_value_type_base<Sink> {
				static_assert(tc::is_decayed<Sink>::value);
				static_assert(tc::is_decayed<TakePred>::value);

				template<typename Sink2>
				take_first_sink(Sink2&& sink, TakePred& takepred, tc::break_or_continue& breakorcontinue) noexcept
					: m_sink(std::forward<Sink2>(sink))
					, m_takepred(takepred)
					, m_breakorcontinue(breakorcontinue)
				{}

				template< typename T >
				auto operator()(T&& t) const& MAYTHROW -> tc::common_type_t<
					decltype(tc::continue_if_not_break(std::declval<Sink const&>(), std::declval<T>())),
					INTEGRAL_CONSTANT(tc::break_)
				> {
					auto const Take=[&]() MAYTHROW { return tc::continue_if_not_break(m_sink, std::forward<T>(t)); };
					switch_no_default(m_takepred.take(t)) {
						case etakepredTAKEANDCONTINUE: {
							auto breakorcontinue=Take(); // MAYTHROW
							m_breakorcontinue=breakorcontinue;
							return breakorcontinue; // allow return type to be INTEGRAL_CONSTANT(tc::break_) if possible
						}
						case etakepredTAKEANDBREAK:
							m_breakorcontinue=Take(); // MAYTHROW
							return INTEGRAL_CONSTANT(tc::break_)();
						case etakepredDONTTAKE:
							m_breakorcontinue=tc::continue_;
							return INTEGRAL_CONSTANT(tc::break_)();
					}
				}

				template< typename Rng >
				auto chunk(Rng&& rng) const& MAYTHROW -> tc::common_type_t<
					decltype(tc::continue_if_not_break(tc::mem_fn_chunk(), std::declval<Sink const&>(), tc::take(std::declval<Rng>(), tc::begin(std::declval<Rng&>())))),
					INTEGRAL_CONSTANT(tc::break_)
				> {
					auto_cref(pairitetakepred, m_takepred.take_range(rng)); // MAYTHROW
					auto breakorcontinue=tc::continue_if_not_break(tc::mem_fn_chunk(), m_sink, tc::take(std::forward<Rng>(rng), pairitetakepred.first)); // MAYTHROW
					m_breakorcontinue=breakorcontinue;
					switch_no_default(pairitetakepred.second) {
						case etakepredTAKEANDCONTINUE: return breakorcontinue;
						case etakepredTAKEANDBREAK: return INTEGRAL_CONSTANT(tc::break_)();
					}
				}

			private:
				Sink m_sink;
				TakePred& m_takepred;
				tc::break_or_continue& m_breakorcontinue;
			};

			struct take_first_pred {
				explicit take_first_pred(std::size_t nCount) noexcept
					: m_nCount(VERIFYPRED(nCount, 0<_))
				{}

				template<typename T>
				ETakePred take(T const&) & noexcept {
					--VERIFYPRED(m_nCount, 0<_);
					return may_continue();
				}

				template<typename Rng>
				auto take_range(Rng /*const*/& rng) & MAYTHROW {
					auto it=tc::begin(rng); // MAYTHROW
					m_nCount-=tc::advance_forward_bounded(it, m_nCount, tc::end(rng)); // MAYTHROW
					return std::make_pair(it, may_continue());
				}

			protected:
				ETakePred may_continue() const& noexcept {
					return 0<m_nCount ? etakepredTAKEANDCONTINUE : etakepredTAKEANDBREAK;
				}

				std::size_t m_nCount;
			};
		}

		template< typename TakePred, bool bTruncate, typename Rng >
		auto take_first_impl(Rng&& rng, std::size_t n) noexcept {
			static_assert(!tc::is_range_with_iterators<Rng>::value);
			return [rng=tc::make_reference_or_value(std::forward<Rng>(rng)), n](auto&& sink) MAYTHROW {
				if(0<n) {
					TakePred takepred(n);
					tc::break_or_continue breakorcontinue;
					if(tc::break_==tc::for_each(*rng, no_adl::take_first_sink<tc::decay_t<decltype(sink)>, TakePred>(tc_move_if_owned(sink), takepred, breakorcontinue))) { // MAYTHROW
						return VERIFYINITIALIZED(breakorcontinue);
					} else { // breakorcontinue won't be initialized if rng is empty
						_ASSERT(bTruncate);
					}
				}
				return tc::continue_;
			};
		}
	}

	template< typename Rng, std::enable_if_t<!tc::is_range_with_iterators<Rng>::value>* = nullptr >
	[[nodiscard]] auto take_first(Rng&& rng, std::size_t n=1) return_decltype_noexcept(
		take_first_detail::take_first_impl<take_first_detail::no_adl::take_first_pred, /*bTruncate*/ false>(std::forward<Rng>(rng), n)
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
	[[nodiscard]] auto drop_first(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) return_decltype_xvalue_by_ref_MAYTHROW(
		tc::drop(std::forward<Rng>(rng), tc::begin_next(rng, n)) // assumes tc::drop does not take rng by value
	)

	namespace no_adl {
		template< typename Sink >
		struct drop_first_sink /*final*/ : tc::sink_value_type_base<Sink> {
			static_assert(tc::is_decayed<Sink>::value);

			template<typename Sink2>
			drop_first_sink(Sink2&& sink, std::size_t& nCount) noexcept
				: m_sink(std::forward<Sink2>(sink))
				, m_nCount(nCount)
			{}

			template< typename T >
			auto operator()(T&& t) const& MAYTHROW -> tc::common_type_t<
				INTEGRAL_CONSTANT(tc::continue_),
				decltype(tc::continue_if_not_break(std::declval<Sink const&>(), std::declval<T>()))
			> {
				if(0<m_nCount) {
					--m_nCount;
					return INTEGRAL_CONSTANT(tc::continue_)();
				} else {
					return tc::continue_if_not_break(m_sink, std::forward<T>(t)); // MAYTHROW
				}
			}

			template< typename Rng >
			auto chunk(Rng&& rng) const& MAYTHROW -> tc::common_type_t<
				INTEGRAL_CONSTANT(tc::continue_),
				decltype(tc::continue_if_not_break(tc::mem_fn_chunk(), std::declval<Sink const&>(), tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>()))))
			> {
				auto it=tc::begin(rng);
				m_nCount-=tc::advance_forward_bounded(it, m_nCount, tc::end(rng));
				if(0<m_nCount) {
					_ASSERT(tc::end(rng)==it);
					return INTEGRAL_CONSTANT(tc::continue_)();
				} else {
					return tc::continue_if_not_break(tc::mem_fn_chunk(), m_sink, tc::drop(std::forward<Rng>(rng), tc_move(it))); // MAYTHROW
				}
			}

		private:
			Sink m_sink;
			std::size_t& m_nCount;
		};
	}

	template< typename Rng, std::enable_if_t<!tc::is_range_with_iterators<Rng>::value>* = nullptr >
	auto drop_first(Rng&& rng, std::size_t n=1) noexcept {
		return [rng=tc::make_reference_or_value(std::forward<Rng>(rng)), n](auto&& sink) MAYTHROW {
			auto nCount=n;
			auto breakorcontinue=tc::for_each(*rng, no_adl::drop_first_sink<tc::decay_t<decltype(sink)>>(tc_move_if_owned(sink), nCount)); // MAYTHROW
			_ASSERTEQUAL(nCount, 0);
			return breakorcontinue;
		};
	}

	template< typename Cont >
	void take_last_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) noexcept {
		tc::drop_inplace(cont, tc::end_prev(cont, n));
	}

	template< typename Rng >
	[[nodiscard]] auto take_last(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) return_decltype_xvalue_by_ref_MAYTHROW(
		tc::drop(std::forward<Rng>(rng), tc::end_prev(rng, n)) // assumes tc::drop does not take rng by value
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
	[[nodiscard]] auto drop_last(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n=1) return_decltype_xvalue_by_ref_MAYTHROW(
		tc::take(std::forward<Rng>(rng), tc::end_prev(rng, n)) // assumes tc::take does not take rng by value
	)

	//-------------------------------------------------------------------------------------------------------------------------
	// take_first_truncate

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
	void take_first_truncate_inplace( Cont& rng, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
		auto it=tc::begin(rng);
		tc::advance_forward_bounded( it, n, tc::end(rng) );
		tc::take_inplace( rng, tc_move(it) );
	}

	template< typename Rng, std::enable_if_t<!tc::is_range_with_iterators<Rng>::value>* = nullptr >
	[[nodiscard]] auto take_first_truncate(Rng&& rng, std::size_t n) return_decltype_noexcept(
		take_first_detail::take_first_impl<take_first_detail::no_adl::take_first_pred, /*bTruncate*/ true>(std::forward<Rng>(rng), n)
	)

	template< typename Rng, std::enable_if_t<tc::is_range_with_iterators<Rng>::value>* = nullptr >
	[[nodiscard]] auto take_first_truncate(Rng&& rng, typename boost::range_size< std::remove_reference_t<Rng> >::type n) noexcept
	->decltype(tc::take( std::forward<Rng>(rng), tc::begin(rng) )) {
		auto it=tc::begin(rng);
		tc::advance_forward_bounded( it, n, tc::end(rng) );
		return tc::take( std::forward<Rng>(rng), tc_move(it) );
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// make iterator range

	// subrange from iterator pair
	template< typename It >
	constexpr auto make_iterator_range_impl( It itBegin, It itEnd )
		return_ctor_noexcept( tc::subrange<tc::iterator_base<It>>, ( tc::iterator_base<It>(), tc_move(itBegin), tc_move(itEnd) ) )

	// There is an other make_iterator_range_impl overload for range adaptor based iterarors in range_adaptor.h

	// make sure ADL lookup of index_iterator::make_iterator_range works
	template< typename ItBegin, typename ItEnd >
	[[nodiscard]] constexpr auto make_iterator_range(ItBegin&& itBegin, ItEnd&& itEnd) noexcept {
		return make_iterator_range_impl( std::forward<ItBegin>(itBegin), std::forward<ItEnd>(itEnd) );
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// make empty range

	template< typename T >
	[[nodiscard]] constexpr auto make_empty_range() noexcept {
		return make_iterator_range( std::add_pointer_t<T>(), std::add_pointer_t<T>() );
	}
		
	//-------------------------------------------------------------------------------------------------------------------------
	// make counted range

	template< typename It, typename Count >
	[[nodiscard]] constexpr auto counted( It const& it, Count&& count ) noexcept {
		return make_iterator_range( it, it+std::forward<Count>(count) );
	}

	template< typename T, std::enable_if_t<std::is_reference<T>::value>* =nullptr >
	[[nodiscard]] constexpr auto single(T&& t) noexcept {
		return tc::counted(std::addressof(t),1);
	}

	template< typename T, std::enable_if_t<tc::is_actual_integer<T>::value && std::is_signed<T>::value>* = nullptr>
	[[nodiscard]] bool npos(T t) noexcept {
		return -1 == t;
	}

	template< typename T, std::enable_if_t<tc::is_actual_integer<T>::value && !std::is_signed<T>::value>* = nullptr>
	[[nodiscard]] bool npos(T t) noexcept {
		return std::numeric_limits<T>::max() == t;
	}

	template< typename T, std::enable_if_t<!std::is_integral<T>::value>* = nullptr>
	[[nodiscard]] bool npos(T const& t) noexcept {
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
			static constexpr type pack_border(It&&, Rng&&) noexcept {}
			template<typename It, typename Ref>
			static constexpr type pack_element(It&&, Rng&&, Ref&&) noexcept {}
			template<typename Ref>
			static constexpr type pack_element(Ref&&) noexcept {}
			template<typename It>
			static constexpr type pack_view(Rng&&, It&&, It&&) noexcept {}
			static constexpr type pack_no_element(Rng&&) noexcept {}
			static constexpr type pack_no_element() noexcept {}
		};

		/////////////////////////////////////
		// controlling bound return

		// returning element

		template< typename Rng >
		struct return_element_before final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			static constexpr type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				_ASSERTE(it != tc::begin(rng));
				return boost::prior(it);
			}
		};

		template< typename Rng >
		struct return_element_before_or_null final {
			using type = element_t< typename boost::range_iterator<Rng>::type >;
			static constexpr bool requires_iterator = true;

			static constexpr type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				if( it != tc::begin(rng) ) {
					return static_cast<type>(boost::prior(it));
				} else {
					return type{}; // value initialization to initialize pointers to nullptr
				}
			}
		};

		template< typename Rng >
		struct return_element_before_or_front final {
			using type = element_t< typename boost::range_iterator<Rng>::type >;
			static constexpr bool requires_iterator = true;

			static constexpr type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				if( it != tc::begin(rng) ) {
					return static_cast<type>(boost::prior(it));
				} else {
					return tc::begin(rng);
				}
			}
		};

		template< typename Rng >
		struct return_element_after final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			static constexpr type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				_ASSERTE(it != tc::end(rng));
				return it;
			}
		};

		template< typename Rng >
		struct return_element_after_or_null final {
			using type = element_t< typename boost::range_iterator<Rng>::type >;

			static constexpr type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				if (it != tc::end(rng)) {
					return static_cast<type>(it);
				} else {
					return type{}; // value initialization to initialize pointers to nullptr
				}
			}
		};

		// returning bound

		template< typename Rng >
		struct return_border final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			static constexpr type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				return it;
			}
		};

		template< typename Rng >
		struct return_border_index final {
			using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;
			static constexpr bool requires_iterator = true;

			static constexpr type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
				return tc::explicit_cast<type>(it - tc::begin(rng));
			}
		};

		// returning range

		template< typename >
		struct return_take final {
			static constexpr bool requires_iterator = true;

			template <typename Rng>
			static constexpr auto pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) return_decltype_xvalue_by_ref_noexcept(
				tc::take(std::forward<Rng>(rng), it)
			)
		};

		template< typename Rng >
		struct return_drop final {
			using type = decltype(tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			static constexpr type pack_border(typename boost::range_iterator<Rng>::type it, Rng&& rng) noexcept {
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
			static constexpr type pack_element(It&&, Rng&&, Ref&&) noexcept {
				return true;
			}
			template<typename Ref>
			static constexpr type pack_element(Ref&&) noexcept {
				return true;
			}
			template<typename It>
			static constexpr type pack_view(Rng&&, It&&, It&&) noexcept {
				return true;
			}
			static constexpr type pack_no_element(Rng&&) noexcept {
				return false;
			}
			static constexpr type pack_no_element() noexcept {
				return false;
			}
		};

		// returning element

		template< typename Rng >
		struct return_element final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
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
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&&, Ref&&) noexcept {
				return static_cast<type>(it);
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return type{}; // value initialization to initialize pointers to nullptr
			}
		};

		template< typename Rng >
		struct return_element_or_front final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return it;
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::begin(rng);
			}
		};

		template< typename Rng >
		struct return_value final {
			using type = tc::range_value_t< Rng >;
			static constexpr bool requires_iterator = false;

			template<typename It, typename Ref>
			static constexpr type pack_element(It&&, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Ref>
			static constexpr type pack_element(Ref&& ref) noexcept {
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
			static constexpr type pack_element(It&&, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Ref>
			static constexpr type pack_element(Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			static constexpr type pack_no_element(Rng&&) noexcept {
				return{};
			}
			static constexpr type pack_no_element() noexcept {
				return{};
			}
		};

		template< typename Rng >
		struct return_value_or_none final {
			using type = std::optional< tc::range_value_t< Rng > >;
			static constexpr bool requires_iterator = false;

			template<typename It, typename Ref>
			static constexpr type pack_element(It&&, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Ref>
			static constexpr type pack_element(Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			static constexpr type pack_no_element(Rng&&) noexcept {
				return std::nullopt;
			}
			static constexpr type pack_no_element() noexcept {
				return std::nullopt;
			}
		};

		template< typename Rng >
		struct return_element_index final {
			using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::explicit_cast<type>(it - tc::begin(rng));
			}
			static type pack_no_element(Rng&&) noexcept {
				_ASSERTFALSE;
				return type(0);
			}
		};

		template< typename Rng >
		struct return_element_index_or_none final {
			using type=std::optional<tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >>;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return type(std::in_place, it - tc::begin(rng));
			}
			static constexpr type pack_no_element(Rng&&) noexcept {
				return std::nullopt;
			}
		};

		template< typename Rng >
		struct return_element_index_or_npos final {
			// static_cast<int>(npos) must be -1. Anything else is error-prone. So use range_difference instead of range_size for now.
			// Alternatively, we could return a special type that casts npos to -1.
			using type = tc::size_proxy< typename boost::range_difference<Rng>::type >;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::explicit_cast<type>(it - tc::begin(rng));
			}
			static constexpr type pack_no_element(Rng&&) noexcept {
				return type(static_cast<typename boost::range_difference<Rng>::type>(-1));
			}
		};

		template< typename Rng >
		struct return_element_index_or_size final {
			using type = tc::size_proxy< typename boost::range_size< std::remove_reference_t<Rng> >::type >;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::explicit_cast<type>(it - tc::begin(rng));
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::size(rng);
			}
		};

		template< typename Rng >
		struct return_singleton_range final {
			using type = tc::make_subrange_result_t<Rng>;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::slice(std::forward<Rng>(rng), it, tc::next(it));
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
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&&, Ref&&) noexcept {
				return tc::next(it);
			}
			static constexpr type pack_view(Rng&&, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
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
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::next(it);
			}
			static constexpr type pack_view(Rng&&, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return itEnd;
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::begin(rng);
			}
		};

		template< typename Rng >
		struct return_border_before final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&&, Ref&&) noexcept {
				return it;
			}
			static constexpr type pack_view(Rng&&, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return itBegin;
			}
			static type pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return tc::begin(rng);
			}
		};

		template< typename Rng >
		struct return_border_before_or_begin final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&&, Ref&&) noexcept {
				return it;
			}
			static constexpr type pack_view(Rng&&, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return itBegin;
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::begin(rng);
			}
		};

		template< typename Rng >
		struct return_border_before_or_end final {
			using type = typename boost::range_iterator<Rng>::type;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&&, Ref&&) noexcept {
				return it;
			}
			static constexpr type pack_view(Rng&&, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return itBegin;
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::end(rng);
			}
		};

		// returning range

		template< typename Rng >
		struct return_take_before final {
			using type = tc::make_subrange_result_t<Rng>;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), it);
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
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
			using type = tc::make_subrange_result_t<Rng>;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), it);
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::take(std::forward<Rng>(rng), itBegin);
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template< typename Rng >
		struct return_take_before_or_all final {
			using type = tc::make_subrange_result_t<Rng>;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), it);
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::take(std::forward<Rng>(rng), itBegin);
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::end(rng));
			}
		};

		template< typename Rng >
		struct return_take_after final {
			using type = tc::make_subrange_result_t<Rng>;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::next(it));
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
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
			using type = tc::make_subrange_result_t<Rng>;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::next(it));
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::take(std::forward<Rng>(rng), itEnd);
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template< typename Rng >
		struct return_take_after_or_all final {
			using type = tc::make_subrange_result_t<Rng>;
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::next(it));
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::take(std::forward<Rng>(rng), itEnd);
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::end(rng));
			}
		};

		template< typename Rng >
		struct return_drop_before final {
			using type = decltype(tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop(std::forward<Rng>(rng), it);
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
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
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop( std::forward<Rng>(rng), it );
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::drop(std::forward<Rng>(rng), itBegin);
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::drop( std::forward<Rng>(rng), tc::end(rng));
			}
		};

		template< typename Rng >
		struct return_drop_before_or_all final {
			using type = decltype(tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop(std::forward<Rng>(rng), it);
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type) noexcept {
				return tc::drop(std::forward<Rng>(rng), itBegin);
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::drop(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template< typename Rng >
		struct return_drop_after final {
			using type = decltype(tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop(std::forward<Rng>(rng), tc::next(it));
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
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
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop( std::forward<Rng>(rng), tc::next(it) );
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::drop(std::forward<Rng>(rng), itEnd);
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::drop(std::forward<Rng>(rng), tc::end(rng));
			}
		};

		template< typename Rng >
		struct return_drop_after_or_all final {
			using type = decltype(tc::drop( std::declval<Rng>(), tc::begin(std::declval<Rng&>()) ));
			static constexpr bool requires_iterator = true;

			template<typename Ref>
			static constexpr type pack_element(typename boost::range_iterator<Rng>::type it, Rng&& rng, Ref&&) noexcept {
				return tc::drop( std::forward<Rng>(rng), tc::next(it) );
			}
			static constexpr type pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::drop(std::forward<Rng>(rng), itEnd);
			}
			static constexpr type pack_no_element(Rng&& rng) noexcept {
				return tc::drop(std::forward<Rng>(rng), tc::begin(rng));
			}
		};

		template<typename Rng>
		struct return_view {
			using type = decltype(tc::slice(std::declval<Rng>(), tc::begin(std::declval<Rng&>()), tc::end(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			static constexpr auto pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::slice(std::forward<Rng>(rng), itBegin, itEnd);
			}

			static auto pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return tc::slice(std::forward<Rng>(rng), tc::begin(rng), tc::end(rng));
			}
		};

		template<typename Rng>
		struct return_view_or_empty {
			using type = decltype(tc::take(std::declval<Rng>(), tc::begin(std::declval<Rng&>())));
			static constexpr bool requires_iterator = true;

			static constexpr auto pack_view(Rng&& rng, typename boost::range_iterator<Rng>::type itBegin, typename boost::range_iterator<Rng>::type itEnd) noexcept {
				return tc::slice(std::forward<Rng>(rng), itBegin, itEnd);
			}

			static constexpr auto pack_no_element(Rng&& rng) noexcept {
				return tc::take(std::forward<Rng>(rng), tc::begin(rng));
			}
		};
	} // namespace no_adl
	using no_adl::return_void;
	using no_adl::return_element_before;
	using no_adl::return_element_before_or_null;
	using no_adl::return_element_before_or_front;
	using no_adl::return_element_after;
	using no_adl::return_element_after_or_null;
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
	using no_adl::return_border_before;
	using no_adl::return_border_before_or_begin;
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

	template <template<typename> class RangeReturn, typename Rng>
	[[nodiscard]] decltype(auto) begin_element(
		Rng&& rng
	) noexcept {
		auto const itEnd = tc::end(rng);
		auto const itBegin = tc::begin(rng);
		if (itBegin==itEnd) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			return RangeReturn<Rng>::pack_element(itBegin, std::forward<Rng>(rng), *itBegin);
		}
	}

	// Write as macros to keep temporary iterators alive.
	// By standard, the lifetime of a reference is limited to the lifetime of the iterator.
	// for ranges without iterators, use tc::linear_at<>
	#define tc_front(rng) (*tc::begin_element<tc::return_element>(rng))
	#define tc_only(rng) (*tc::only<tc::return_element>(rng))
	#define tc_back(rng) (*tc::end_prev(rng))
	#define tc_at(rng, i) (*tc::begin_next_not_end(rng,(i)))
	#define tc_linear_at(rng, i) (*tc::linear_begin_next_not_end(rng,(i)))
	#define tc_linear_back(rng) (*tc::linear_end_prev(rng))
	#define tc_reverse_at(rng, i) (*tc::end_prev((rng),(i)+1))

	//-------------------------------------------------------------------------------------------------------------------------
	// with_optional/transform_optional/only/if_non_empty_with_[only/front]

	template<typename T, typename Func>
	bool with_optional(T&& ot, Func func) MAYTHROW {
		if(tc::bool_cast(ot)) {
			RETURNS_VOID(func(*std::forward<T>(ot))); // MAYTHROW
			return true;
		} else {
			return false;
		}
	}

	template<typename T, typename Func>
	[[nodiscard]] auto transform_optional(T&& ot, Func func) MAYTHROW -> std::optional<
		tc::decay_t<decltype(func(*std::forward<T>(ot)))>
	> {
		if(tc::bool_cast(ot)) {
			return std::optional<
				tc::decay_t<decltype(func(*std::forward<T>(ot)))>
			>(
				std::in_place,
				func(*std::forward<T>(ot)) // MAYTHROW
			);
		} else {
			return std::nullopt;
		}
	}

	template<typename Rng, typename Func>
	bool if_nonempty_with_only(Rng&& rng, Func func) MAYTHROW {
		bool bCalled=false;
		tc::for_each(std::forward<Rng>(rng), [&](auto&&... args) MAYTHROW {
			VERIFY(tc::change(bCalled, true)); // only a single element is allowed

			RETURNS_VOID( func(std::forward<decltype(args)>(args)...) ); // MAYTHROW

			return INTEGRAL_CONSTANT(tc::continue_)();
		});
		return bCalled;
	}

	template<typename Rng, typename Func>
	bool if_nonempty_with_front(Rng&& rng, Func func) MAYTHROW {
		return tc::break_==tc::for_each(std::forward<Rng>(rng), [&](auto&&... args) MAYTHROW{
			RETURNS_VOID(func(std::forward<decltype(args)>(args)...)); // MAYTHROW
			return INTEGRAL_CONSTANT(tc::break_)();
		});
	}

	namespace only_detail {
		template<template<typename> class RangeReturn, typename Rng, typename Func, std::enable_if_t<RangeReturn<Rng>::requires_iterator>* = nullptr>
		decltype(auto) only(Rng&& rng, Func func) MAYTHROW {
			auto const itEnd = tc::end(rng);
			auto const itBegin = tc::begin(rng);
			if(itEnd != itBegin && (itEnd == tc::next(itBegin) || tc::bool_cast(func()))) {
				return RangeReturn<Rng>::pack_element(itBegin, std::forward<Rng>(rng), *itBegin);
			} else {
				return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
			}
		}

		template<template<typename> class RangeReturn, typename Rng, typename Func, std::enable_if_t<!RangeReturn<Rng>::requires_iterator>* = nullptr>
		typename RangeReturn<Rng>::type only(Rng&& rng, Func func) MAYTHROW {
			std::optional<typename RangeReturn<Rng>::type> ot;
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
	[[nodiscard]] decltype(auto) only(Rng&& rng, Func&& func = Func()) MAYTHROW {
		return only_detail::only<RangeReturn>(std::forward<Rng>(rng), std::forward<Func>(func));
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// as_pointers
	// get a consecutive block of memory from range and return an iterator_range of pointers

	template< typename Rng, std::enable_if_t<tc::is_safely_convertible<Rng&&, tc::subrange<tc::iterator_base<decltype( ptr_begin( std::declval<Rng>() ) )>>>::value>* = nullptr >
	[[nodiscard]] auto as_pointers(Rng&& rng) noexcept ->tc::subrange<
		tc::iterator_base<
			decltype( ptr_begin( std::declval<Rng>() ) )
		>
	> {
		return std::forward<Rng>(rng);
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// as_array

	template< typename T, std::size_t N, std::enable_if_t<is_char<T>::value>* = nullptr >
	[[nodiscard]] auto as_array(T (&at)[N] ) return_decltype_noexcept(
		tc::counted( std::addressof(at[0]), N )
	)

	template< typename T, std::size_t N, std::enable_if_t<is_char<T>::value && std::is_const<T>::value && !std::is_volatile<T>::value>* = nullptr >
	[[nodiscard]] constexpr auto string_literal(T (&at)[N] ) return_decltype_noexcept(
		tc::counted( std::addressof(at[0]), N-1 )
	)

	template< typename T >
	using ptr_range = subrange < iterator_base<T*> >;

	namespace no_adl {
		struct empty_range {
			template< typename Func >
			constexpr auto operator()(Func const&) const& noexcept {
				return INTEGRAL_CONSTANT(tc::continue_)();
			}

			template< typename Func >
			static constexpr auto enumerate_reversed(empty_range const&, Func const&) noexcept {
				return INTEGRAL_CONSTANT(tc::continue_)();
			}

			template<typename T>
			constexpr operator tc::ptr_range<T>() const& noexcept {
				return tc::make_empty_range<T>();
			}

			constexpr unsigned int size() const& noexcept {
				return 0;
			}
		};
	}
	using no_adl::empty_range;

	namespace no_adl {
		static_assert(
			// As long as our compiler do not correctly handle alias templates as template template parameters we need is_ptr_range
			// If this assert triggers, usage below can be replaced by
			// tc::is_instance<tc::ptr_range, std::remove_reference_t<T>>::value
			!tc::is_instance<tc::ptr_range, tc::ptr_range<int>>::value
		);

		template<typename T> struct is_ptr_range : public std::false_type {};
		template<typename T> struct is_ptr_range<T const> : public is_ptr_range<T> {};
		template<typename T> struct is_ptr_range<T volatile> : public is_ptr_range<T> {};
		template<typename T> struct is_ptr_range<T const volatile> : public is_ptr_range<T> {};
		template<typename Y> struct is_ptr_range<tc::ptr_range<Y>> : public std::true_type {};

		template<typename T, typename TSource>
		struct is_class_safely_constructible<tc::ptr_range<T>, tc::type::list<TSource>> final
			: std::integral_constant<
				bool,
				is_ptr_range<std::remove_reference_t<TSource>>::value ||
				std::is_lvalue_reference<TSource>::value ||
				std::is_pointer<std::remove_reference_t<TSource>>::value ||
				std::is_same<tc::decay_t<TSource>, tc::empty_range>::value
			>
		{
		};

		template<typename Rng, typename Enable=void>
		struct ptr_range_type_impl final {};

		template<typename Rng>
		struct ptr_range_type_impl<Rng, std::enable_if_t<
			tc::is_safely_convertible<
				Rng,
				tc::ptr_range<std::remove_pointer_t<decltype(tc::ptr_begin(std::declval<Rng>()))>>
			>::value
		>> final {
			using type = tc::ptr_range<std::remove_pointer_t<decltype(tc::ptr_begin(std::declval<Rng>()))>>;
		};
	}

	template<typename Rng>
	using ptr_range_t = typename no_adl::ptr_range_type_impl<Rng>::type;

	namespace no_adl {
		template<typename T1, typename T2, typename Enable=void>
		struct common_ptr_range_impl_base {};

		template<typename T1, typename T2>
		struct common_ptr_range_impl_base<tc::ptr_range<T1>, tc::ptr_range<T2>, std::enable_if_t<
			sizeof(T1)==sizeof(std::remove_pointer_t<tc::common_type_t<T1*, T2*>>) &&
			sizeof(T2)==sizeof(std::remove_pointer_t<tc::common_type_t<T1*, T2*>>)
		>> {
			using type = tc::ptr_range<std::remove_pointer_t<tc::common_type_t<T1*, T2*>>>;
		};

		template<typename Rng0, typename Rng1, typename Enable=void>
		struct common_ptr_range_impl final {};

		template<>
		struct common_ptr_range_impl<empty_range, empty_range, void> final
		{
			using type = empty_range;
		};

		template<typename Rng>
		struct common_ptr_range_impl<empty_range, Rng, tc::void_t<tc::ptr_range_t<Rng>>> final
		{
			using type = tc::ptr_range_t<Rng>;
		};

		template<typename Rng>
		struct common_ptr_range_impl<Rng, empty_range, tc::void_t<tc::ptr_range_t<Rng>>> final
		{
			using type = tc::ptr_range_t<Rng>;
		};

		template<typename Rng0, typename Rng1>
		struct common_ptr_range_impl<Rng0, Rng1, tc::void_t<tc::ptr_range_t<Rng0>, tc::ptr_range_t<Rng1>>> final: common_ptr_range_impl_base<tc::ptr_range_t<Rng0>, tc::ptr_range_t<Rng1>> {};

		template<typename Rng0, typename Rng1>
		using common_ptr_range_impl_t = typename common_ptr_range_impl<Rng0, Rng1>::type;

		template<typename TypeList, typename Enable=void>
		struct has_common_ptr_range final: std::false_type {};

		template<typename TypeList>
		struct has_common_ptr_range<TypeList, tc::void_t<typename tc::type::accumulate<TypeList, common_ptr_range_impl_t>::type>> final: std::true_type {};

		// tc::common_reference_xvalue_as_ref_t customization tc::ptr_range: If
		//		1. all input ranges are tc::is_safely_convertible to tc::ptr_range and,
		//		2. those tc::ptr_range(s) has_common_ptr_range,
		// the result type is common_ptr_range_impl_t. 
		template<typename TypeList>
		struct common_reference_xvalue_as_ref<TypeList, std::enable_if_t<
			!has_guaranteed_common_reference<TypeList>::value &&
			has_common_ptr_range<TypeList>::value
		>> final {
			using type = typename tc::type::accumulate<TypeList, common_ptr_range_impl_t>::type;
		};

		// tc::common_reference_xvalue_as_ref_t customization tc::subrange: If
		//		1. the base ranges (tc::subrange_param_t) have a tc::common_reference_xvalue_as_ref which is an lvalue reference,
		// the result type is tc::make_subrange_result_t<tc::common_reference_xvalue_as_ref<base ranges...>>
		template<typename TypeList, typename Enable=void>
		struct common_subrange_lvalue_reference {};

		template<typename TypeList>
		struct common_subrange_lvalue_reference<TypeList, std::enable_if_t<
			std::is_lvalue_reference<
				typename common_reference_xvalue_as_ref<
					tc::type::transform_t<
						tc::type::transform_t<
							tc::type::transform_t<
								TypeList,
								tc::remove_rvalue_reference_t
							>,
							tc::make_subrange_result_t
						>,
						tc::subrange_param_t
					>
				>::type
			>::value
		>> {
			using type = tc::make_subrange_result_t<
				typename common_reference_xvalue_as_ref<
					tc::type::transform_t<
						tc::type::transform_t<
							tc::type::transform_t<
								TypeList,
								tc::remove_rvalue_reference_t
							>,
							tc::make_subrange_result_t
						>,
						tc::subrange_param_t
					>
				>::type
			>;
		};
	}
	template<typename TypeList>
	using common_subrange_lvalue_reference_t = typename no_adl::common_subrange_lvalue_reference<TypeList>::type;

	namespace no_adl {
		template<typename TypeList, typename Enable=void>
		struct has_common_subrange_lvalue_reference final: std::false_type {};

		template<typename TypeList>
		struct has_common_subrange_lvalue_reference<
			TypeList,
			tc::void_t<tc::common_subrange_lvalue_reference_t<TypeList>>
		>: std::true_type {};

		template<typename TypeList>
		struct common_reference_xvalue_as_ref<TypeList, std::enable_if_t<
			!has_guaranteed_common_reference<TypeList>::value &&
			has_common_subrange_lvalue_reference<TypeList>::value
		>> final {
			using type = tc::common_subrange_lvalue_reference_t<TypeList>;
		};
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

	template<typename Rng, std::enable_if_t<has_ptr_begin<Rng>::value>* = nullptr>
	[[nodiscard]] auto range_as_blob(Rng&& rng) noexcept {
		static_assert( std::is_trivially_copyable< tc::range_value_t< Rng > >::value, "as_blob only works on std::is_trivially_copyable types" );
		using cv_value_type = std::remove_pointer_t<decltype(tc::ptr_begin(rng))>;
		return tc::make_iterator_range( 
			reinterpret_cast<same_cvref_t<unsigned char, cv_value_type>*>( tc::ptr_begin(rng) ),
			reinterpret_cast<same_cvref_t<unsigned char, cv_value_type>*>( tc::ptr_end(rng) )
		);
	}

	template<typename T, std::enable_if_t<std::is_trivially_copyable<std::remove_reference_t<T>>::value>* = nullptr>
	[[nodiscard]] auto as_blob(T&& t) noexcept {
		return tc::range_as_blob( tc::single(/* no std::forward<T> */ t) );
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
		assert_no_overlap_impl::assert_no_overlap(tc::single(lhs), tc::single(rhs));
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<has_ptr_begin<Lhs>::value && has_ptr_begin<Rhs>::value>* = nullptr >
	void assert_no_overlap(Lhs const& lhs, Rhs const& rhs) noexcept {
		assert_no_overlap_impl::assert_no_overlap(tc::single(lhs), tc::single(rhs));
		assert_no_overlap_impl::assert_no_overlap(lhs, rhs);
	}

#ifdef _CHECKS
	namespace no_adl {
		template< typename Rng >
		struct SSinglePassRange {
			explicit SSinglePassRange(Rng&& rng) noexcept
				: m_rng(tc::aggregate_tag, std::forward<Rng>(rng))
			{}

			tc::reference_or_value<Rng&&> m_rng;
			bool mutable m_bFirstPass = true;

			template< typename Func >
			auto operator()(Func func) && MAYTHROW {
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
