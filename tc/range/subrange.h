
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/tc_move.h"
#include "../base/trivial_functors.h"
#include "../base/modified.h"
#include "../base/invoke.h"
#include "../container/container_traits.h"
#include "../algorithm/size.h"
#include "../algorithm/size_linear.h"
#include "../algorithm/size_bounded.h"
#include "../base/assign.h"
#include "../base/rvalue_property.h"
#include "../optional.h"
#include "../storage_for.h"
#include "../base/scope.h"

#include "empty_range.h"
#include "index_range.h"
#include "meta.h"
#include "range_adaptor.h"

#include <type_traits>
#include <memory>
#include <deque>

namespace tc {
	namespace return_detail::no_adl {
		template<typename ReturnSubrange>
		struct return_subrange_or_assert;
	}
	namespace no_adl {
		struct return_border;
		struct return_take_or_empty;
		struct return_drop_or_empty;
	}
	using no_adl::return_border;
	using return_take = return_detail::no_adl::return_subrange_or_assert<no_adl::return_take_or_empty>;
	using return_drop = return_detail::no_adl::return_subrange_or_assert<no_adl::return_drop_or_empty>;

	namespace no_adl {
		template < typename It >
		struct universal_range;
		template< typename Rng >
		struct subrange;
	}
	using no_adl::universal_range;
	using no_adl::subrange;

	//-------------------------------------------------------------------------------------------------------------------------
	// Utility functions.
#if !defined(_MSC_VER)
	// On libc++ and libstdc++, std::pointer_trait is broken.
	// E.g. on libc++, it is misisng a specialization for std::__wrap_iter (aka std::vector::iterator).
	template <tc::contiguous_range Rng>
	using ptr_iterator_t = decltype(std::to_address(std::declval<tc::iterator_t<Rng>>()));
#else
	// On MSVC, std::to_address isn't SFINAE friendly, so we have to use std::pointer_traits.
	template <tc::contiguous_range Rng>
	using ptr_iterator_t = typename std::pointer_traits<tc::iterator_t<Rng>>::element_type*;
#endif

	template<tc::contiguous_range Rng> requires tc::borrowed_range<Rng>
	[[nodiscard]] constexpr ptr_iterator_t<Rng> ptr_begin(Rng&& rng) noexcept {
		return std::to_address(tc::begin(rng));
	}

	template<tc::contiguous_range Rng> requires tc::borrowed_range<Rng>
	[[nodiscard]] constexpr ptr_iterator_t<Rng> ptr_end(Rng&& rng) noexcept {
		if constexpr (tc::common_range<Rng>) {
			return std::to_address(tc::end(rng));
		} else {
			return tc::ptr_begin(rng) + tc::size_linear_raw(rng);
		}
	}

#ifdef _CHECKS
	namespace no_adl {
		template< typename Rng >
		struct SSinglePassRange : tc::noncopyable {
			explicit SSinglePassRange(Rng&& rng) noexcept
			: m_rng(tc_move(rng)) {}
			template< typename Sink >
			constexpr auto operator()(Sink&& sink) && MAYTHROW {
				_ASSERTE( tc::change(m_bFirstPass, false) );
				return tc::for_each(tc_move(m_rng), std::forward<Sink>(sink));
			}

			Rng m_rng;
			bool mutable m_bFirstPass = true;
		};
	}

	template< typename Rng >
	auto assert_single_pass(Rng&& rng) noexcept {
		return no_adl::SSinglePassRange<Rng>(tc_move(rng)); // Disallow lvalue references.
	}
#else
	template< typename Rng >
	decltype(auto) assert_single_pass(Rng&& rng) noexcept {
		return std::forward<Rng>(rng);
	}
#endif
	
	//-------------------------------------------------------------------------------------------------------------------------
	// subrange

	namespace subrange_detail {
		template<typename Rng>
		static constexpr decltype(auto) whole_range(Rng&& rng) noexcept {
			if constexpr( tc::instance<std::remove_reference_t<Rng>, tc::subrange> ) {
				return std::forward<Rng>(rng).base_range();
			} else {
				return std::forward<Rng>(rng);
			}
		}

		template<typename Rng>
		using whole_range_t = tc::remove_rvalue_reference_t<decltype(subrange_detail::whole_range(std::declval<Rng>()))>;

		// It may not be tc::safely_constructible_from because of slicing (e.g. tc::iterator_t<std::vector (const)> on Windows, or tc::element_t)
		template <typename TTarget, typename TSource> constexpr auto is_index_safely_constructible = std::is_constructible<TTarget, TSource>::value;

		// Unlike tc::safely_constructible_from, we want to prevent pointer slicing here, as we're dealing with arrays, not single values.
		template<typename TTarget, typename TSource> constexpr auto is_ptr_safely_constructible = false;
		template<typename T> constexpr auto is_ptr_safely_constructible<T*, T*> = true;
		template<typename T> constexpr auto is_ptr_safely_constructible<T const*, T*> = true;
		template<typename T> constexpr auto is_ptr_safely_constructible<T volatile*, T*> = true;
		template<typename T> constexpr auto is_ptr_safely_constructible<T const volatile*, T*> = true;

		// When forwarding `Rhs` into `Rng`, do we first need to translate existing indices of `Rhs` because they might be invalidated?
		template <typename Rng, typename Rhs>
		constexpr auto require_index_translation = true;
		template <typename Rng, typename Rhs>
			requires tc::borrowed_range<Rng>
		constexpr auto require_index_translation<Rng, Rhs> = false; // Forwarding `Rhs` doesn't affect indices at all.
		template <typename Rng, typename Rhs>
			requires (!tc::borrowed_range<Rng>) && tc::stable_index_on_move<Rng>
				&& (!std::is_lvalue_reference<Rhs>::value && !std::is_const<std::remove_reference_t<Rhs>>::value)
		constexpr auto require_index_translation<Rng, Rhs> = false; // We perform a move but have a stable index.

		namespace no_adl {
			template <typename Rng>
			struct get_index {
				template <typename Rhs>
				static constexpr auto begin(Rhs& rng) return_decltype_MAYTHROW(tc::begin_index(rng));
				template <typename BaseRng, typename Rhs>
				static constexpr auto begin(BaseRng&& baserng, Rhs&) return_decltype_MAYTHROW(tc::begin_index(baserng));

				template <typename Rhs>
				static constexpr auto end(Rhs& rng) return_decltype_MAYTHROW(tc::end_index(rng));
				template <typename BaseRng, typename Rhs>
				static constexpr auto end(BaseRng&& baserng, Rhs&) return_decltype_MAYTHROW(tc::end_index(baserng));
			};

			template <typename It>
			struct get_index<universal_range<It>> {
				template <typename Rhs>
				static constexpr auto begin(Rhs& rng) return_decltype_MAYTHROW(tc::begin(rng));
				template <typename Rhs>
				static constexpr auto begin(universal_range<It>, Rhs& rng) return_decltype_MAYTHROW(tc::begin(rng));

				template <typename Rhs>
				static constexpr auto end(Rhs& rng) return_decltype_MAYTHROW(tc::end(rng));
				template <typename Rhs>
				static constexpr auto end(universal_range<It>, Rhs& rng) return_decltype_MAYTHROW(tc::end(rng));
			};

			template <typename T>
			struct get_index<universal_range<T*>> {
				template <typename Rhs>
				static constexpr auto begin(Rhs& rng) return_decltype_MAYTHROW(tc::ptr_begin(rng));
				template <typename Rhs>
				static constexpr auto begin(universal_range<T*>, Rhs& rng) return_decltype_MAYTHROW(tc::ptr_begin(rng));

				template <typename Rhs>
				static constexpr auto end(Rhs& rng) return_decltype_MAYTHROW(tc::ptr_end(rng));
				template <typename Rhs>
				static constexpr auto end(universal_range<T*>, Rhs& rng) return_decltype_MAYTHROW(tc::ptr_end(rng));
			};
		}
		using no_adl::get_index;
	}

	namespace no_adl {
		// meta function to determine the correct type
		template<typename Rng>
		struct make_subrange_result {
			using type = subrange<Rng>;
		};

		// collapse subrange< subrange< ... > > to single subrange
		template <typename Rng> requires tc::instance<std::remove_reference_t<Rng>, tc::subrange> 
		struct make_subrange_result<Rng> : make_subrange_result<tc::subrange_detail::whole_range_t<Rng>> {};

		// turn subrange< borrowed_range< ... > > to subrange< universal_range<...> >
		template <typename Rng> requires
			(!tc::instance<std::remove_reference_t<Rng>, tc::subrange>) &&
			(!tc::is_index_iterator<tc::iterator_t<Rng>>) && // storing two index_iterator is worse than storing ptr + two indices
			tc::borrowed_range<Rng> && tc::common_range<Rng>
		struct make_subrange_result<Rng> {
			using type = subrange<universal_range<tc::iterator_t<Rng>>>;
		};
	}
	template <typename Rng>
	using make_subrange_result_t = typename no_adl::make_subrange_result<Rng>::type;

	namespace no_adl {
		// The universal range is the superset of all possible ranges build from iterator pairs It.
		// It is used as Rng type of subrange when the range isn't known.
		template <typename It>
		struct universal_range {
			constexpr universal_range() noexcept = default;

			template <tc::common_range Rhs> requires
				(!std::is_pointer<It>::value) &&
				subrange_detail::is_index_safely_constructible<It, tc::iterator_t<Rhs>>
			constexpr universal_range(Rhs&&) noexcept {}

			template <tc::contiguous_range Rhs> requires
				std::is_pointer<It>::value &&
				subrange_detail::is_ptr_safely_constructible<It, tc::ptr_iterator_t<Rhs>>
			constexpr universal_range(Rhs&&) noexcept {}

			// subrange constructors assume that "universal_range constructible from Rhs" <=> "can call subrange_detail::get_index".
			// Without deleting construction from non-ranges, this assumption breaks for tc_return_cast.
			template <typename Rhs> requires (!tc::range_with_iterators<Rhs>)
			universal_range(Rhs&&) = delete;

			template <typename It_ = It>
			constexpr It begin() const& noexcept {
				static_assert(tc::dependent_false<It_>::value, "tc::universal_range::begin() does not exist");
				return std::declval<It>();
			}
			template <typename It_ = It>
			constexpr It end() const& noexcept {
				static_assert(tc::dependent_false<It_>::value, "tc::universal_range::end() does not exist");
				return std::declval<It>();
			}
		};
	}

	template <typename It>
	constexpr auto enable_borrowed_range<universal_range<It>> = true;

	namespace no_adl {
		template< typename Rng >
		struct subrange
			: tc::index_range_adaptor<subrange<Rng>, Rng, tc::index_range_adaptor_flags::inherit_behavior>
			// Disable compiler generated special member functions.
			// Implicit construction from another subrange of the same type may still be possible via the user-defined constructor below.
			, std::conditional_t<tc::borrowed_range<Rng>,
				tc::copyable,
				std::conditional_t<tc::stable_index_on_move<Rng>,
					tc::noncopyable,
					tc::nonmovable
				>
			>
		{
			static_assert( std::same_as<subrange, tc::make_subrange_result_t<Rng>>, "Use tc::make_subrange_result_t to construct subrange type." );

		private:
			using this_type = subrange;
			using base_ = typename subrange::index_range_adaptor;

		public:
			using typename base_::tc_index;

		private:
			tc_index m_idxBegin;
			tc_index m_idxEnd;

		private:
			DEFINE_NESTED_TAG_TYPE(integer_tag)
			DEFINE_NESTED_TAG_TYPE(index_tag)

			// ctor from rng/numeric begin/numeric end
			template<typename Rhs, typename N>
			constexpr subrange(integer_tag_t, Rhs&& wholerng, N nFrom, N nTo) noexcept
				: subrange(tc_move_if_owned(wholerng))
			{
				take_inplace(tc_modified(this->begin_index(), this->advance_index(_, nTo)));
				drop_inplace(tc_modified(this->begin_index(), this->advance_index(_, nFrom)));
			}

			// ctor from baserng/begin/end (with translation)
			template<typename Rhs, typename IndexBegin, typename IndexEnd> requires subrange_detail::require_index_translation<Rng, Rhs>
			constexpr subrange(index_tag_t, Rhs&& wholerng, IndexBegin&& idxBegin, IndexEnd&& idxEnd) noexcept
				: subrange(integer_tag,
					tc_move_if_owned(wholerng),
					tc::distance_to_index(wholerng, tc::begin_index(wholerng), tc_move_if_owned(idxBegin)),
					tc::distance_to_index(wholerng, tc::begin_index(wholerng), tc_move_if_owned(idxEnd))
				)
			{}
			// ctor from baserng/begin/end (no index translation)
			template<typename Rhs, typename IndexBegin, typename IndexEnd> requires (!subrange_detail::require_index_translation<Rng, Rhs>)
			constexpr subrange(index_tag_t, Rhs&& wholerng, IndexBegin&& idxBegin, IndexEnd&& idxEnd) noexcept
				: base_(aggregate_tag, tc_move_if_owned(wholerng))
				, m_idxBegin(tc_move_if_owned(idxBegin))
				, m_idxEnd(tc_move_if_owned(idxEnd))
			{}

		public:
			// default ctor (for deferred initialization)
			constexpr subrange() = default;

			// ctor from empty range
			constexpr subrange(tc::empty_range) noexcept(std::is_nothrow_default_constructible<base_>::value) requires std::is_default_constructible<base_>::value
				: base_()
				, m_idxBegin()
				, m_idxEnd()
			{}

			// ctor from whole range
			template<typename Rhs> requires
				(!tc::instance<std::remove_cvref_t<Rhs>, tc::subrange>) && (!tc::instance<std::remove_cvref_t<Rhs>, tc::universal_range>) && tc::safely_constructible_from<Rng, Rhs>
			constexpr subrange(Rhs&& rng) noexcept(
				std::is_nothrow_constructible<base_, aggregate_tag_t, Rhs>::value &&
				noexcept(subrange_detail::get_index<Rng>::begin(rng)) &&
				noexcept(subrange_detail::get_index<Rng>::end(rng))
			)
				: base_(aggregate_tag, tc_move_if_owned(rng))
				, m_idxBegin(subrange_detail::get_index<Rng>::begin(this->base_range_best_access(), rng))
				, m_idxEnd(subrange_detail::get_index<Rng>::end(this->base_range_best_access(), rng))
			{}

			// ctor from other subrange
			template <typename Rhs> requires
				tc::instance<std::remove_cvref_t<Rhs>, tc::subrange> && tc::safely_constructible_from<Rng, subrange_detail::whole_range_t<Rhs>> &&
				// If Rhs is the same subrange, we must not have a copy constructor we could have used instead.
				(!std::same_as<std::remove_cvref_t<Rhs>, subrange> || !tc::borrowed_range<Rhs>) &&
				// If we require index translation, we must have a random access range.
				(!subrange_detail::require_index_translation<Rng, Rhs> || tc::random_access_range<Rng>)
			constexpr subrange(Rhs&& rng) noexcept(noexcept(tc::begin_index(rng)) && noexcept(tc::end_index(rng)))
				: subrange(index_tag,
					subrange_detail::whole_range(tc_move_if_owned(rng)),
					subrange_detail::get_index<Rng>::begin(rng),
					subrange_detail::get_index<Rng>::end(rng)
				)
			{}

			// ctor for tc::slice
			template<typename Rhs, typename Begin, typename End> requires
				tc::safely_constructible_from<Rng, subrange_detail::whole_range_t<Rhs>> &&
				subrange_detail::is_index_safely_constructible<tc_index, decltype(tc::iterator2index<Rhs>(std::declval<Begin>()))> &&
				subrange_detail::is_index_safely_constructible<tc_index, decltype(tc::iterator2index<Rhs>(std::declval<End>()))>
			constexpr explicit subrange(Rhs&& rng, Begin&& begin, End&& end) noexcept
				: subrange(index_tag,
					subrange_detail::whole_range(tc_move_if_owned(rng)),
					tc::iterator2index<Rhs>(tc_move_if_owned(begin)),
					tc::iterator2index<Rhs>(tc_move_if_owned(end))
				)
			{
				static_assert(!subrange_detail::require_index_translation<Rng, subrange_detail::whole_range_t<Rhs>> || tc::random_access_range<Rng>,
					"We require index translation but do not have a random access range. Potential fixes: make Rng a random access range, specialize tc::enable_stable_index_on_move for Rng and move Rhs in, or specialize tc::enable_borrowed_range");
			}

		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				return m_idxBegin;
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index {
				return m_idxEnd;
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return idx == m_idxEnd;
			}

		public:
			template< typename It >
			constexpr void take_inplace( It&& it ) & noexcept {
				m_idxEnd=tc::iterator2index<Rng>( std::forward<It>(it) );
			}

			template< typename It >
			constexpr void drop_inplace( It&& it ) & noexcept {
				m_idxBegin=tc::iterator2index<Rng>( std::forward<It>(it) );
			}
		};
	}

	template<typename Rng>
	constexpr auto enable_stable_index_on_move<tc::subrange<Rng>> = tc::stable_index_on_move<Rng>;
	template <typename Rng>
	constexpr auto enable_borrowed_range<tc::subrange<Rng>> = tc::borrowed_range<Rng>;

	//-------------------------------------------------------------------------------------------------------------------------
	// subrange creation

	template<tc::range_with_iterators Rng>
	[[nodiscard]] constexpr auto all(Rng&& rng) return_ctor_MAYTHROW( tc::make_subrange_result_t< Rng >, (std::forward<Rng>(rng)) )

	// slice from range + iterator pair
	// slice from range + difference
	template< typename Rng, typename Begin, typename End >
	[[nodiscard]] constexpr auto slice(Rng&& rng, Begin&& begin, End&& end) return_ctor_MAYTHROW(
		tc::make_subrange_result_t< Rng >,
		(std::forward<Rng>(rng), std::forward<Begin>(begin), std::forward<End>(end))
	)

	template< typename It >
	[[nodiscard]] constexpr auto make_iterator_range( It itBegin, It itEnd ) return_ctor_MAYTHROW(
		tc::subrange<tc::universal_range<It>>,
		( tc::universal_range<It>(), tc_move(itBegin), tc_move(itEnd) )
	)

	namespace make_iterator_range_detail {
		template< typename IndexRange, bool bConst >
		[[nodiscard]] constexpr decltype(auto) get_range( tc::index_iterator<IndexRange, bConst> const& itBegin, tc::index_iterator<IndexRange, bConst> const& itEnd ) noexcept {
			if constexpr (!tc::empty_type<IndexRange>) _ASSERTEQUAL(std::addressof(itBegin.get_range()), std::addressof(itEnd.get_range()));
			return itBegin.get_range();
		}
	}

	template< typename IndexRange, bool bConst >
	[[nodiscard]] constexpr auto make_iterator_range( tc::index_iterator<IndexRange, bConst> itBegin, tc::index_iterator<IndexRange, bConst> itEnd ) return_ctor_MAYTHROW(
		TC_FWD(tc::make_subrange_result_t< tc::conditional_const_t<IndexRange,bConst> & >),
		( make_iterator_range_detail::get_range(itBegin, itEnd), tc_move(itBegin).get_index(), tc_move(itEnd).get_index() )
	) 

	template <typename It>
	using iterator_range = decltype(tc::make_iterator_range(std::declval<It>(), std::declval<It>()));
	template <typename Rng>
		requires tc::safely_convertible_to<Rng&&, tc::iterator_range<tc::iterator_t<Rng>>>
	using iterator_range_t = tc::iterator_range<tc::iterator_t<Rng>>;

	template< typename T >
	[[nodiscard]] constexpr auto make_empty_range() noexcept {
		return tc::make_iterator_range( std::add_pointer_t<T>(), std::add_pointer_t<T>() );
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// make counted range

	template< typename It, typename Count >
	[[nodiscard]] constexpr auto counted( It const& it, Count&& count ) noexcept {
		return tc::make_iterator_range( it, it+std::forward<Count>(count) );
	}

	template< tc::char_type T, std::size_t N >
	[[nodiscard]] auto as_array(T (&at)[N] ) return_decltype_noexcept(
		tc::counted( std::addressof(at[0]), N )
	)

	//-------------------------------------------------------------------------------------------------------------------------
	// take/drop
	namespace detail {
		template<typename Cont, typename It>
		concept has_mem_fn_erase_from_begin = requires { std::declval<Cont&>().erase(tc::begin(std::declval<Cont&>()),std::declval<It&&>()); };

		template<typename Cont, typename It>
		concept has_mem_fn_take_inplace = requires { std::declval<Cont&>().take_inplace(std::declval<It&&>()); };

		template<typename Cont, typename It>
		concept has_mem_fn_drop_inplace = requires { std::declval<Cont&>().drop_inplace(std::declval<It&&>()); };
	}

	template< typename Cont, typename It >
	constexpr void take_inplace( Cont& cont, It&& it ) noexcept {
		if constexpr( detail::has_mem_fn_take_inplace<Cont,It> ) {
			cont.take_inplace(std::forward<It>(it));
		} else if constexpr( (tc::instance<Cont, std::vector> || tc::instance<Cont, std::deque>) && !std::is_move_assignable<tc::range_value_t<Cont&>>::value ) {
			if (tc::begin(cont) == it) {
				cont.clear();
			} else {
				--it;
				while (tc_modified(tc::end(cont), --_) != it) {
					cont.pop_back();
				}
			}
		} else {
			cont.erase(std::forward<It>(it),tc::end(cont));
		}
	}

	template< typename Rng, typename End >
	[[nodiscard]] constexpr auto take(Rng&& rng, End&& end) return_ctor_NOEXCEPT( // boost::iterator_range doesn't have a noexcept constructor
		tc::make_subrange_result_t< Rng >,
		(std::forward<Rng>(rng), tc::begin(rng), std::forward<End>(end))
	)

	template< typename C, typename T, typename A, typename It >
	[[nodiscard]] std::basic_string<C,T,A> && take( std::basic_string<C,T,A>&& rng, It&& it ) noexcept {
		tc::take_inplace(rng,std::forward<It>(it));
		return tc_move(rng);
	}
#if 0
	// TODO: pending proper fix of chained calls in partition_range.h
	template< typename Rng, typename It >
	[[nodiscard]] constexpr subrange<Rng>&& take( subrange<Rng>&& rng, It&& it ) noexcept {
		rng.take_inplace(std::forward<It>(it));
		return tc_move(rng);
	}
#endif

	template< typename Cont, typename It> requires detail::has_mem_fn_erase_from_begin<Cont,It>
	constexpr void drop_inplace( Cont & cont, It&& it ) noexcept {
		cont.erase(tc::begin(cont),std::forward<It>(it));
	}

	template< typename Cont, typename It> requires detail::has_mem_fn_drop_inplace<Cont,It>
	constexpr void drop_inplace( Cont & cont, It&& it ) noexcept {
		cont.drop_inplace(std::forward<It>(it));
	}

	template< tc::char_ptr CharPtr, typename It>
	constexpr void drop_inplace( CharPtr& pch, It&& it ) noexcept {
		pch=std::forward<It>(it);
	}

	namespace detail {
		template< typename Rng, typename It>
		constexpr tc::make_subrange_result_t< Rng > drop_impl( Rng&& rng, It&& itBegin ) noexcept {
			return tc::make_subrange_result_t< Rng >( std::forward<Rng>(rng), std::forward<It>(itBegin), tc::end(rng) );
		}

		// C strings have efficient in-place drop
		template< typename CharPtr, typename It> requires tc::char_ptr<std::remove_reference_t<CharPtr>>
		constexpr std::decay_t<CharPtr> drop_impl( CharPtr&& pch, It&& it ) noexcept {
			return tc_modified(pch, tc::drop_inplace(_, std::forward<It>(it)));
		}
	}

	template< typename Rng, typename It >
	[[nodiscard]] constexpr auto drop(Rng&& rng, It&& it) return_decltype_NOEXCEPT(
		detail::drop_impl( std::forward<Rng>(rng), std::forward<It>(it) )
	)

#if 0
	// TODO: pending proper fix of chained calls in partition_range.h
	template< typename Rng, typename It >
	[[nodiscard]] constexpr subrange<Rng>&& drop( subrange<Rng>&& rng, It&& it ) noexcept {
		rng.drop_inplace(std::forward<It>(it));
		return tc_move(rng);
	}
#endif

	////////////////////////////////
	// begin_next/end_prev

	namespace begin_next_detail {
		template< typename RangeReturn, bool bLinear, typename Rng >
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
			auto const itBound=tc::end(rng);
			while (0<n) {
				if(it==itBound) {
					return RangeReturn::pack_no_border(std::forward<Rng>(rng), tc_move(it));
				}
				--n;
				++it;
			}
			return RangeReturn::pack_border(tc_move(it), std::forward<Rng>(rng));
		}

		template< typename RangeReturn, bool bLinear, typename Rng >
		constexpr auto begin_next(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::random_access_traversal_tag
		) noexcept {
			_ASSERTDEBUG(0<=n);
			if constexpr( tc::common_range<Rng> ) {
				if(n<=tc::size_raw(rng)) {
					return RangeReturn::pack_border(tc::begin(rng)+n, std::forward<Rng>(rng));
				} else {
					return RangeReturn::pack_no_border(std::forward<Rng>(rng));
				}
			} else if constexpr( RangeReturn::allowed_if_always_has_border ) {
#ifdef _DEBUG
				return begin_next_detail::begin_next<RangeReturn, /*bLinear*/true>(std::forward<Rng>(rng), n, boost::iterators::forward_traversal_tag());
#else
				return RangeReturn::pack_border(tc::begin(rng)+n, std::forward<Rng>(rng));
#endif
			} else {
				return begin_next_detail::begin_next<RangeReturn, bLinear>(std::forward<Rng>(rng), n, boost::iterators::forward_traversal_tag());
			}
		}
	}

	template< typename RangeReturn, tc::range_with_iterators Rng>
	[[nodiscard]] constexpr auto begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return begin_next_detail::begin_next<RangeReturn, /*bLinear*/false>(std::forward<Rng>(rng), n, typename boost::range_traversal<Rng>::type());
	}

	template< typename RangeReturn, tc::range_with_iterators Rng>
	[[nodiscard]] auto linear_begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return begin_next_detail::begin_next<RangeReturn, /*bLinear*/true>(std::forward<Rng>(rng), n, typename boost::range_traversal<Rng>::type());
	}

	namespace end_prev_detail {
		template< typename RangeReturn, typename Rng >
		constexpr auto end_prev(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::bidirectional_traversal_tag
		) noexcept {
			_ASSERT(0 <= n);
			_ASSERTNOTIFY(n <= 2);
			auto it=tc::end(rng);
			auto const itBound=tc::begin(rng);
			while (0<n) {
				if(it==itBound) {
					return RangeReturn::pack_no_border(std::forward<Rng>(rng));
				}
				--n;
				--it;
			}
			return RangeReturn::pack_border(it, std::forward<Rng>(rng));
		}

		template< typename RangeReturn, typename Rng >
		constexpr auto end_prev(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::random_access_traversal_tag
		) noexcept {
			_ASSERTDEBUG(0 <= n);
			if(n<=tc::size_raw(rng)) {
				return RangeReturn::pack_border(tc::end(rng)-n, std::forward<Rng>(rng));
			} else {
				return RangeReturn::pack_no_border(std::forward<Rng>(rng));
			}
		}
	}

	template< typename RangeReturn, typename Rng >
	[[nodiscard]] constexpr auto end_prev(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return end_prev_detail::end_prev<RangeReturn>(std::forward<Rng>(rng), n, typename boost::range_traversal<Rng>::type());
	}

	////////////////////////////////////////////////////////////////////////////////////
	// take/drop_*_first/last_n

	template< typename Cont >
	void take_first_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) noexcept {
		tc::take_inplace(cont, tc::begin_next<tc::return_border>(cont,n));
	}

	namespace take_first_detail {
		TC_DEFINE_ENUM(ETakePred, etakepred,
			(TAKEANDCONTINUE)
			(TAKEANDBREAK)
			(DONTTAKE)
		)
		namespace no_adl {
			template< typename Sink, typename TakePred >
			struct take_first_sink /*final*/ {
				static_assert(tc::decayed<Sink>);
				static_assert(tc::decayed<TakePred>);

				template<typename Sink2>
				take_first_sink(Sink2&& sink, TakePred& takepred, tc::break_or_continue& boc) noexcept
					: m_sink(std::forward<Sink2>(sink))
					, m_takepred(takepred)
					, m_boc(boc)
				{}

				template< typename T >
				auto operator()(T&& t) const& MAYTHROW -> tc::common_type_t<
					decltype(tc::continue_if_not_break(std::declval<Sink const&>(), std::declval<T>())),
					tc::constant<tc::break_>
				> {
					auto const Take=[&]() MAYTHROW { return tc::continue_if_not_break(m_sink, std::forward<T>(t)); };
					switch_no_default(m_takepred.take(t)) {
						case etakepredTAKEANDCONTINUE: {
							auto boc=Take(); // MAYTHROW
							m_boc=boc;
							return boc; // allow return type to be tc::constant<tc::break_> if possible
						}
						case etakepredTAKEANDBREAK:
							m_boc=Take(); // MAYTHROW
							return tc::constant<tc::break_>();
						case etakepredDONTTAKE:
							m_boc=tc::continue_;
							return tc::constant<tc::break_>();
					}
				}

				template< typename Rng >
				auto chunk(Rng&& rng) const& MAYTHROW -> tc::common_type_t<
					decltype(tc::continue_if_not_break(tc::mem_fn_chunk(), std::declval<Sink const&>(), tc::take(std::declval<Rng>(), tc::begin(std::declval<Rng&>())))),
					tc::constant<tc::break_>
				> {
					tc_auto_cref(pairitetakepred, m_takepred.take_range(rng)); // MAYTHROW
					auto boc=tc::continue_if_not_break(tc::mem_fn_chunk(), m_sink, tc::take(std::forward<Rng>(rng), pairitetakepred.first)); // MAYTHROW
					m_boc=boc;
					switch_no_default(pairitetakepred.second) {
						case etakepredTAKEANDCONTINUE: return boc;
						case etakepredTAKEANDBREAK: return tc::constant<tc::break_>();
					}
				}

			private:
				Sink m_sink;
				TakePred& m_takepred;
				tc::break_or_continue& m_boc;
			};

			struct take_first_pred {
				explicit take_first_pred(std::size_t nCount) noexcept
					: m_nCount(VERIFYPRED(nCount, 0<_))
				{}

				ETakePred take(tc::unused) & noexcept {
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
		auto take_first_impl(Rng&& rng, std::size_t const n) noexcept {
			static_assert(!tc::range_with_iterators<Rng>);
			return [rng=tc::make_reference_or_value(std::forward<Rng>(rng)), n](auto&& sink) MAYTHROW {
				if(0<n) {
					TakePred takepred(n);
					tc::break_or_continue boc;
					if(tc::break_==tc::for_each(*rng, no_adl::take_first_sink<tc::decay_t<decltype(sink)>, TakePred>(tc_move_if_owned(sink), takepred, boc))) { // MAYTHROW
						return VERIFYINITIALIZED(boc);
					} else { // boc won't be initialized if rng is empty
						_ASSERT(bTruncate);
					}
				}
				return tc::continue_;
			};
		}
	}

	template< typename RangeReturn, typename Rng, std::enable_if_t<!tc::range_with_iterators<Rng> && std::is_same<RangeReturn, tc::return_take>::value>* = nullptr >
	[[nodiscard]] auto begin_next(Rng&& rng, std::size_t n=1) return_decltype_noexcept(
		tc::take_first_detail::take_first_impl<take_first_detail::no_adl::take_first_pred, /*bTruncate*/ false>(std::forward<Rng>(rng), n)
	)

	template< typename Cont >
	void drop_first_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n) noexcept {
		tc::drop_inplace(cont, tc::begin_next<tc::return_border>(cont, n));
	}

	template< typename Cont >
	void drop_first_inplace(Cont& cont) noexcept {
		if constexpr( has_mem_fn_pop_front<Cont> ) {
			cont.pop_front();
		} else {
			tc::drop_inplace(cont, tc::begin_next<tc::return_border>(cont));
		}
	}

	namespace no_adl {
		template< typename Sink >
		struct drop_first_sink /*final*/ {
			static_assert(tc::decayed<Sink>);

			template<typename Sink2>
			drop_first_sink(Sink2&& sink, std::size_t& nCount) noexcept
				: m_sink(std::forward<Sink2>(sink))
				, m_nCount(nCount)
			{}

			template< typename T >
			auto operator()(T&& t) const& MAYTHROW -> tc::common_type_t<
				tc::constant<tc::continue_>,
				decltype(tc::continue_if_not_break(std::declval<Sink const&>(), std::declval<T>()))
			> {
				if(0<m_nCount) {
					--m_nCount;
					return tc::constant<tc::continue_>();
				} else {
					return tc::continue_if_not_break(m_sink, std::forward<T>(t)); // MAYTHROW
				}
			}

			template< typename Rng >
			auto chunk(Rng&& rng) const& MAYTHROW -> tc::common_type_t<
				tc::constant<tc::continue_>,
				decltype(tc::continue_if_not_break(tc::mem_fn_chunk(), std::declval<Sink const&>(), tc::drop(std::declval<Rng>(), tc::begin(std::declval<Rng&>()))))
			> {
				auto it=tc::begin(rng);
				m_nCount-=tc::advance_forward_bounded(it, m_nCount, tc::end(rng));
				if(0<m_nCount) {
					_ASSERT(tc::end(rng)==it);
					return tc::constant<tc::continue_>();
				} else {
					return tc::continue_if_not_break(tc::mem_fn_chunk(), m_sink, tc::drop(std::forward<Rng>(rng), tc_move(it))); // MAYTHROW
				}
			}

		private:
			Sink m_sink;
			std::size_t& m_nCount;
		};
	}

	template< typename RangeReturn, typename Rng> requires (!tc::range_with_iterators<Rng>) && std::is_same<RangeReturn, tc::return_drop>::value
	[[nodiscard]] auto begin_next(Rng&& rng, std::size_t const n=1) noexcept {
		return [rng=tc::make_reference_or_value(std::forward<Rng>(rng)), n](auto&& sink) MAYTHROW {
			auto nCount=n;
			auto boc=tc::for_each(*rng, no_adl::drop_first_sink<tc::decay_t<decltype(sink)>>(tc_move_if_owned(sink), nCount)); // MAYTHROW
			_ASSERTEQUAL(nCount, 0);
			return boc;
		};
	}

	template< typename Cont >
	void take_last_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n=1) noexcept {
		tc::drop_inplace(cont, tc::end_prev<tc::return_border>(cont, n));
	}

	template< typename Cont >
	void drop_last_inplace(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n) noexcept {
		tc::take_inplace(cont, tc::end_prev<tc::return_border>(cont, n));
	}

	template< typename Cont>
	void drop_last_inplace(Cont& cont) noexcept {
		if constexpr( has_mem_fn_pop_back<Cont> ) {
			cont.pop_back();
		} else {
			tc::take_inplace(cont, tc::end_prev<tc::return_border>(cont));
		}
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// take_first_truncate...

	template< typename Cont >
	void take_first_truncate_inplace( Cont& rng, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
		auto it=tc::begin(rng);
		tc::advance_forward_bounded( it, n, tc::end(rng) );
		tc::take_inplace( rng, tc_move(it) );
	}


	//-------------------------------------------------------------------------------------------------------------------------
	// span
	template <typename T>
	using span = subrange<universal_range<T*>>;

	namespace span_detail {
		template <typename Rng>
		using unchecked_span_t = span<std::remove_pointer_t<tc::ptr_iterator_t<Rng>>>;
	}

	template <tc::contiguous_range Rng>
		requires tc::safely_convertible_to<Rng&&, span_detail::unchecked_span_t<Rng>>
	using span_t = span_detail::unchecked_span_t<Rng>;

	// get a consecutive block of memory from range and return an iterator_range of pointers
	template<tc::contiguous_range Rng>
	[[nodiscard]] constexpr auto as_span(Rng&& rng) noexcept 
		-> tc::span_t<Rng&&>
	{
		return std::forward<Rng>(rng);
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// range common reference
	namespace no_adl {
		template <typename It, typename TSource>
		struct is_class_safely_constructible<tc::subrange<tc::universal_range<It>>, TSource> final
			: tc::constant<
				std::is_same<tc::decay_t<TSource>, tc::empty_range>::value ||
				tc::borrowed_range<TSource>
			>
		{};

		template <typename Rng1, typename Rng2>
		struct common_iterator_range_impl {};

		// Rng have common iterator
		template <typename Rng1, typename Rng2>
			requires tc::safely_constructible_from<tc::iterator_range<tc::common_type_t<tc::iterator_t<Rng1>, tc::iterator_t<Rng2>>>, Rng1>
				&& tc::safely_constructible_from<tc::iterator_range<tc::common_type_t<tc::iterator_t<Rng1>, tc::iterator_t<Rng2>>>, Rng2>
		struct common_iterator_range_impl<Rng1, Rng2> {
			using type = tc::iterator_range<tc::common_type_t<tc::iterator_t<Rng1>, tc::iterator_t<Rng2>>>;
		};
		// Rng have common pointer iterator
		template <typename Rng1, typename Rng2>
			requires (!requires { typename tc::common_type_t<tc::iterator_t<Rng1>, tc::iterator_t<Rng2>>; })
				&& tc::safely_constructible_from<tc::iterator_range<tc::common_type_t<tc::ptr_iterator_t<Rng1>, tc::ptr_iterator_t<Rng2>>>, Rng1>
				&& tc::safely_constructible_from<tc::iterator_range<tc::common_type_t<tc::ptr_iterator_t<Rng1>, tc::ptr_iterator_t<Rng2>>>, Rng2>
		struct common_iterator_range_impl<Rng1, Rng2> {
			using type = tc::iterator_range<tc::common_type_t<tc::ptr_iterator_t<Rng1>, tc::ptr_iterator_t<Rng2>>>;
		};

		// special case: empty_range
		template <typename Rng>
			requires requires { typename tc::iterator_range_t<Rng>; }
		struct common_iterator_range_impl<Rng, tc::empty_range> {
			using type = tc::iterator_range_t<Rng>;
		};
		template <typename Rng>
			requires requires { typename tc::iterator_range_t<Rng>; }
		struct common_iterator_range_impl<tc::empty_range, Rng> {
			using type = tc::iterator_range_t<Rng>;
		};

		template <typename Rng1, typename Rng2>
		using common_iterator_range_impl_t = typename common_iterator_range_impl<Rng1, Rng2>::type;
		template <typename... T>
		using common_iterator_range_t = tc::type::accumulate_with_front_t<tc::type::list<T...>, common_iterator_range_impl_t>;

		// If the Ts have a common iterator range, that is their common reference.
		template <typename ... T>
			requires (!tc::has_actual_common_reference<T...>) && requires { typename common_iterator_range_t<T...>; }
		struct common_reference_xvalue_as_ref<T...> final {
			using type = common_iterator_range_t<T...>;
		};
	}
}
