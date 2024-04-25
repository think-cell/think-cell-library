
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/move.h"
#include "../base/trivial_functors.h"
#include "../base/modified.h"
#include "../base/invoke.h"
#include "../container/container_traits.h"
#include "../algorithm/size.h"
#include "../algorithm/size_linear.h"
#include "../algorithm/size_bounded.h"
#include "../base/change.h"
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
		template<typename It>
		struct universal_range;
		template<typename Rng>
		struct subrange;
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// ptr_iterator_t/ptr_begin/ptr_end
#if !defined(_MSC_VER)
	// On libc++ and libstdc++, std::pointer_trait is broken.
	// E.g. on libc++, it is missing a specialization for std::__wrap_iter (aka std::vector::iterator).
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
	
	//-------------------------------------------------------------------------------------------------------------------------
	// subrange

	namespace subrange_detail {
		template<typename Rng>
		static constexpr decltype(auto) whole_range(Rng&& rng) noexcept {
			if constexpr( tc::instance<std::remove_reference_t<Rng>, no_adl::subrange> ) {
				return tc_move_if_owned(rng).base_range();
			} else {
				return tc_move_if_owned(rng);
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
			struct get_index<tc::no_adl::universal_range<It>> {
				template <typename Rhs>
				static constexpr auto begin(Rhs& rng) return_decltype_MAYTHROW(tc::begin(rng));
				template <typename Rhs>
				static constexpr auto begin(tc::no_adl::universal_range<It>, Rhs& rng) return_decltype_MAYTHROW(tc::begin(rng));

				template <typename Rhs>
				static constexpr auto end(Rhs& rng) return_decltype_MAYTHROW(tc::end(rng));
				template <typename Rhs>
				static constexpr auto end(tc::no_adl::universal_range<It>, Rhs& rng) return_decltype_MAYTHROW(tc::end(rng));
			};

			template <typename T>
			struct get_index<tc::no_adl::universal_range<T*>> {
				template <typename Rhs>
				static constexpr auto begin(Rhs& rng) return_decltype_MAYTHROW(tc::ptr_begin(rng));
				template <typename Rhs>
				static constexpr auto begin(tc::no_adl::universal_range<T*>, Rhs& rng) return_decltype_MAYTHROW(tc::ptr_begin(rng));

				template <typename Rhs>
				static constexpr auto end(Rhs& rng) return_decltype_MAYTHROW(tc::ptr_end(rng));
				template <typename Rhs>
				static constexpr auto end(tc::no_adl::universal_range<T*>, Rhs& rng) return_decltype_MAYTHROW(tc::ptr_end(rng));
			};
		}
		using no_adl::get_index;
	}

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
				static_assert(tc::dependent_false<It_>::value, "universal_range::begin() does not exist");
				return std::declval<It>();
			}
			template <typename It_ = It>
			constexpr It end() const& noexcept {
				static_assert(tc::dependent_false<It_>::value, "universal_range::end() does not exist");
				return std::declval<It>();
			}
		};
	}

	template <typename It>
	constexpr auto enable_borrowed_range<no_adl::universal_range<It>> = true;

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
			static_assert(!tc::instance<Rng, no_adl::subrange>, "Don't write the type directly.");

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
				(!tc::instance<std::remove_cvref_t<Rhs>, no_adl::subrange>) && (!tc::instance<std::remove_cvref_t<Rhs>, no_adl::universal_range>) && tc::safely_constructible_from<Rng, Rhs>
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
				tc::instance<std::remove_cvref_t<Rhs>, no_adl::subrange> && tc::safely_constructible_from<Rng, subrange_detail::whole_range_t<Rhs>> &&
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
				m_idxEnd=tc::iterator2index<Rng>( tc_move_if_owned(it) );
			}

			template< typename It >
			constexpr void drop_inplace( It&& it ) & noexcept {
				m_idxBegin=tc::iterator2index<Rng>( tc_move_if_owned(it) );
			}
		};
	}

	template<typename Rng>
	constexpr auto enable_stable_index_on_move<no_adl::subrange<Rng>> = tc::stable_index_on_move<Rng>;
	template <typename Rng>
	constexpr auto enable_borrowed_range<no_adl::subrange<Rng>> = tc::borrowed_range<Rng>;

	template<typename Rng>
	using subrange_arg_t = tc::mp_only<typename tc::is_instance<std::remove_reference_t<Rng>, no_adl::subrange>::arguments>;

	//-------------------------------------------------------------------------------------------------------------------------
	// subrange creation
	template<tc::range_with_iterators Rng, typename Begin, typename End>
	[[nodiscard]] constexpr auto slice(Rng&& rng, Begin&& begin, End&& end) noexcept {
		// If we have a common borrowed range, we don't need to store rng.
		// However, if we have index iteratores, storing two index_iterators is worse then storing the reference + two indices.
		if constexpr (tc::borrowed_range<Rng> && tc::common_range<Rng> && !tc::is_index_iterator<tc::iterator_t<Rng>>) {
			return no_adl::subrange<no_adl::universal_range<tc::iterator_t<Rng>>>(tc_move_if_owned(rng), tc_move_if_owned(begin), tc_move_if_owned(end));
		} else {
			return no_adl::subrange<subrange_detail::whole_range_t<Rng>>(tc_move_if_owned(rng), tc_move_if_owned(begin), tc_move_if_owned(end));
		}
	}
	template<typename Rng>
	using slice_t = decltype(slice(std::declval<Rng>(), std::declval<tc::iterator_t<Rng>>(), std::declval<tc::iterator_t<Rng>>()));

	template<tc::range_with_iterators Rng>
	[[nodiscard]] constexpr auto all(Rng&& rng) return_ctor_MAYTHROW(tc::slice_t<Rng>, (tc_move_if_owned(rng)))

	template<typename It>
	[[nodiscard]] constexpr auto make_iterator_range(It itBegin, It itEnd) noexcept {
		if constexpr (tc::is_index_iterator<It>) {
			if constexpr (!tc::empty_type<std::remove_reference_t<decltype(itBegin.get_range())>>) {
				_ASSERTEQUAL(std::addressof(itBegin.get_range()), std::addressof(itEnd.get_range()));
			}
			return slice(itBegin.get_range(), tc_move(itBegin).get_index(), tc_move(itEnd).get_index());
		} else {
			return no_adl::subrange<no_adl::universal_range<It>>(no_adl::universal_range<It>(), tc_move(itBegin), tc_move(itEnd));
		}
	}

	template<typename It>
	using iterator_range = decltype(tc::make_iterator_range(std::declval<It>(), std::declval<It>()));
	template<typename Rng>
		requires tc::safely_convertible_to<Rng&&, tc::iterator_range<tc::iterator_t<Rng>>>
	using iterator_range_t = tc::iterator_range<tc::iterator_t<Rng>>;

	template<typename T>
	[[nodiscard]] constexpr auto make_empty_range() noexcept {
		return tc::make_iterator_range(std::add_pointer_t<T>(), std::add_pointer_t<T>());
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// make counted range

	template< typename It, typename Count >
	[[nodiscard]] constexpr auto counted( It const& it, Count&& count ) noexcept {
		return tc::make_iterator_range( it, it+tc_move_if_owned(count) );
	}

	template< tc::char_type T, std::size_t N >
	[[nodiscard]] constexpr auto as_array(T (&at)[N] ) return_decltype_noexcept(
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
			cont.take_inplace(tc_move_if_owned(it));
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
			cont.erase(tc_move_if_owned(it),tc::end(cont));
		}
	}

	template< typename Rng, typename End >
	[[nodiscard]] constexpr auto take(Rng&& rng, End&& end) return_ctor_NOEXCEPT( // boost::iterator_range doesn't have a noexcept constructor
		tc::slice_t<Rng>,
		(tc_move_if_owned(rng), tc::begin(rng), tc_move_if_owned(end))
	)

	template< typename C, typename T, typename A, typename It >
	[[nodiscard]] std::basic_string<C,T,A> && take( std::basic_string<C,T,A>&& rng, It&& it ) noexcept {
		tc::take_inplace(rng,tc_move_if_owned(it));
		return tc_move(rng);
	}
	template< typename Rng, typename It >
	[[nodiscard]] constexpr no_adl::subrange<Rng>&& take( no_adl::subrange<Rng>&& rng, It&& it ) noexcept {
		rng.take_inplace(tc_move_if_owned(it));
		return tc_move(rng);
	}

	template< typename Cont, typename It> requires detail::has_mem_fn_erase_from_begin<Cont,It>
	constexpr void drop_inplace( Cont & cont, It&& it ) noexcept {
		cont.erase(tc::begin(cont),tc_move_if_owned(it));
	}

	template< typename Cont, typename It> requires detail::has_mem_fn_drop_inplace<Cont,It>
	constexpr void drop_inplace( Cont & cont, It&& it ) noexcept {
		cont.drop_inplace(tc_move_if_owned(it));
	}

	template< tc::char_ptr CharPtr, typename It>
	constexpr void drop_inplace( CharPtr& pch, It&& it ) noexcept {
		pch=tc_move_if_owned(it);
	}

	namespace detail {
		template< typename Rng, typename It>
		constexpr auto drop_impl( Rng&& rng, It&& itBegin ) noexcept {
			return tc::slice_t<Rng>( tc_move_if_owned(rng), tc_move_if_owned(itBegin), tc::end(rng) );
		}

		// C strings have efficient in-place drop
		template< typename CharPtr, typename It> requires tc::char_ptr<std::remove_reference_t<CharPtr>>
		constexpr std::decay_t<CharPtr> drop_impl( CharPtr&& pch, It&& it ) noexcept {
			return tc_modified(pch, tc::drop_inplace(_, tc_move_if_owned(it)));
		}
	}

	template< typename Rng, typename It >
	[[nodiscard]] constexpr auto drop(Rng&& rng, It&& it) return_decltype_NOEXCEPT(
		detail::drop_impl( tc_move_if_owned(rng), tc_move_if_owned(it) )
	)
	template< typename Rng, typename It >
	[[nodiscard]] constexpr no_adl::subrange<Rng>&& drop( no_adl::subrange<Rng>&& rng, It&& it ) noexcept {
		rng.drop_inplace(tc_move_if_owned(it));
		return tc_move(rng);
	}

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
					return RangeReturn::pack_no_border(tc_move_if_owned(rng), tc_move(it));
				}
				--n;
				++it;
			}
			return RangeReturn::pack_border(tc_move(it), tc_move_if_owned(rng));
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
					return RangeReturn::pack_border(tc::begin(rng)+n, tc_move_if_owned(rng));
				} else {
					return RangeReturn::pack_no_border(tc_move_if_owned(rng));
				}
			} else if constexpr( RangeReturn::allowed_if_always_has_border ) {
#ifdef _DEBUG
				return begin_next_detail::begin_next<RangeReturn, /*bLinear*/true>(tc_move_if_owned(rng), n, boost::iterators::forward_traversal_tag());
#else
				return RangeReturn::pack_border(tc::begin(rng)+n, tc_move_if_owned(rng));
#endif
			} else {
				return begin_next_detail::begin_next<RangeReturn, bLinear>(tc_move_if_owned(rng), n, boost::iterators::forward_traversal_tag());
			}
		}
	}

	template< typename RangeReturn, tc::range_with_iterators Rng>
	[[nodiscard]] constexpr auto begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return begin_next_detail::begin_next<RangeReturn, /*bLinear*/false>(tc_move_if_owned(rng), n, typename boost::range_traversal<Rng>::type());
	}

	template< typename RangeReturn, tc::range_with_iterators Rng>
	[[nodiscard]] auto linear_begin_next(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return begin_next_detail::begin_next<RangeReturn, /*bLinear*/true>(tc_move_if_owned(rng), n, typename boost::range_traversal<Rng>::type());
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
					return RangeReturn::pack_no_border(tc_move_if_owned(rng));
				}
				--n;
				--it;
			}
			return RangeReturn::pack_border(it, tc_move_if_owned(rng));
		}

		template< typename RangeReturn, typename Rng >
		constexpr auto end_prev(
			Rng&& rng,
			typename boost::range_size< std::remove_reference_t<Rng> >::type n,
			boost::iterators::random_access_traversal_tag
		) noexcept {
			_ASSERTDEBUG(0 <= n);
			if(n<=tc::size_raw(rng)) {
				return RangeReturn::pack_border(tc::end(rng)-n, tc_move_if_owned(rng));
			} else {
				return RangeReturn::pack_no_border(tc_move_if_owned(rng));
			}
		}
	}

	template< typename RangeReturn, typename Rng >
	[[nodiscard]] constexpr auto end_prev(
		Rng&& rng,
		typename boost::range_size< std::remove_reference_t<Rng> >::type n=1
	) noexcept {
		return end_prev_detail::end_prev<RangeReturn>(tc_move_if_owned(rng), n, typename boost::range_traversal<Rng>::type());
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
					: m_sink(tc_move_if_owned(sink))
					, m_takepred(takepred)
					, m_boc(boc)
				{}

				template< typename T >
				auto operator()(T&& t) const& MAYTHROW -> tc::common_type_t<
					decltype(tc::continue_if_not_break(std::declval<Sink const&>(), std::declval<T>())),
					tc::constant<tc::break_>
				> {
					auto const Take=[&]() MAYTHROW { return tc::continue_if_not_break(m_sink, tc_move_if_owned(t)); };
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
					auto boc=tc::continue_if_not_break(tc::mem_fn_chunk(), m_sink, tc::take(tc_move_if_owned(rng), pairitetakepred.first)); // MAYTHROW
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
	}

	namespace take_first_adaptor_adl {
		template< typename TakePred, bool bTruncate, typename Rng >
		struct take_first_adaptor : tc::range_adaptor_base_range<Rng>, tc::range_output_from_base_range {
			take_first_adaptor(Rng&& rng, std::size_t const n) noexcept
				: take_first_adaptor::range_adaptor_base_range(aggregate_tag, tc_move_if_owned(rng)), m_n(n)
			{
				static_assert(!tc::range_with_iterators<Rng>);
			}

			template<tc::decayed_derived_from<take_first_adaptor> Self, typename Sink>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				if(0<self.m_n) {
					TakePred takepred(self.m_n);
					tc::break_or_continue boc;
					if(tc::break_==tc::for_each(
						tc_move_if_owned(self).base_range(),
						tc::take_first_detail::no_adl::take_first_sink<tc::decay_t<decltype(sink)>, TakePred>(tc_move_if_owned(sink), takepred, boc)
					)) { // MAYTHROW
						return VERIFYINITIALIZED(boc);
					} else { // boc won't be initialized if rng is empty
						_ASSERT(bTruncate);
					}
				}
				return tc::continue_;
			}
		private:
			std::size_t m_n;
		};
	}

	template< std::same_as<tc::return_take> RangeReturn, typename Rng> requires (!tc::range_with_iterators<Rng>)
	[[nodiscard]] auto begin_next(Rng&& rng, std::size_t n=1) noexcept {
		return tc::take_first_adaptor_adl::take_first_adaptor<take_first_detail::no_adl::take_first_pred, /*bTruncate*/ false, Rng>(tc_move_if_owned(rng), n);
	}

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
				: m_sink(tc_move_if_owned(sink))
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
					return tc::continue_if_not_break(m_sink, tc_move_if_owned(t)); // MAYTHROW
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
					return tc::continue_if_not_break(tc::mem_fn_chunk(), m_sink, tc::drop(tc_move_if_owned(rng), tc_move(it))); // MAYTHROW
				}
			}

		private:
			Sink m_sink;
			std::size_t& m_nCount;
		};
	}

	namespace drop_first_adaptor_adl {
		template<typename Rng>
		struct drop_first_adaptor : tc::range_adaptor_base_range<Rng>, tc::range_output_from_base_range {
			drop_first_adaptor(Rng&& rng, std::size_t const n) noexcept
				: drop_first_adaptor::range_adaptor_base_range(aggregate_tag, tc_move_if_owned(rng)), m_n(n) {}

			template<tc::decayed_derived_from<drop_first_adaptor> Self, typename Sink>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				auto nCount=self.m_n;
				auto boc=tc::for_each(tc_move_if_owned(self).base_range(), no_adl::drop_first_sink<tc::decay_t<decltype(sink)>>(tc_move_if_owned(sink), nCount)); // MAYTHROW
				_ASSERTEQUAL(nCount, 0);
				return boc;
			}
		private:
			std::size_t m_n;
		};
	}

	template< std::same_as<tc::return_drop> RangeReturn, typename Rng> requires (!tc::range_with_iterators<Rng>)
	[[nodiscard]] auto begin_next(Rng&& rng, std::size_t const n=1) noexcept {
		return tc::drop_first_adaptor_adl::drop_first_adaptor<Rng>(tc_move_if_owned(rng), n);
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
	template<typename T>
	using span = no_adl::subrange<no_adl::universal_range<T*>>;

	namespace span_detail {
		template <typename Rng>
		using unchecked_span_t = span<std::remove_pointer_t<tc::ptr_iterator_t<Rng>>>;
	}

	template<tc::contiguous_range Rng>
		requires tc::safely_convertible_to<Rng&&, span_detail::unchecked_span_t<Rng>>
	using span_t = span_detail::unchecked_span_t<Rng>;

	// get a consecutive block of memory from range and return an iterator_range of pointers
	template<tc::contiguous_range Rng>
	[[nodiscard]] constexpr auto as_span(Rng&& rng) noexcept 
		-> tc::span_t<Rng&&>
	{
		return tc_move_if_owned(rng);
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// range common reference
	namespace no_adl {
		template <typename It, typename TSource>
		struct is_class_safely_constructible<subrange<universal_range<It>>, TSource> final
			: tc::constant<
				std::is_same<tc::decay_t<TSource>, tc::empty_range>::value ||
				tc::borrowed_range<TSource>
			>
		{};

		template <typename Rng1, typename Rng2>
		struct common_iterator_range {};

		// Rng have common iterator
		template <typename Rng1, typename Rng2>
			requires tc::safely_constructible_from<tc::iterator_range<tc::common_type_t<tc::iterator_t<Rng1>, tc::iterator_t<Rng2>>>, Rng1>
				&& tc::safely_constructible_from<tc::iterator_range<tc::common_type_t<tc::iterator_t<Rng1>, tc::iterator_t<Rng2>>>, Rng2>
		struct common_iterator_range<Rng1, Rng2> {
			using type = tc::iterator_range<tc::common_type_t<tc::iterator_t<Rng1>, tc::iterator_t<Rng2>>>;
		};
		// Rng have common pointer iterator
		template <typename Rng1, typename Rng2>
			requires (!requires { typename tc::common_type_t<tc::iterator_t<Rng1>, tc::iterator_t<Rng2>>; })
				&& tc::safely_constructible_from<tc::iterator_range<tc::common_type_t<tc::ptr_iterator_t<Rng1>, tc::ptr_iterator_t<Rng2>>>, Rng1>
				&& tc::safely_constructible_from<tc::iterator_range<tc::common_type_t<tc::ptr_iterator_t<Rng1>, tc::ptr_iterator_t<Rng2>>>, Rng2>
		struct common_iterator_range<Rng1, Rng2> {
			using type = tc::iterator_range<tc::common_type_t<tc::ptr_iterator_t<Rng1>, tc::ptr_iterator_t<Rng2>>>;
		};

		// special case: empty_range
		template <typename Rng, typename EmptyRng>
			requires requires { typename tc::iterator_range_t<Rng>; } && std::same_as<std::remove_cvref_t<EmptyRng>, tc::empty_range>
		struct common_iterator_range<Rng, EmptyRng> {
			using type = tc::iterator_range_t<Rng>;
		};
		template <typename Rng, typename EmptyRng>
			requires requires { typename tc::iterator_range_t<Rng>; } && std::same_as<std::remove_cvref_t<EmptyRng>, tc::empty_range>
		struct common_iterator_range<EmptyRng, Rng> {
			using type = tc::iterator_range_t<Rng>;
		};

		// If T0 and T1 have a common iterator range, that is their common reference.
		template <typename T0, typename T1> requires requires { typename common_iterator_range<T0, T1>::type; }
		struct common_reference_impl<T0, T1> : common_iterator_range<T0, T1> {};
	}
}
