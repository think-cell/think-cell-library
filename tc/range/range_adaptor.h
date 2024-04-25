
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "index_iterator.h"
#include "meta.h"
#include "../base/assert_defs.h"
#include "../base/casts.h"
#include "../base/static_polymorphism.h"
#include "../algorithm/for_each.h"

#include <boost/range/detail/demote_iterator_traversal_tag.hpp>
#include <boost/mpl/has_xxx.hpp>

#include <type_traits>

namespace tc {

	TC_HAS_MEM_FN_XXX_CONCEPT_DEF(end_index, const&);

	//////////////////////////////////////////////////////////
	// range adaptors
	//
	// Basic building block for all ranges.
	// Comes in two variations, one for generator ranges, one for iterator ranges. 
	//
	namespace no_adl {
		template <typename Derived, typename Index>
		struct TC_EMPTY_BASES index_range_interface {
		private:
			using this_type = index_range_interface;
		public:
			using tc_index = Index;

			STATIC_VIRTUAL(begin_index)
			STATIC_VIRTUAL(end_index)
			STATIC_VIRTUAL(increment_index)
			STATIC_VIRTUAL(decrement_index)
			STATIC_VIRTUAL(dereference_index)
			STATIC_VIRTUAL(advance_index)
			STATIC_VIRTUAL(index_to_address)
			STATIC_VIRTUAL(distance_to_index)
			STATIC_VIRTUAL(middle_point)

			STATIC_VIRTUAL_WITH_FALLBACK_MOD(
				TC_FWD(
					template<typename Derived_ = Derived> requires has_mem_fn_end_index<Derived_> && tc::is_equality_comparable<Index>::value
					constexpr
				),
			at_end_index)(tc_index const& idx) const&
				return_MAYTHROW(end_index() == idx)

		};
	}
	using no_adl::index_range_interface;

	namespace no_adl {
		// simulate iterator interface on top of index interface
		template <typename Derived, typename Index>
		struct TC_EMPTY_BASES range_iterator_from_index : index_range_interface<Derived, Index> {
		private:
			using iterator = index_iterator<Derived, false>;
			using const_iterator = index_iterator<Derived, true>;

		public:
			constexpr iterator make_iterator(auto&& idx) & noexcept {
				return iterator(*tc::derived_cast<Derived>(MSVC_WORKAROUND_THIS),tc_move_if_owned(idx));
			}
			constexpr const_iterator make_iterator(auto&& idx) const& noexcept {
				return const_iterator(*tc::derived_cast<Derived>(MSVC_WORKAROUND_THIS), tc_move_if_owned(idx));
			}

			constexpr iterator begin() &
				return_MAYTHROW(make_iterator(this->begin_index()))
			constexpr const_iterator begin() const&
				return_MAYTHROW(make_iterator(this->begin_index()))


			template<typename Derived_ = Derived>
				requires tc::has_mem_fn_end_index<Derived_> && tc::is_equality_comparable<Index>::value
			constexpr iterator end() &
				return_MAYTHROW(make_iterator(this->end_index()))
			template<typename Derived_ = Derived>
				requires tc::has_mem_fn_end_index<Derived_> && tc::is_equality_comparable<Index>::value
			constexpr const_iterator end() const&
				return_MAYTHROW(make_iterator(this->end_index()))

			template<typename Derived_ = Derived> // no requires, so lower priority
			constexpr end_sentinel end() const& noexcept {
				return {};
			}

			template<typename It>
			constexpr static decltype(auto) iterator2index(It&& it) noexcept {
				if constexpr(std::same_as<Index, tc::decay_t<It>>) {
					return tc_move_if_owned(it);
				} else {
					static_assert(std::same_as<Index, tc::decay_t<decltype(tc_unwrap_temporary(tc_move_if_owned(it)).m_idx)>>);
					return tc_rewrap_temporary(It, tc_unwrap_temporary(tc_move_if_owned(it)).m_idx);
				}
			}
		};
	}
	using no_adl::range_iterator_from_index;

	namespace no_adl {
		template<typename Rng>
		struct TC_EMPTY_BASES range_adaptor_base_range : /*not private to enable use as non-type template parameter*/ tc::reference_or_value<Rng, /*bBestAccess*/true> {
		private:
			static_assert(!std::is_rvalue_reference<Rng>::value);
			using base_= tc::reference_or_value<Rng, true>;
			using base_::operator->;
			using base_::operator*;
			using base_::best_access;

		public:
			constexpr range_adaptor_base_range()=default;
			template<typename Rhs>
			constexpr range_adaptor_base_range(tc::aggregate_tag_t, Rhs&& rhs) noexcept
				: tc::reference_or_value<Rng, /*bBestAccess*/true>(tc::aggregate_tag, tc_move_if_owned(rhs))
			{}

			constexpr decltype(auto) base_range() & noexcept { return **this; }
			constexpr decltype(auto) base_range() const& noexcept { return **this; }
			constexpr decltype(auto) base_range() && noexcept { return *tc_move_always(*this); }
			constexpr decltype(auto) base_range() const&& noexcept { return *tc_move_always_even_const(*this); }
			constexpr decltype(auto) base_range_best_access() const& noexcept { return this->best_access(); }

			template<ENABLE_SFINAE>
			constexpr auto base_begin_index() const& return_decltype_MAYTHROW(
				tc::begin_index(SFINAE_VALUE(this)->base_range_best_access())
			)
			template<ENABLE_SFINAE>
			constexpr auto base_end_index() const& return_decltype_MAYTHROW(
				tc::end_index(SFINAE_VALUE(this)->base_range_best_access())
			)
		};
	}
	using no_adl::range_adaptor_base_range;

	namespace generator_range_adl {
		//-------------------------------------------------------------------------------------------------------------------------
		// First generator ranges
		//
		// a generator range is any type that supports an operator() with a template parameter that is a Function that can be 
		// called with the element type of the range. 
		// The generator range should support the break_or_continue protocol

		template<typename Rng>
		struct TC_EMPTY_BASES generator_range_adaptor : tc::range_adaptor_base_range<Rng> {
			constexpr generator_range_adaptor()=default;
			using range_adaptor_base_range<Rng>::range_adaptor_base_range;
			using is_generator_range_adaptor = void;
		};

		template<typename Self, typename Sink, typename std::remove_reference_t<Self>::is_generator_range_adaptor* = nullptr>
		constexpr auto for_each_impl(Self&& self, Sink&& sink) return_decltype_MAYTHROW(
			tc::for_each(
				tc_move_if_owned(self).base_range(),
				tc_move_if_owned(self).adapted_sink(tc_move_if_owned(sink), /*bReverse*/tc::constant<false>())
			)
		)
	}
	using generator_range_adl::generator_range_adaptor;

	namespace generator_range_output_detail::no_adl {
		template<typename Derived, typename T>
		struct generator_range_output_sink_base {
			STATICASSERTSAME( T, std::remove_cv_t<T>, "range output must be a reference or cv-unqualified object type" );

			template<typename Derived_ = Derived>
			constexpr auto operator()(T t) const& return_decltype_MAYTHROW(
				tc_invoke(tc::derived_cast<Derived_>(this)->m_sink, tc_move_if_owned(t))
			)
		};

		template<typename Sink, typename... T>
		struct generator_range_output_sink : generator_range_output_sink_base<generator_range_output_sink<Sink, T...>, T>... {
			static_assert(tc::decayed<Sink>);
			using guaranteed_break_or_continue = guaranteed_break_or_continue_t<Sink>;
			Sink m_sink;

			template<typename Sink_>
			constexpr generator_range_output_sink(tc::aggregate_tag_t, Sink_&& sink) noexcept : m_sink(tc_move_if_owned(sink)) {}

			using generator_range_output_sink_base<generator_range_output_sink<Sink, T...>, T>::operator()...;

			// generator_range_output_sink forwards chunks without modifications.
			// This is relevant when m_sink is a tc::contiguous_chunk_appender or tc::no_adl::with_iterator_range.
			// To enforce consistency with operator(), we enforce all output types of a chunk to be similar to
			// one of the declared output types.
			template<typename U>
			using is_valid_chunk_output = tc::constant<
				((
					std::is_reference<T>::value && !std::is_const<std::remove_reference_t<T>>::value
						? std::same_as<T, U&&> // require exact match for mutable references
						: std::same_as< // bind immutable references and prvalues from any reference to same underlying object type
							std::remove_const_t<std::remove_reference_t<T>>,
							std::remove_const_t<std::remove_reference_t<U>>
						>
				) || ...)
			>;

			template<typename Rng> requires tc::has_mem_fn_chunk<Sink const&, Rng> && boost::mp11::mp_all_of<tc::range_output_t<Rng>, is_valid_chunk_output>::value
			constexpr auto chunk(Rng&& rng) const& noexcept(noexcept(m_sink.chunk(std::declval<Rng>()))) {
				return m_sink.chunk(tc_move_if_owned(rng));
			}
		};
	}

	namespace generator_range_output_adaptor_adl {
		template<typename Rng, typename... T>
		struct [[nodiscard]] TC_EMPTY_BASES generator_range_output_adaptor : generator_range_output_adaptor<Rng, boost::mp11::mp_list<T...>> {
			using generator_range_output_adaptor<Rng, boost::mp11::mp_list<T...>>::generator_range_output_adaptor;
		};

		template<typename Rng, typename... T>
		struct [[nodiscard]] TC_EMPTY_BASES generator_range_output_adaptor<Rng, boost::mp11::mp_list<T...>> : tc::generator_range_adaptor<Rng> {
			using generator_range_adaptor<Rng>::generator_range_adaptor;
			friend auto range_output_t_impl(generator_range_output_adaptor const&)
				-> boost::mp11::mp_unique<boost::mp11::mp_list<tc::remove_rvalue_reference_t<T>...>>; // declaration only

			template<typename Sink>
			constexpr auto adapted_sink(Sink&& sink, bool /*bReverse*/) const& noexcept {
				return generator_range_output_detail::no_adl::generator_range_output_sink<tc::decay_t<Sink>, T...>(tc::aggregate_tag, tc_move_if_owned(sink));
			}
		};
	}

	template<typename... TypeListOrTs, typename Rng>
	constexpr auto generator_range_output(Rng&& rng) noexcept {
		return generator_range_output_adaptor_adl::generator_range_output_adaptor<Rng, TypeListOrTs...>(tc::aggregate_tag, tc_move_if_owned(rng));
	}

	namespace range_output_from_base_range_adl {
		struct TC_EMPTY_BASES range_output_from_base_range {
			template<typename Derived, std::enable_if_t<tc::decayed_derived_from<Derived, range_output_from_base_range>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Derived&&) -> tc::range_output_t<decltype(std::declval<Derived>().base_range())> {} // unevaluated
		};
	}
	using range_output_from_base_range_adl::range_output_from_base_range;

#ifdef _CHECKS
	namespace no_adl {
		template< typename Rng >
		struct SSinglePassRange : tc::noncopyable {
			explicit SSinglePassRange(Rng&& rng) noexcept
			: m_rng(tc_move(rng)) {}
			template< typename Sink >
			constexpr auto operator()(Sink&& sink) && MAYTHROW {
				_ASSERTE( tc::change(m_bFirstPass, false) );
				return tc::for_each(tc_move(m_rng), tc_move_if_owned(sink));
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
		return tc_move_if_owned(rng);
	}
#endif

	//-------------------------------------------------------------------------------------------------------------------------
	// iterator/index based ranges
	//
	// they derive from the generator case, because the generator interface can transparently and efficiently be added
	// to any iterator or index based range.
	//
	namespace index_range_adaptor_flags_adl {
		// Cannot use tc::enumset due to cyclic dependency.
		enum class index_range_adaptor_flags {
			inherit_none = 0,

			inherit_begin = 1 << 0,
			inherit_end = 1 << 1,
			inherit_begin_end = inherit_begin | inherit_end,

			inherit_dereference = 1 << 2,
			inherit_traversal = 1 << 3,
			inherit_behavior = inherit_dereference | inherit_traversal,

			inherit_all = inherit_begin_end | inherit_behavior,
		};
		TC_BITMASK_OPS(index_range_adaptor_flags)
	}
	using index_range_adaptor_flags_adl::index_range_adaptor_flags;

	namespace no_adl {
		namespace index_range_adaptor_detail {
			template <typename Derived, typename Rng, bool InheritBehavior>
			struct iterator_range_interface : tc::range_iterator_from_index<Derived, tc::index_t<Rng>> {};

			template <typename Derived, typename Rng>
				requires tc::range_with_iterators<Rng>
			struct iterator_range_interface<Derived, Rng, true> : tc::index_range_interface<Derived, tc::index_t<Rng>> {
			private:
				using iterator = tc::iterator_t<decltype(*std::declval<tc::reference_or_value<Rng>&>())>;
				using const_iterator = tc::iterator_t<decltype(*std::declval<tc::reference_or_value<Rng> const&>())>;
				
				struct sentinel {
					Derived const* self;

					constexpr bool operator==(auto const& it) const MAYTHROW {
						return tc::at_end_index(*self, tc::iterator2index<Rng>(it));
					}
				};

			public:
				constexpr iterator make_iterator(auto&& idx) & return_MAYTHROW(
					tc::make_iterator(tc::derived_cast<Derived>(MSVC_WORKAROUND_THIS)->base_range(), tc_move_if_owned(idx))
				)
				constexpr const_iterator make_iterator(auto&& idx) const& return_MAYTHROW(
					tc::make_iterator(tc::derived_cast<Derived>(MSVC_WORKAROUND_THIS)->base_range(), tc_move_if_owned(idx))
				)

				constexpr iterator begin() &
					return_MAYTHROW(make_iterator(this->begin_index()))
				constexpr const_iterator begin() const&
					return_MAYTHROW(make_iterator(this->begin_index()))

				template<typename Derived_ = Derived>
					requires tc::has_mem_fn_end_index<Derived_>
				constexpr iterator end() &
					return_MAYTHROW(make_iterator(this->end_index()))
				template<typename Derived_ = Derived>
					requires tc::has_mem_fn_end_index<Derived_>
				constexpr const_iterator end() const&
					return_MAYTHROW(make_iterator(this->end_index()))

				template<typename Derived_ = Derived> // no requires, so lower priority
				constexpr auto end() const& noexcept {
					return sentinel{tc::derived_cast<Derived>(MSVC_WORKAROUND_THIS)};
				}

				template<typename It>
				constexpr static decltype(auto) iterator2index(It&& it) noexcept {
					return tc::iterator2index<Rng>(tc_move_if_owned(it));
				}
			};
		}

		template<
			typename Derived,
			typename Rng, index_range_adaptor_flags Flags,
			typename Base = tc::range_adaptor_base_range<Rng>
		>
		struct TC_EMPTY_BASES index_range_adaptor
			: Base
			, index_range_adaptor_detail::iterator_range_interface<
				Derived, Rng,
				(Flags & index_range_adaptor_flags::inherit_behavior) == index_range_adaptor_flags::inherit_behavior
			>
		{
		private:
			using this_type = index_range_adaptor;

		public:
			constexpr index_range_adaptor() = default;
			using Base::Base;

			using tc_index = tc::index_t<Rng>;
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng>>::value;

		private:
			static constexpr auto inherit_begin = static_cast<bool>(Flags & index_range_adaptor_flags::inherit_begin);

			STATIC_OVERRIDE_MOD(constexpr,begin_index)() const& MAYTHROW
				requires inherit_begin
			{
				return this->base_begin_index();
			}

		private:
			static constexpr auto inherit_end = static_cast<bool>(Flags & index_range_adaptor_flags::inherit_end);

			STATIC_OVERRIDE_MOD(constexpr,end_index)() const& MAYTHROW
				requires inherit_end && tc::has_end_index<Rng>
			{
				return this->base_end_index();
			}

			STATIC_OVERRIDE_MOD(constexpr,at_end_index)(tc_index const& idx) const& MAYTHROW
				requires inherit_end
			{
				return tc::at_end_index(this->base_range(),idx);
			}

		private:
			static constexpr auto inherit_dereference = static_cast<bool>(Flags & index_range_adaptor_flags::inherit_dereference);

			STATIC_OVERRIDE_MOD(constexpr, dereference_index)(auto&& idx) & MAYTHROW -> decltype(auto)
				requires inherit_dereference
			{
				return tc::dereference_index(this->base_range(), tc_move_if_owned(idx));
			}
			STATIC_OVERRIDE_MOD(constexpr, dereference_index)(auto&& idx) const& MAYTHROW -> decltype(auto)
				requires inherit_dereference
			{
				return tc::dereference_index(this->base_range(), tc_move_if_owned(idx));
			}

		public:
			constexpr decltype(auto) dereference_untransform(auto&& idx) const& noexcept
				requires inherit_dereference
			{
				return this->base_range().dereference_untransform(tc_move_if_owned(idx));
			}

			static constexpr decltype(auto) element_base_index(auto&& idx) noexcept
				requires inherit_dereference
			{
				return tc_move_if_owned(idx);
			}

		private:
			static constexpr auto inherit_traversal = static_cast<bool>(Flags & index_range_adaptor_flags::inherit_traversal);

			STATIC_OVERRIDE_MOD(constexpr,increment_index)(tc_index& idx) const& MAYTHROW
				requires inherit_traversal
			{
				tc::increment_index(this->base_range(),idx);
			}

			STATIC_OVERRIDE_MOD(constexpr, decrement_index)(tc_index& idx) const& MAYTHROW
				requires inherit_traversal && tc::has_decrement_index<std::remove_reference_t<Rng>>
			{
				tc::decrement_index(this->base_range(),idx);
			}

			STATIC_OVERRIDE_MOD(constexpr, advance_index)(tc_index& idx, typename boost::range_difference<Rng>::type d) const& MAYTHROW
				requires inherit_traversal && tc::has_advance_index<std::remove_reference_t<Rng>>
			{
				tc::advance_index(this->base_range(),idx,d);
			}

			STATIC_OVERRIDE_MOD(constexpr, distance_to_index)(tc_index const& idxLhs, tc_index const& idxRhs) const& MAYTHROW
				requires inherit_traversal && tc::has_distance_to_index<std::remove_reference_t<Rng>>
			{
				return tc::distance_to_index(this->base_range(),idxLhs,idxRhs);
			}

			STATIC_OVERRIDE_MOD(constexpr, middle_point)( tc_index & idxBegin, tc_index const& idxEnd ) const& MAYTHROW
				requires inherit_traversal && tc::has_middle_point<std::remove_reference_t<Rng>>
			{
				tc::middle_point(this->base_range(),idxBegin,idxEnd);
			}


			STATIC_OVERRIDE_MOD(constexpr, index_to_address)(tc_index const& idx) & noexcept
				requires inherit_dereference && inherit_traversal && tc::has_index_to_address<std::remove_reference_t<Rng>>
			{
				return tc::index_to_address(this->base_range(), idx);
			}
			STATIC_OVERRIDE_MOD(constexpr, index_to_address)(tc_index const& idx) const& noexcept
				requires inherit_dereference && inherit_traversal && tc::has_index_to_address<std::remove_reference_t<Rng>>
			{
				return tc::index_to_address(this->base_range(), idx);
			}
		};
	}
	using no_adl::index_range_adaptor;

	namespace no_adl {
		template<template<bool, typename...> typename AdaptorTemplate, template<typename...> typename IndexTemplate, typename... Rng>
		struct product_index_range_adaptor
			: AdaptorTemplate<false, Rng...>
			, range_iterator_from_index<
				AdaptorTemplate<true, Rng...>,
				IndexTemplate<tc::index_t<std::remove_reference_t<Rng>>...>
			>
		{
			static constexpr bool c_bHasStashingIndex=std::disjunction<tc::has_stashing_index<std::remove_reference_t<Rng>>...>::value;

			using AdaptorTemplate<false, Rng...>::AdaptorTemplate;
			using typename product_index_range_adaptor::range_iterator_from_index::tc_index;

			template<typename Self>
			static constexpr auto base_ranges(Self&& self) noexcept { // TODO C++23 deducing *this
				return tc::tuple_transform(
					tc_move_if_owned(self).m_tupleadaptbaserng,
					tc_mem_fn(.base_range)
				);
			}

		private:
			using Derived = AdaptorTemplate<true, Rng...>;
			using this_type = product_index_range_adaptor;

			STATIC_OVERRIDE_MOD(template<typename Index> constexpr, dereference_index)(Index&& idx) const& MAYTHROW {
				return tc::tuple_transform(
					tc::zip(this->m_tupleadaptbaserng, idx),
					[](auto&& adaptbaserng, auto&& baseidx) MAYTHROW -> decltype(auto) {
						return tc::dereference_index(adaptbaserng.base_range(), tc::forward_like<Index>(baseidx));
					}
				);
			}

			STATIC_OVERRIDE_MOD(template<typename Index> constexpr, dereference_index)(Index&& idx) & MAYTHROW {
				return tc::tuple_transform(
					tc::zip(this->m_tupleadaptbaserng, idx),
					[](auto&& adaptbaserng, auto&& baseidx) MAYTHROW -> decltype(auto) {
						return tc::dereference_index(adaptbaserng.base_range(), tc::forward_like<Index>(baseidx));
					}
				);
			}
		};
	}
	using no_adl::product_index_range_adaptor;
}

namespace tc::no_adl {
	template<typename... T> requires (requires { typename tc::value_t<T>; } && ...)
	struct value_type_impl<tc::tuple<T...>> final {
		using type = tc::tuple<tc::value_t<T>...>;
	};
}
