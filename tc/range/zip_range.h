
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/functors.h"
#include "../base/utility.h"
#include "../algorithm/quantifier.h"
#include "../algorithm/element.h"
#include "../algorithm/size_linear.h"
#include "../algorithm/empty.h"
#include "filter_adaptor.h"
#include "range_adaptor.h"
#include "reverse_adaptor.h"
#include "iota_range.h"

namespace tc {
	namespace zip_detail {
		// These functions are only used in zip_adaptor::internal_for_each_impl. They do not depend on Sink, so having them outside saves compilation time (a few seconds for LayoutSolver.cpp).
		template<typename... AdaptBaseRng>
		constexpr auto MakeRngIndexPairTuple(AdaptBaseRng&&... adaptbaserng) MAYTHROW {
			return tc::make_tuple(
				std::pair<decltype(std::declval<AdaptBaseRng>().base_range()), decltype(adaptbaserng.base_begin_index())>(
					std::forward<AdaptBaseRng>(adaptbaserng).base_range(),
					adaptbaserng.base_begin_index() // MAYTHROW
				)...
			);
		}
		template<typename... AdaptBaseRng>
		constexpr auto MakeRngEndIndexPairTuple(AdaptBaseRng&&... adaptbaserng) MAYTHROW {
			return tc::make_tuple(
				std::pair<decltype(std::declval<AdaptBaseRng>().base_range()), decltype(adaptbaserng.base_end_index())>(
					std::forward<AdaptBaseRng>(adaptbaserng).base_range(),
					adaptbaserng.base_end_index() // MAYTHROW
				)...
			);
		}

		template<typename PairRngIdx>
		constexpr decltype(auto) DereferenceRngIndexPair(PairRngIdx& pairrngidx) MAYTHROW {
			return tc::dereference_index(pairrngidx.first, pairrngidx.second); // MAYTHROW
		}

		template<typename PairRngIdx>
		constexpr void IncrementRngIndexPair(PairRngIdx& pairrngidx) MAYTHROW {
			tc::increment_index(pairrngidx.first, pairrngidx.second);
		}

		template<typename PairRngIdx>
		constexpr void DecrementRngIndexPair(PairRngIdx& pairrngidx) MAYTHROW {
			tc::decrement_index(pairrngidx.first, pairrngidx.second);
		}


#ifdef _CHECKS
		template<typename PairRngIdx>
		constexpr bool RngIndexPairAtEnd(PairRngIdx& pairrngidx) noexcept {
			return tc::at_end_index(pairrngidx.first, pairrngidx.second);
		}
#endif
	}

	namespace no_adl {

		template<typename...>
		struct smallest_numeric_type;

		template<typename A>
		struct smallest_numeric_type<A> {
			using type = A;
		};

		template<typename A, typename B, typename... Rest>
		struct smallest_numeric_type<A, B, Rest...>
			: smallest_numeric_type<std::conditional_t<std::numeric_limits<B>::max() < std::numeric_limits<A>::max(), B, A>, Rest...>
		{};

		template<typename... T>
		using smallest_numeric_type_t = typename smallest_numeric_type<T...>::type;

		template<typename Rng>
		using range_difference_t = typename boost::range_difference<std::remove_cvref_t<Rng>>::type;

		template<typename T, typename ConstIndex, typename... Rng>
		using zip_adaptor_with_generator_forwarding_tuple_t = tc::type::apply_t<
			tc::tuple,
			tc::type::transform_t<
				tc::type::concat_t<
					tc::type::transform_t<
						tc::type::transform_t<
							tc::type::take_first_t<tc::type::list<Rng...>, ConstIndex::value>,
							tc::iterator_t
						>,
						std::iter_reference_t
					>,
					tc::type::list<T>,
					tc::type::transform_t<
						tc::type::transform_t<
							tc::type::drop_first_t<tc::type::list<Rng...>, ConstIndex::value+1>,
							tc::iterator_t
						>,
						std::iter_reference_t
					>
				>,
				std::add_rvalue_reference_t // forwarding tuple
			>
		>;

		template<typename Rng, typename Self>
		using apply_cvref_to_base_range_t = decltype(std::declval<tc::apply_cvref_t<tc::range_adaptor_base_range<Rng>, Self>>().base_range());

		namespace trait_from_concept_workaround { // workaround Xcode14.1 segmentation fault
			template<typename Rng>
			using has_constexpr_size = tc::constant<tc::has_constexpr_size<Rng>>;
		}

		template<
			bool HasIterator,
			typename... Rng
		>
		struct [[nodiscard]] zip_adaptor {
		protected:
			tc::tuple<tc::range_adaptor_base_range<Rng>...> m_tupleadaptbaserng;
#ifdef __clang__
		private:
			template<typename T>
			using is_range_with_iterators = tc::constant<tc::range_with_iterators<T>>;
#endif

#if !defined(__clang__) && !defined(_MSC_VER) // Bug in gcc12
		public:
#endif		
			static constexpr std::size_t generator_index() noexcept {
				using PureGenerator = tc::type::find_unique_if<
					tc::type::list<Rng...>,
					tc::type::negation<
#ifdef __clang__
						is_range_with_iterators // workaround Xcode14 clang segmentation fault
#else
						TRAITFROMCONCEPT(tc::range_with_iterators)
#endif
					>::template type
				>;
				if constexpr( PureGenerator::found ) {
					return PureGenerator::index;
				} else {
					return 0; // TODO? Heuristically select generator to avoid complex iterators.
				}
			}

		public:
			template<typename... RngRef>
			constexpr zip_adaptor(aggregate_tag_t, RngRef&&... rng) noexcept
				: m_tupleadaptbaserng{{ {{aggregate_tag, std::forward<RngRef>(rng)}}... }}
			{}

			template<tc::decayed_derived_from<zip_adaptor> Self, typename Sink>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				return internal_for_each_impl(
					std::forward<Self>(self),
					std::forward<Sink>(sink),
					std::make_index_sequence<generator_index()>(),
					std::make_index_sequence<sizeof...(Rng) - generator_index() - 1>()
				); // MAYTHROW
			}
			template<tc::decayed_derived_from<zip_adaptor> Self, typename Sink>
			friend constexpr auto for_each_reverse_impl(Self&& self, Sink&& sink) MAYTHROW {
				return internal_for_each_reverse_impl(
					std::forward<Self>(self),
					std::forward<Sink>(sink),
					std::make_index_sequence<generator_index()>(),
					std::make_index_sequence<sizeof...(Rng) - generator_index() - 1>()
				); // MAYTHROW
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, zip_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> tc::type::unique_t<tc::type::transform_t<
				tc::range_output_t<apply_cvref_to_base_range_t<tc::type::at_t<tc::type::list<Rng...>, generator_index()>, Self>>,
				tc::type::rcurry<
					zip_adaptor_with_generator_forwarding_tuple_t,
					tc::constant<generator_index()>,
					apply_cvref_to_base_range_t<Rng, Self>... // workaround VS compiler bug: https://developercommunity.visualstudio.com/t/visual-studio-fails-to-expand-parameter-pack-with/1620097
				>::template type
			>> {} // unevaluated

		private:
			template<typename Self, typename Sink, std::size_t... nPrefix/*=0,...,generator_index()-1*/, std::size_t... nSuffix/*=0,...,sizeof...(Rng)-generator_index()-2*/>
			static constexpr auto internal_for_each_impl(Self&& self, Sink&& sink, std::index_sequence<nPrefix...>, std::index_sequence<nSuffix...>) MAYTHROW {
				STATICASSERTEQUAL( sizeof...(nPrefix), generator_index() );
				STATICASSERTEQUAL( sizeof...(nPrefix) + 1 + sizeof...(nSuffix), sizeof...(Rng) );
				auto tuplepairrngidxPrefix = zip_detail::MakeRngIndexPairTuple(tc::get<nPrefix>(std::forward<Self>(self).m_tupleadaptbaserng)...); // MAYTHROW
				auto tuplepairrngidxSuffix = zip_detail::MakeRngIndexPairTuple(tc::get<sizeof...(nPrefix) + 1 + nSuffix>(std::forward<Self>(self).m_tupleadaptbaserng)...); // MAYTHROW
				auto const breakorcontinue = tc::for_each(tc::get<sizeof...(nPrefix)>(std::forward<Self>(self).m_tupleadaptbaserng).base_range(), [&](auto&& u) MAYTHROW {
					auto const breakorcontinue = tc::continue_if_not_break(
						sink,
						tc::forward_as_tuple(
							zip_detail::DereferenceRngIndexPair(tc::get<nPrefix>(tuplepairrngidxPrefix))...,  // MAYTHROW
							tc_move_if_owned(u),
							zip_detail::DereferenceRngIndexPair(tc::get<nSuffix>(tuplepairrngidxSuffix))...  // MAYTHROW
						)
					); // MAYTHROW
					if( tc::continue_ == breakorcontinue ) {
						(zip_detail::IncrementRngIndexPair(tc::get<nPrefix>(tuplepairrngidxPrefix)), ...); // MAYTHROW
						(zip_detail::IncrementRngIndexPair(tc::get<nSuffix>(tuplepairrngidxSuffix)), ...); // MAYTHROW
					}
					return breakorcontinue;
				});  // MAYTHROW
				_ASSERTE( tc::break_ == breakorcontinue || (zip_detail::RngIndexPairAtEnd(tc::get<nPrefix>(tuplepairrngidxPrefix)) && ...) && (zip_detail::RngIndexPairAtEnd(tc::get<nSuffix>(tuplepairrngidxSuffix)) && ...) );
				return breakorcontinue;
			}
			template<typename Self, typename Sink, std::size_t... nPrefix/*=0,...,generator_index()-1*/, std::size_t... nSuffix/*=0,...,sizeof...(Rng)-generator_index()-2*/>
			static constexpr auto internal_for_each_reverse_impl(Self&& self, Sink&& sink, std::index_sequence<nPrefix...>, std::index_sequence<nSuffix...>) MAYTHROW {
				STATICASSERTEQUAL( sizeof...(nPrefix), generator_index() );
				STATICASSERTEQUAL( sizeof...(nPrefix) + 1 + sizeof...(nSuffix), sizeof...(Rng) );
				auto tuplepairrngidxPrefix = zip_detail::MakeRngEndIndexPairTuple(tc::get<nPrefix>(std::forward<Self>(self).m_tupleadaptbaserng)...); // MAYTHROW
				auto tuplepairrngidxSuffix = zip_detail::MakeRngEndIndexPairTuple(tc::get<sizeof...(nPrefix) + 1 + nSuffix>(std::forward<Self>(self).m_tupleadaptbaserng)...); // MAYTHROW
				auto const breakorcontinue = tc::for_each(tc::reverse(tc::get<sizeof...(nPrefix)>(std::forward<Self>(self).m_tupleadaptbaserng).base_range()), [&](auto&& u) MAYTHROW {
					(zip_detail::DecrementRngIndexPair(tc::get<nPrefix>(tuplepairrngidxPrefix)), ...); // MAYTHROW
					(zip_detail::DecrementRngIndexPair(tc::get<nSuffix>(tuplepairrngidxSuffix)), ...); // MAYTHROW
					return tc::continue_if_not_break(
						sink,
						tc::forward_as_tuple(
							zip_detail::DereferenceRngIndexPair(tc::get<nPrefix>(tuplepairrngidxPrefix))...,  // MAYTHROW
							tc_move_if_owned(u),
							zip_detail::DereferenceRngIndexPair(tc::get<nSuffix>(tuplepairrngidxSuffix))...  // MAYTHROW
						)
					); // MAYTHROW
				}); // MAYTHROW
				_ASSERTE( tc::break_ == breakorcontinue || ((tc::begin_index(tc::get<nPrefix>(tuplepairrngidxPrefix).first)==tc::get<nPrefix>(tuplepairrngidxPrefix).second) && ...) && ((tc::begin_index(tc::get<nSuffix>(tuplepairrngidxSuffix).first)==tc::get<nSuffix>(tuplepairrngidxSuffix).second) && ...) );
				return breakorcontinue;
			}

		public:
			constexpr auto size() const& MAYTHROW requires (... || tc::has_size<Rng>) && (!(... || tc::has_constexpr_size<Rng>)) {
				return tc::all_same_element<tc::return_value>(tc::generator_range_output<std::size_t>(tc::transform(
					tc::filter(m_tupleadaptbaserng, [](auto const& rng) noexcept {
						return tc::constant<tc::has_size<decltype(rng.base_range())>>{};
					}),
					[](auto const& rng) noexcept {
						return tc::size_raw(rng.base_range());
					}
				)));
			}

			constexpr auto size() const& noexcept requires (... || tc::has_constexpr_size<Rng>) {
				using sized_rng = tc::type::front_t<tc::type::filter_t<tc::type::list<Rng...>, trait_from_concept_workaround::has_constexpr_size>>;
				return tc::constexpr_size<sized_rng>;
			}
		};

		template<typename... Index>
		struct zip_index : tc::tuple<Index...> {
			friend bool operator==(zip_index const& lhs, zip_index const& rhs) noexcept {
				return tc::get<0>(lhs) == tc::get<0>(rhs);
			}
		};

		template<typename... Rng>
		struct [[nodiscard]] zip_adaptor<true, Rng...>
			: product_index_range_adaptor<zip_adaptor, zip_index, Rng...>
		{
		private:
			using this_type = zip_adaptor;
		public:
			using product_index_range_adaptor<zip_adaptor, zip_index, Rng...>::product_index_range_adaptor;

			using typename this_type::range_iterator_from_index::tc_index;
			using difference_type = smallest_numeric_type_t<range_difference_t<Rng>...>;

		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				return {tc::tuple_transform(this->m_tupleadaptbaserng, tc_mem_fn(.base_begin_index))};
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index
				requires (... && tc::has_end_index<std::remove_reference_t<Rng>>)
			{
				return {tc::tuple_transform(this->m_tupleadaptbaserng, tc_mem_fn(.base_end_index))};
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& noexcept -> bool {
				auto MemberRangeAtEnd = 
					[&](auto const nconstIndex) noexcept {
						return tc::at_end_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(), tc::get<nconstIndex()>(idx));
					};

				bool const bAtEnd = MemberRangeAtEnd(tc::constant<tc::explicit_cast<std::size_t>(0)>());

				_ASSERT(tc::all_of(
					tc::make_integer_sequence<std::size_t, 1, sizeof...(Rng)>(),
					[&](auto const nconstIndex) noexcept { return MemberRangeAtEnd(nconstIndex) == bAtEnd; }
				));

				return bAtEnd;
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept -> void {
				tc::for_each(
					tc::zip(this->m_tupleadaptbaserng, idx),
					[](auto&& adaptbaserng, auto& baseidx) noexcept {
						tc::increment_index(adaptbaserng.base_range(), baseidx);
					}
				);
			}

			STATIC_FINAL_MOD(constexpr, decrement_index)(tc_index& idx) const& noexcept -> void
				requires (... && tc::has_decrement_index<std::remove_reference_t<Rng>>)
			{
				tc::for_each(
					tc::zip(this->m_tupleadaptbaserng, idx),
					[](auto&& adaptbaserng, auto& baseidx) noexcept {
						tc::decrement_index(adaptbaserng.base_range(), baseidx);
					}
				);
			}

			STATIC_FINAL_MOD(constexpr, advance_index)(tc_index& idx, difference_type d) const& noexcept -> void
				requires (... && tc::has_advance_index<std::remove_reference_t<Rng>>)
			{
				tc::for_each(
					tc::zip(this->m_tupleadaptbaserng, idx),
					[&](auto&& adaptbaserng, auto& baseidx) noexcept {
						tc::advance_index(adaptbaserng.base_range(), baseidx, d);
					}
				);
			}

			// For consistency with other functions, distance_to_index is only available if all base ranges support it - even though we only require one of the base ranges to support it.
			STATIC_FINAL_MOD(constexpr, distance_to_index)(tc_index const& idxLhs, tc_index const& idxRhs) const& noexcept->difference_type
				requires (... && tc::has_distance_to_index<std::remove_reference_t<Rng>>)
			{
				tc_return_cast(tc::distance_to_index(tc::get<0>(this->m_tupleadaptbaserng).base_range(), tc::get<0>(idxLhs), tc::get<0>(idxRhs)));
			}
		};
	}
	using no_adl::zip_adaptor;

	template<bool HasIterator, typename... Rng>
	constexpr auto enable_stable_index_on_move<zip_adaptor<HasIterator, Rng...>>
		= (tc::stable_index_on_move<Rng> && ...);

	template<typename... Rng>
	constexpr no_adl::zip_adaptor</*HasIterator*/(... && tc::range_with_iterators<Rng>), Rng...> zip(Rng&&... rng) noexcept {
		return {tc::aggregate_tag, std::forward<Rng>(rng)...};
	}

	template<typename Rng0, typename Rng1>
	constexpr auto zip_any(Rng0&& rng0, Rng1&& rng1) noexcept {
		return [
			rng0_ = reference_or_value<Rng0>(tc::aggregate_tag, std::forward<Rng0>(rng0)),
			rng1_ = reference_or_value<Rng1>(tc::aggregate_tag, std::forward<Rng1>(rng1))
		](auto sink) MAYTHROW {
			auto it0 = tc::begin(*rng0_);
			auto it1 = tc::begin(*rng1_);

			tc_auto_cref(it0End, tc::end(tc::as_const(*rng0_)));
			tc_auto_cref(it1End, tc::end(tc::as_const(*rng1_)));
			for(;;) {
				if (it0End == it0) {
					return tc::for_each(
						tc::make_iterator_range(it1,it1End),
						[&](auto&& elem) noexcept {
							return tc::continue_if_not_break(sink, tc::tuple<std::nullopt_t, decltype(elem)>{{ {{std::nullopt}}, {{tc_move_if_owned(elem)}} }});
						}
					);
				} else if (it1End == it1) {
					return tc::for_each(
						tc::make_iterator_range(it0,it0End),
						[&](auto&& elem) noexcept {
							return tc::continue_if_not_break(sink, tc::tuple<decltype(elem), std::nullopt_t>{{ {{tc_move_if_owned(elem)}}, {{std::nullopt}} }});
						}
					);
				} else {
					tc_yield(sink, tc::forward_as_tuple(*it0, *it1));
					++it0;
					++it1;
				}
			}
		};
	}

	template<typename Rng> requires tc::instance_b<std::remove_reference_t<Rng>, no_adl::zip_adaptor>
	[[nodiscard]] constexpr decltype(auto) unzip(Rng&& rng) noexcept {
		return std::remove_reference_t<Rng>::base_ranges(std::forward<Rng>(rng));
	}

	template<typename Rng> requires
		tc::instance_b<std::remove_reference_t<
				tc::type::only_t<
					typename tc::is_instance<std::remove_reference_t<Rng>, subrange>::arguments
				>
			>, no_adl::zip_adaptor>
	[[nodiscard]] constexpr auto unzip(Rng&& rng) noexcept {
		return tc::tuple_transform(
			tc::zip(
				tc::unzip(std::forward<Rng>(rng).base_range()),
				std::forward<Rng>(rng).begin_index(),
				std::forward<Rng>(rng).end_index()
			),
			tc_fn(tc::slice)
		);
	}

	/*
		TODO: RT#16520
		It is reasonable that the following use case of zip_ranges should work without copying
		the R-value range.

		if (auto o = tc::find_last<tc::return_value_or_none>(
			tc::zip_ranges( CreateRValueRngRng() ),
			predicate
		)) {
			*o; // must still be valid.
		}

		- Currently, *o is 'tc::transform(*rngrng, [n](auto const& rng) return_decltype_MAYTHROW(tc::at(rng, n)))'
		with rngrng beeing then out of scope.

		return_value_or_none.pack_element(It&&, Rng&&, Ref&& ref) is called with R-Value range, so in principle
		it can call (note && and tc_move)
		transform_adaptor::dereference_index() && {
			tc_move(m_func)(...)
		},
		and the transform functor of zip_ranges could overload for '&&' and in that case aggregating rngrng.

		This is not possible with find_last using iterators. Todo is then to
		- Specialize find_last (and similar functions) for index-based ranges
		- Introduce pack_element for index-based results
		- Introduce transform_adaptor::dereference_index &&
	*/
	template<typename RngRng>
	auto zip_ranges(RngRng&& rngrng) noexcept { // for random access ranges
		_ASSERT(tc::all_same_element<tc::return_bool>(tc::transform(rngrng, tc_fn(tc::size))));
		auto const n = tc::empty(rngrng) ? 0 : tc::size(tc::front(rngrng)); // Do not inline, function evaluation order undefined
		return tc::transform(
			tc::iota(0, n),
			[rngrng = tc::make_reference_or_value(std::forward<RngRng>(rngrng))](auto const n) noexcept {
				// return_decltype_MAYTHROW(tc::at(rng, n)) may cause ICE on Apple clang 10
				return tc::transform(*rngrng, [n](auto const& rng) MAYTHROW -> decltype(auto) { return tc::at(rng, n); });
			}
		);
	}
	
	namespace enumerate_adl {
		template<typename Rng>
		struct enumerate_generator_adaptor : private tc::range_adaptor_base_range<Rng> {
			using base_ = tc::range_adaptor_base_range<Rng>;
			enumerate_generator_adaptor(Rng&& rng) noexcept
				: base_(tc::aggregate_tag, tc_move_if_owned(rng))
			{}

			template<tc::decayed_derived_from<enumerate_generator_adaptor> Self, typename Sink>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				int i = 0;
				// return_decltype_MAYTHROW with tc_move_if_owned causes compiler segfault on Mac
				return tc::for_each(std::forward<Self>(self).base_range(), [&](auto&& t) noexcept(noexcept(tc::invoke(sink, std::declval<tc::tuple<int, decltype(t)>>()))) -> decltype(auto) {
					return tc::invoke(sink, tc::tuple<int, decltype(t)>{i++, tc_move_if_owned(t)}); // MAYTHROW
				});
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, enumerate_generator_adaptor>>* = nullptr> // accept any cvref qualifiers. use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> typename tc::type::transform<tc::range_output_t<tc::no_adl::apply_cvref_to_base_range_t<Rng, Self>>, tc::type::curry<tc::tuple, int>::template type>::type {} // unevaluated
		};
	};

	// Transform [t0, t1, ..., tn] to [(0, t0), (1, t1), ..., (n, tn)]
	// TODO: return iterator range, if range has iterators.
	template <typename Rng>
	[[nodiscard]] constexpr auto enumerate(Rng&& rng) noexcept {
		if constexpr( tc::has_size<Rng> ) {
			auto nSize = tc::size(rng); // do not inline, evaluation order important
			return tc::zip(tc::iota(0, nSize), std::forward<Rng>(rng));
		} else {
			return enumerate_adl::enumerate_generator_adaptor<Rng>(tc_move_if_owned(rng));
		}
	}
}
