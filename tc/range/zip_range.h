
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
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
	namespace zip_detail::no_adl {
		template<
			typename Self, typename Sink, bool bReverse,
			typename PrefixSeq = std::make_index_sequence<Self::generator_index()>,
			typename SuffixSeq = std::make_index_sequence<Self::c_nZipAdaptorFactors - Self::generator_index() - 1>
		>
		struct zip_adaptor_sink;

		template<typename Self, typename Sink, bool bReverse, std::size_t... nPrefix/*=0,...,generator_index()-1*/, std::size_t... nSuffix/*=0,...,sizeof...(Rng)-generator_index()-2*/>
		struct zip_adaptor_sink<Self, Sink, bReverse, std::index_sequence<nPrefix...>, std::index_sequence<nSuffix...>> {
			using guaranteed_break_or_continue = std::conditional_t<
				std::is_same<tc::constant<tc::continue_>, tc::guaranteed_break_or_continue_t<Sink>>::value,
				tc::constant<tc::continue_>,
				tc::break_or_continue
			>;

		private:
			Self& m_self;
			Sink const m_sink;
			tc::tuple<decltype(tc::get<nPrefix>(std::declval<Self&>().m_tupleadaptbaserng).base_begin_index())...> m_tplidxPrefix;
			tc::tuple<decltype(tc::get<sizeof...(nPrefix) + 1 + nSuffix>(std::declval<Self&>().m_tupleadaptbaserng).base_begin_index())...> m_tplidxSuffix;

			template<std::size_t n>
			constexpr decltype(auto) PrefixBase() const& noexcept {
				return tc::get<n>(m_self.m_tupleadaptbaserng);
			}

			template<std::size_t n>
			constexpr decltype(auto) SuffixBase() const& noexcept {
				return tc::get<sizeof...(nPrefix) + 1 + n>(m_self.m_tupleadaptbaserng);
			}

		public:
			constexpr zip_adaptor_sink(Self& self, auto&& sink) MAYTHROW
				: m_self(self)
				, m_sink(tc_move_if_owned(sink))
				, m_tplidxPrefix{{ {bReverse ? PrefixBase<nPrefix>().base_end_index() : PrefixBase<nPrefix>().base_begin_index()}... }} // MAYTHROW
				, m_tplidxSuffix{{ {bReverse ? SuffixBase<nSuffix>().base_end_index() : SuffixBase<nSuffix>().base_begin_index()}... }} // MAYTHROW
			{}

			constexpr auto operator()(auto&& u) & MAYTHROW {
				if constexpr(bReverse) {
					(tc::decrement_index(PrefixBase<nPrefix>().base_range(), tc::get<nPrefix>(m_tplidxPrefix)), ...); // MAYTHROW
					(tc::decrement_index(SuffixBase<nSuffix>().base_range(), tc::get<nSuffix>(m_tplidxSuffix)), ...); // MAYTHROW
				}
				auto const boc = tc::continue_if_not_break(
					m_sink,
					tc::tie(
						tc::dereference_index(PrefixBase<nPrefix>().base_range(), tc::get<nPrefix>(m_tplidxPrefix))...,  // MAYTHROW
						tc_move_if_owned(u),
						tc::dereference_index(SuffixBase<nSuffix>().base_range(), tc::get<nSuffix>(m_tplidxSuffix))...  // MAYTHROW
					)
				); // MAYTHROW
				if constexpr(!bReverse) {
					if( tc::continue_ == boc ) {
						(tc::increment_index(PrefixBase<nPrefix>().base_range(), tc::get<nPrefix>(m_tplidxPrefix)), ...); // MAYTHROW
						(tc::increment_index(SuffixBase<nSuffix>().base_range(), tc::get<nSuffix>(m_tplidxSuffix)), ...); // MAYTHROW
					}
				}
				return boc;
			}
#ifdef _CHECKS
			constexpr bool SameLength() const& MAYTHROW {
				if constexpr(bReverse) {
					return ((tc::begin_index(PrefixBase<nPrefix>().base_range()) == tc::get<nPrefix>(m_tplidxPrefix)) && ...) // MAYTHROW
						&& ((tc::begin_index(SuffixBase<nSuffix>().base_range()) == tc::get<nSuffix>(m_tplidxSuffix)) && ...); // MAYTHROW
				} else {
					return (tc::at_end_index(PrefixBase<nPrefix>().base_range(), tc::get<nPrefix>(m_tplidxPrefix)) && ...) // MAYTHROW
						&& (tc::at_end_index(SuffixBase<nSuffix>().base_range(), tc::get<nSuffix>(m_tplidxSuffix)) && ...); // MAYTHROW
				}
			}
#endif
		};
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

#if defined(__clang__)
		template<typename T>
		using is_range_with_iterators = tc::constant<tc::range_with_iterators<T>>;
		template<typename T>
		using prefers_for_each_workaround = tc::constant<tc::prefers_for_each<T>>;
#endif

		template<typename T, typename ConstIndex, typename... Rng>
		using zip_adaptor_with_generator_forwarding_tuple_t = tc::mp_transform<
			std::add_rvalue_reference_t,
			boost::mp11::mp_replace_at<
				boost::mp11::mp_transform_if<
#ifdef __clang__
					is_range_with_iterators, // workaround Xcode14 clang segmentation fault
#else
					TRAITFROMCONCEPT(tc::range_with_iterators),
#endif
					tc::mp_chained<std::iter_reference_t, tc::iterator_t>::template fn, 
					tc::tuple<Rng...>
				>,
				ConstIndex, T
			>
		>;

		template<typename... Rng>
		using zip_adaptor_prefers_for_each = boost::mp11::mp_any_of<
			boost::mp11::mp_list<std::remove_reference_t<Rng>...>,
#ifdef __clang__
			prefers_for_each_workaround // workaround Xcode14 clang segmentation fault
#else
			TRAITFROMCONCEPT(tc::prefers_for_each)
#endif
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

			template<typename Self, typename Sink, bool bReverse, typename PrefixSeq, typename SuffixSeq>
			friend struct zip_detail::no_adl::zip_adaptor_sink;

		public:
			static auto constexpr c_nZipAdaptorFactors = sizeof...(Rng);

			static constexpr std::size_t generator_index() noexcept {
				using PureGenerator = tc::mp_find_unique_if<
					boost::mp11::mp_list<Rng...>,
					boost::mp11::mp_not_fn<
#ifdef __clang__
						is_range_with_iterators // workaround Xcode14 clang segmentation fault
#else
						TRAITFROMCONCEPT(tc::range_with_iterators)
#endif
					>::template fn
				>;
				if constexpr(PureGenerator::found) {
					return PureGenerator::index;
				} else {
					// Heuristically select generator of range with complex iterators.
					return boost::mp11::mp_find_if<
						boost::mp11::mp_list<std::remove_reference_t<Rng>...>,
#ifdef __clang__
						prefers_for_each_workaround // workaround Xcode14 clang segmentation fault
#else
						TRAITFROMCONCEPT(tc::prefers_for_each)
#endif
					>::value % sizeof...(Rng);
				}
			}

		public:
			template<typename... RngRef>
			constexpr zip_adaptor(aggregate_tag_t, RngRef&&... rng) noexcept
				: m_tupleadaptbaserng{{ {{aggregate_tag, tc_move_if_owned(rng)}}... }}
			{}

			template<tc::decayed_derived_from<zip_adaptor> Self, typename Sink>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				zip_detail::no_adl::zip_adaptor_sink<std::remove_reference_t<Self>, tc::decay_t<Sink>, /*bReverse*/false> adaptedsink(self, tc_move_if_owned(sink)); // MAYTHROW
				auto const boc = tc::for_each(
					tc::get<generator_index()>(tc_move_if_owned(self).m_tupleadaptbaserng).base_range(),
					std::ref(adaptedsink)
				);  // MAYTHROW
				_ASSERTE( tc::break_ == boc || adaptedsink.SameLength() );
				return boc;
			}

			template<tc::decayed_derived_from<zip_adaptor> Self, typename Sink>
			friend constexpr auto for_each_reverse_impl(Self&& self, Sink&& sink) MAYTHROW {
				zip_detail::no_adl::zip_adaptor_sink<std::remove_reference_t<Self>, tc::decay_t<Sink>, /*bReverse*/true> adaptedsink(self, tc_move_if_owned(sink)); // MAYTHROW
				auto const boc = tc::for_each(
					tc::reverse(tc::get<generator_index()>(tc_move_if_owned(self).m_tupleadaptbaserng).base_range()),
					std::ref(adaptedsink)
				); // MAYTHROW
				_ASSERTE( tc::break_ == boc || adaptedsink.SameLength() );
				return boc;
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, zip_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> boost::mp11::mp_unique<tc::mp_transform<
				boost::mp11::mp_bind_back<
					zip_adaptor_with_generator_forwarding_tuple_t,
					tc::constant<generator_index()>,
					apply_cvref_to_base_range_t<Rng, Self>...
				>::template fn,
				tc::range_output_t<apply_cvref_to_base_range_t<boost::mp11::mp_at_c<boost::mp11::mp_list<Rng...>, generator_index()>, Self>>
			>> {} // unevaluated

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
				using sized_rng = boost::mp11::mp_front<boost::mp11::mp_filter<trait_from_concept_workaround::has_constexpr_size, boost::mp11::mp_list<Rng...>>>;
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

			static constexpr bool c_bPrefersForEach = zip_adaptor_prefers_for_each<Rng...>::value;

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
		return {tc::aggregate_tag, tc_move_if_owned(rng)...};
	}

	template<typename Rng0, typename Rng1>
	constexpr auto zip_any(Rng0&& rng0, Rng1&& rng1) noexcept {
		return [
			rng0_ = tc::reference_or_value<Rng0>(tc::aggregate_tag, tc_move_if_owned(rng0)),
			rng1_ = tc::reference_or_value<Rng1>(tc::aggregate_tag, tc_move_if_owned(rng1))
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
					tc_return_if_break(tc::continue_if_not_break(sink, tc::tie(*it0, *it1)))
					++it0;
					++it1;
				}
			}
		};
	}

	template<typename Rng> requires tc::instance_n<std::remove_reference_t<Rng>, no_adl::zip_adaptor>
	[[nodiscard]] constexpr decltype(auto) unzip(Rng&& rng) noexcept {
		return std::remove_reference_t<Rng>::base_ranges(tc_move_if_owned(rng));
	}

	template<typename Rng> requires tc::instance_n<std::remove_reference_t<tc::subrange_arg_t<Rng>>, no_adl::zip_adaptor>
	[[nodiscard]] constexpr auto unzip(Rng&& rng) noexcept {
		return tc::tuple_transform(
			tc::zip(
				tc::unzip(tc_move_if_owned(rng).base_range()),
				tc_move_if_owned(rng).begin_index(),
				tc_move_if_owned(rng).end_index()
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
			[rngrng = tc::make_reference_or_value(tc_move_if_owned(rngrng))](auto const n) noexcept {
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
				return tc::for_each(tc_move_if_owned(self).base_range(), [&](auto&& t) noexcept(noexcept(tc_invoke(sink, (std::declval<tc::tuple<int, decltype(t)>>())))) -> decltype(auto) {
					return tc_invoke(sink, (tc::tuple<int, decltype(t)>{i++, tc_move_if_owned(t)})); // MAYTHROW
				});
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, enumerate_generator_adaptor>>* = nullptr> // accept any cvref qualifiers. use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> tc::mp_transform<boost::mp11::mp_bind_front<tc::tuple, int>::template fn, tc::range_output_t<tc::no_adl::apply_cvref_to_base_range_t<Rng, Self>>> {} // unevaluated
		};
	};

	// Transform [t0, t1, ..., tn] to [(0, t0), (1, t1), ..., (n, tn)]
	// TODO: return iterator range, if range has iterators.
	template <typename Rng>
	[[nodiscard]] constexpr auto enumerate(Rng&& rng) noexcept {
		if constexpr( tc::has_size<Rng> ) {
			auto nSize = tc::size(rng); // do not inline, evaluation order important
			return tc::zip(tc::iota(0, nSize), tc_move_if_owned(rng));
		} else {
			return enumerate_adl::enumerate_generator_adaptor<Rng>(tc_move_if_owned(rng));
		}
	}
}
