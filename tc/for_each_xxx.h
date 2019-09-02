
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "for_each.h"
#include "array.h"
#include "size_bounded.h"
#include "format.h"
#include "accessors.h"

namespace tc {
	namespace no_adl {
		template<typename It>
		struct iterator_cache final : tc::nonmovable /*m_ref may contain pointer into m_it*/ {
		private:
			DEFINE_MEMBER_AND_ACCESSORS(It, iterator, m_it)
			tc::reference_or_value< typename std::iterator_traits<It>::reference > m_ref;

		public:
			iterator_cache(It it) noexcept
				: m_it(tc_move(it))
				, m_ref(aggregate_tag, *m_it)
			{}

			iterator_cache& operator=(It it) & noexcept {
				m_it=tc_move(it);
				tc::renew(m_ref, aggregate_tag, *m_it);
				return *this;
			}

			auto operator*() const & noexcept return_decltype( *m_ref )
			auto operator*() && noexcept return_decltype_xvalue_by_ref( *tc_move(m_ref) )
			auto operator*() const && noexcept = delete;
		};
	}
	using no_adl::iterator_cache;

	template< typename Rng, typename Func, int... i >
	auto for_each_adjacent_tuple_impl(Rng&& rng, Func func, std::integer_sequence<int, i...>) MAYTHROW -> tc::common_type_t<INTEGRAL_CONSTANT(tc::continue_), decltype(tc::continue_if_not_break(func, *tc::begin(rng), (i, *tc::begin(rng))...))> {
		constexpr int N= sizeof...(i)+1;
		if (tc::size_bounded(rng, N)<N) {
			return INTEGRAL_CONSTANT(tc::continue_)();
		} else {
			auto const itEnd = tc::end(rng);
			auto it = tc::begin(rng);
			tc::array<
				tc::iterator_cache< 
					typename boost::range_iterator<Rng>::type
				>,
				N
			> ait(tc::func_tag, [&](std::size_t) noexcept { return it++; });

			for (;;) {
				for (int n = 0; n<N; ++n) {
					if (it == itEnd) {
						return continue_if_not_break(func, *tc_move_always(ait[n]), *tc_move_always(ait[(n + i + 1) % N])...);
					}
					RETURN_IF_BREAK(continue_if_not_break(func, *tc_move_always(ait[n]), *ait[(n + i + 1) % N]...));
					ait[n] = it;
					++it;
				}
			}
		}
	}

	template< int N, typename Rng, typename Func, std::enable_if_t< is_range_with_iterators<Rng>::value >* =nullptr >
	auto for_each_adjacent_tuple(Rng&& rng, Func func) MAYTHROW {
		return for_each_adjacent_tuple_impl(std::forward<Rng>(rng), std::forward<Func>(func), std::make_integer_sequence<int,N-1>());
	}

	namespace restrict_size_impl {
		struct [[maybe_unused]] SRestrictSizeDummy final { };

#ifdef _CHECKS
		template<typename Rng>
		struct [[maybe_unused]] SRestrictSize final {
			explicit SRestrictSize(Rng const& rng, typename boost::range_size<Rng>::type nSizeMin, typename boost::range_size<Rng>::type nSizeMax) noexcept
				: m_rng(rng)
				, m_nSizeMin(tc_move(nSizeMin))
				, m_nSizeMax(tc_move(nSizeMax))
			{}

			~SRestrictSize() noexcept{
				auto const nSize = tc::size(m_rng);
				_ASSERTPRINT(m_nSizeMin <= nSize, "nSize=", tc::as_dec(nSize), "; m_nSizeMin=", tc::as_dec(m_nSizeMin));
				_ASSERTPRINT(nSize <= m_nSizeMax, "nSize=", tc::as_dec(nSize), "; m_nSizeMax=", tc::as_dec(m_nSizeMax));
			}

		private:
			Rng const& m_rng;
			typename boost::range_size<Rng>::type m_nSizeMin;
			typename boost::range_size<Rng>::type m_nSizeMax;
		};

		template< typename Rng >
		SRestrictSize<Rng> RestrictSizeDecrementImpl(Rng const& rng, typename boost::range_size<Rng>::type nDecrementMin, typename boost::range_size<Rng>::type nDecrementMax) noexcept {
			auto const nSize = tc::size(rng);
			return SRestrictSize<Rng>(
				rng,
				nDecrementMax < nSize ? nSize - nDecrementMax : tc::explicit_cast<typename boost::range_size<Rng>::type>(0),
				nDecrementMin < nSize ? nSize - nDecrementMin : tc::explicit_cast<typename boost::range_size<Rng>::type>(0)
			);
		}
#endif

		template< typename Rng >
		auto restrict_size_decrement(Rng const& rng, typename boost::range_size<Rng>::type nDecrementMin, typename boost::range_size<Rng>::type nDecrementMax) {
#ifdef _CHECKS
			if constexpr(tc::has_size<Rng const&>::value) {
				return RestrictSizeDecrementImpl(rng, nDecrementMin, nDecrementMax);
			} else
#endif
			{
				return SRestrictSizeDummy{};
			}
		}

		template< typename Rng >
		auto restrict_size_decrement(Rng const& rng) {
			return restrict_size_decrement(rng, 1, 1);
		}
	}
	using restrict_size_impl::restrict_size_decrement;


	/////////////////////////////////////////////////////
	// for_each_may_remove_current

	// enable_if to ensure that removal preserves iterators would be nice, but is difficult for adapted ranges.
	template< typename Rng, typename Func >
	auto for_each_may_remove_current(Rng&& rng, Func func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(rng))), INTEGRAL_CONSTANT(tc::continue_)> {
		static_assert( !tc::range_filter_by_move_element< std::remove_reference_t<Rng> >::value );
		auto it=tc::begin(rng);
		auto const itEnd=tc::end(rng);
		while( it!=itEnd ) {
			auto const rsize = restrict_size_decrement(rng, 0, 1);
			RETURN_IF_BREAK(tc::continue_if_not_break(func, *it++));
		}
		return INTEGRAL_CONSTANT(tc::continue_)();
	}

	DEFINE_FN(for_each_may_remove_current);

	/////////////////////////////////////////////////////
	// for_each_ordered_pair

	template< typename Rng, typename Func >
	auto for_each_ordered_pair(Rng const& rng, Func func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(rng), *tc::begin(rng))), INTEGRAL_CONSTANT(tc::continue_)> {
		auto const itEndRng = tc::end(rng);
		for(auto itEnd = tc::begin(rng); itEnd != itEndRng; ++itEnd) {
			tc::reference_or_value<tc::range_reference_t<Rng const>> ref(aggregate_tag, *itEnd);

			RETURN_IF_BREAK(
				tc::for_each(
					tc::take(rng, itEnd),
					[&](auto const& _) MAYTHROW { return func(_, *ref); }
				)
			);
		}
		return INTEGRAL_CONSTANT(tc::continue_)();
	}

	namespace no_adl {
		template<typename T, typename Func>
		struct for_each_adjacent_pair_fn final {
		private:
			Func& m_func;
			std::optional<T> m_oparam;
		public:
			for_each_adjacent_pair_fn(Func& func) noexcept
				: m_func(func)
			{}

			template<typename U>
			auto operator()(U&& u) & MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(m_func, *m_oparam, u)), INTEGRAL_CONSTANT(tc::continue_)> {
				if (m_oparam) {
					RETURN_IF_BREAK(tc::continue_if_not_break(m_func, *m_oparam, u));
				}
				m_oparam.emplace(std::forward<U>(u));

				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		};
	}

	template<typename Rng, typename Func, std::enable_if_t<!is_range_with_iterators<Rng>::value>* = nullptr>
	auto for_each_adjacent_pair(Rng&& rng, Func func) MAYTHROW {
		return tc::for_each( std::forward<Rng>(rng), no_adl::for_each_adjacent_pair_fn<tc::range_value_t<Rng>, Func>(func) );
	}

	template<typename Rng, typename FuncBegin, typename FuncElem, typename FuncSeparator, typename FuncEnd>
	constexpr auto framed_for_each(Rng&& rng, FuncBegin funcBegin, FuncElem funcElement, FuncSeparator funcSeparator, FuncEnd funcEnd) MAYTHROW {
		bool bNotEmpty = false;
		auto const breakorcontinue = tc::for_each(std::forward<Rng>(rng), [&](auto&& t) MAYTHROW {
			if(tc::change(bNotEmpty, true)) {
				RETURNS_VOID(funcBegin());
			} else {
				RETURNS_VOID(funcSeparator());
			}
			return tc::continue_if_not_break(funcElement, std::forward<decltype(t)>(t));
		});
		if(bNotEmpty) {
			RETURNS_VOID(funcEnd());
		}
		return breakorcontinue;
	}
}
