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

#include "for_each.h"
#include "array.h"

namespace tc {
	namespace iterator_cache_adl_barrier {
		template<typename It>
		struct iterator_cache final : tc::nonmovable /*m_ref may contain pointer into m_it*/ {
		private:
			It m_it;
			tc::reference_or_value< typename std::iterator_traits<It>::reference > m_ref;

		public:
			iterator_cache(It it) noexcept
				: m_it(tc_move(it))
				, m_ref(aggregate_tag(), *m_it)
			{}

			iterator_cache& operator=(It it) & noexcept {
				m_it=tc_move(it);
				tc::renew(m_ref, aggregate_tag(), *m_it);
				return *this;
			}

			auto operator*() const & noexcept return_decltype( *m_ref )
			auto operator*() && noexcept return_decltype_rvalue_by_ref( *tc_move(m_ref) )
			auto operator*() const && noexcept = delete;
		};
	}
	using iterator_cache_adl_barrier::iterator_cache;

	template< typename Rng, typename Func, int... i >
	auto for_each_adjacent_tuple_impl(Rng&& rng, Func func, std::integer_sequence<int, i...>) MAYTHROW -> tc::common_type_t<INTEGRAL_CONSTANT(tc::continue_), decltype(tc::continue_if_not_break(func, *boost::begin(rng), (i, *boost::begin(rng))...))> {
		constexpr int N= sizeof...(i)+1;
		if (tc::size_bounded(rng, N)<N) {
			return INTEGRAL_CONSTANT(tc::continue_)();
		} else {
			auto const itEnd = boost::end(rng);
			auto it = boost::begin(rng);
			// TODO C++17: remove storage_for and use aggregate initialization: aoit(tc::func_tag(), [&](std::size_t) noexcept { return it++; })
			// This workaround is only needed because GCC and Clang require a copy ctor for aggregate initialization, which we cannot provide for iterator_cache.
			// C++17 should solve this, because copy elision is mandatory then.
			tc::array<
				tc::storage_for<
					tc::iterator_cache< 
						typename boost::range_iterator<std::remove_reference_t<Rng>>::type
					>
				>,
				N
			> aoit;
			for(std::size_t n=0; n<N; n++) {
				aoit[n].ctor(it++);
			}
			scope_exit(	tc::for_each(aoit, [](auto const& it) noexcept { it.dtor(); }) )

			for (;;) {
				for (int n = 0; n<N; ++n) {
					if (it == itEnd) {
						return continue_if_not_break(func, *tc_move_always(*aoit[n]), *tc_move_always(*aoit[(n + i + 1) % N])...);
					}
					RETURN_IF_BREAK(continue_if_not_break(func, *tc_move_always(*aoit[n]), **aoit[(n + i + 1) % N]...));
					*aoit[n] = it;
					++it;
				}
			}
		}
	}

	template< int N, typename Rng, typename Func, std::enable_if_t< is_range_with_iterators<Rng>::value >* =nullptr >
	auto for_each_adjacent_tuple(Rng&& rng, Func func) MAYTHROW {
		return for_each_adjacent_tuple_impl(std::forward<Rng>(rng), std::forward<Func>(func), std::make_integer_sequence<int,N-1>());
	}
}
