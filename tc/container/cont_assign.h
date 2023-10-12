
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../algorithm/append.h"
#include "container.h"
#include "cont_reserve.h"
#include "insert.h"

#include <boost/range/algorithm/copy.hpp>

namespace tc {

	////////////////////////
	// generic container algorithms

	static_assert(tc::instance<tc::vector<int>, std::vector>);

	namespace cont_assign_default {
		namespace detail {
			template<typename Cont, typename Rng0, tc::appendable<Cont&>... Rng>
				requires (!std::same_as<tc::iterator_t<Cont>, tc::iterator_t<Cont const>>) // we assume "deep const" <=> "assignment assigns elements"
					&& std::same_as<std::remove_cvref_t<Cont>, Rng0> && tc::safely_assignable_from<Cont&, Rng0&&>
			constexpr void cont_assign_impl(Cont&& cont, Rng0&& rng0, Rng&&... rng) MAYTHROW {
				cont=tc_move_if_owned(rng0);
				if constexpr(0<sizeof...(Rng)) {
					tc::append(cont, tc_move_if_owned(rng)...);
				}
			}
	
			template< typename Cont, typename... Rng>
			constexpr void cont_assign_impl(Cont&& cont, Rng&&... rng) MAYTHROW {
				if constexpr( has_mem_fn_clear<Cont> ) {
					static_assert( std::is_lvalue_reference<Cont>::value );
					cont.clear();
					if constexpr (0<sizeof...(Rng)) {
						if constexpr( has_mem_fn_lower_bound<Cont> || has_mem_fn_hash_function<Cont> ) {
							tc::cont_must_insert_range(cont, tc::concat(tc_move_if_owned(rng)...)); // MAYTHROW
						} else {
							tc::append(cont, tc_move_if_owned(rng)...); // MAYTHROW
						}
					}
				} else {
					auto itOut = tc::begin(cont);
#ifdef _CHECKS
					auto const itEnd = tc::end(cont);
#endif
					tc::for_each(tc::concat(tc_move_if_owned(rng)...), [&](auto&& t) noexcept {
						*VERIFYPRED(itOut, itEnd!=_) = tc_move_if_owned(t);
						++itOut;
					}); // MAYTHROW
					_ASSERT(itEnd==itOut);
				}
			}
		}
		template< typename Cont, typename... Rng>
		constexpr void cont_assign_impl(Cont&& cont, Rng&&... rng) MAYTHROW {
			if( !std::is_constant_evaluated() ) {
				(tc::assert_no_overlap(cont, tc_move_if_owned(rng)), ...);
			}
			detail::cont_assign_impl(tc_move_if_owned(cont), tc_move_if_owned(rng)...); // MAYTHROW
		}
	}

	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(cont_assign)

	template<typename Cont, typename Rng>
	void cont_change_with_or(Cont& cont, Rng const& rng, bool& flag) noexcept {
		cont_change_with_or(cont, rng, flag, true);
	}

	template< typename Cont, typename Rng >
	bool cont_change(Cont& cont, Rng const& rng) noexcept {
		auto itcont=tc::begin(cont);
		auto const itcontEnd=tc::end(cont);
		auto itrng=tc::begin(rng);
		auto const itrngEnd=tc::end(rng);
		for(;;) {
			if( itcont==itcontEnd ) {
				if( itrng==itrngEnd ) {
					return false;
				} else {
					break;
				}
			}
			if( itrng==itrngEnd || !tc::equal_to(*itcont, *itrng) ) {
				tc::take_inplace( cont, itcont );
				break;
			}
			++itcont;
			++itrng;
		}
		tc::append(cont,tc::drop(rng,itrng));
		return true;
	}

	template<typename T, std::size_t N, typename Rng>
	bool cont_change(T (&a)[N], Rng const& rng) noexcept {
		auto it = tc::begin(rng);
		tc::any_accu anyChanged;
		for(std::size_t i=0; i<N; ++i) {
			_ASSERT(it != tc::end(rng));
			anyChanged(tc::change(a[i], *it));
			++it;
		}

		_ASSERTEQUAL(it, tc::end(rng));
		return anyChanged;
	}

	template<typename Cont, typename Rng, typename Flag>
	void cont_change_with_or(Cont& cont, Rng const& rng, Flag& flag, Flag flagChanged) noexcept {
		_ASSERTINITIALIZED(flag);
		if( flag==flagChanged ) {
			tc::cont_assign(cont, rng);
		} else if( tc::cont_change(cont, rng) ) {
			flag=tc_move(flagChanged);
		}
	}
}
