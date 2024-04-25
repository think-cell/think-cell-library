
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../range/meta.h"
#include "../range/iterator_cache.h"
#include "../base/scope.h"
#include "../base/trivial_functors.h"
#include "../storage_for.h"

#include "equal.h"

namespace tc {
	namespace no_adl {
#ifdef _MSC_VER
		template<typename RangeReturn, typename Rng>
		struct element_return_type : std::type_identity<decltype(RangeReturn::pack_no_element(std::declval<Rng>()))> {};
#endif
	}

#ifdef _MSC_VER // MSVC has problems with decltype in alias templates.
	template<typename RangeReturn, typename Rng>
	using element_return_type_t = typename no_adl::element_return_type<RangeReturn, Rng>::type;
#else
	template<typename RangeReturn, typename Rng>
	using element_return_type_t = decltype(RangeReturn::pack_no_element(std::declval<Rng>()));
#endif

	namespace find_first_if_detail {
		template< typename RangeReturn, IF_TC_CHECKS(bool c_bCheckUnique,) typename Rng >
		[[nodiscard]] constexpr tc::element_return_type_t<RangeReturn, Rng> find_first_if(Rng&& rng, auto pred) MAYTHROW {
			if constexpr( RangeReturn::requires_iterator ) {
				auto const itEnd=tc::end(rng); // MAYTHROW
				for( auto it=tc::begin(rng) /*MAYTHROW*/; it!=itEnd; ++it /*MAYTHROW*/ ) {
					if constexpr(std::same_as<decltype(pred), tc::constexpr_function<true>>) { // tc::front
#ifdef _CHECKS
						if constexpr( c_bCheckUnique ) { // tc::only
							_ASSERTE( tc_modified(it, ++_) == itEnd );
						}
#endif
						return RangeReturn::pack_element(tc_move(it),tc_move_if_owned(rng)); // MAYTHROW

					} else {
						decltype(auto) ref = *it; // MAYTHROW
						if (tc::explicit_cast<bool>(tc_invoke(pred, tc::as_const(ref)) /*MAYTHROW*/)) {
#ifdef _CHECKS
							if constexpr( c_bCheckUnique ) {
								for( auto it2 = tc_modified(it, ++_); it2!=itEnd; ++it2 ) { // hand-rolled loop to avoid dependency to subrange
									_ASSERTE( !tc::explicit_cast<bool>(tc_invoke(pred, tc::as_const(*it2))) );
								}
							}
#endif
							return RangeReturn::pack_element(tc_move(it),tc_move_if_owned(rng),tc_move_if_owned(ref)); // MAYTHROW
						}
					}
				}
				return RangeReturn::pack_no_element(tc_move_if_owned(rng));
			} else {
#ifdef _CHECKS
				if constexpr( c_bCheckUnique ) {
					std::optional<tc::element_return_type_t<RangeReturn, Rng>> ot;
					tc::for_each(tc_move_if_owned(rng), [&](auto&& t) MAYTHROW {
						if( tc::explicit_cast<bool>(tc_invoke(pred, tc::as_const(t)) /*MAYTHROW*/) ) {
							VERIFYCRITICALPRED(ot, !_).emplace(RangeReturn::template pack_element<Rng>(tc_move_if_owned(t)) /* MAYTHROW */);
						}
					}); // MAYTHROW
					if( ot ) return *tc_move(ot);
				} else
#endif
				{
					tc::storage_for_without_dtor<tc::element_return_type_t<RangeReturn, Rng>> ot;
					if( tc::break_ == tc::for_each(tc_move_if_owned(rng), [&](auto&& t) MAYTHROW {
						if (tc::explicit_cast<bool>(tc_invoke(pred, tc::as_const(t)) /*MAYTHROW*/)) {
							ot.ctor(RangeReturn::template pack_element<Rng>(tc_move_if_owned(t)) /* MAYTHROW */);
							return tc::break_; // We assume for_each never throws exceptions after the sink returned break_.
						} else {
							return tc::continue_;
						}
					}) /* MAYTHROW */ ) {
						tc_scope_exit { ot.dtor(); };
						return *tc_move(ot);
					}
				}
				return RangeReturn::template pack_no_element<Rng>();
			}
		}
	}

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity >
	[[nodiscard]] constexpr decltype(auto) find_first_if(Rng&& rng, Pred&& pred = Pred()) MAYTHROW {
		return find_first_if_detail::find_first_if<RangeReturn IF_TC_CHECKS(, /*c_bCheckUnique*/false)>(tc_move_if_owned(rng), tc_move_if_owned(pred));
	}

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity >
	[[nodiscard]] constexpr decltype(auto) find_unique_if(Rng&& rng, Pred&& pred = Pred()) MAYTHROW {
		return find_first_if_detail::find_first_if<RangeReturn IF_TC_CHECKS(, /*c_bCheckUnique*/true)>(tc_move_if_owned(rng), tc_move_if_owned(pred));
	}

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity >
	[[nodiscard]] constexpr decltype(auto) find_last_if(Rng&& rng, Pred pred = Pred()) MAYTHROW {
		if constexpr( tc::bidirectional_range<Rng> && tc::common_range<Rng> ) {
			auto const itBegin=tc::begin(rng);
			for( auto it=tc::end(rng); it!=itBegin; ) {
				--it;
				if constexpr(std::same_as<Pred, tc::constexpr_function<true>>) { // tc::back
					return RangeReturn::pack_element(tc_move(it), tc_move_if_owned(rng));
				} else {
					decltype(auto) ref = *it;
					if (tc::explicit_cast<bool>(tc_invoke(pred, tc::as_const(ref)))) {
						return RangeReturn::pack_element(tc_move(it), tc_move_if_owned(rng), tc_move_if_owned(ref));
					}
				}
			}
		} else {
			auto const itEnd=tc::end(rng);
			for( auto it=tc::begin(rng); it!=itEnd; ++it ) {
				std::array<tc::storage_for_without_dtor<tc::iterator_cache<decltype(it)>>, 2> aic; // tc::storage_for was too slow in debug builds
				int iFound = 0;
				aic[iFound].ctor(it);
				tc_scope_exit { aic[iFound].dtor(); }; //iFound captured by reference
				if (tc::explicit_cast<bool>(tc_invoke(pred, tc::as_const(**aic[iFound])))) {
					for (;;) {
						++it;
						if (itEnd==it) break;
						aic[1 - iFound].ctor(it);
						tc_scope_exit { aic[1 - iFound].dtor(); };
						if (tc_invoke(pred, tc::as_const(**aic[1 - iFound]))) {
							iFound = 1 - iFound;
						}
					}
					
					return RangeReturn::pack_element(
						tc_move_always(*aic[iFound]).m_it_(), // pack_element must not take iterator by value
						tc_move_if_owned(rng),
						*tc_move_always(*aic[iFound])
					);
				}
			}
		}
		return RangeReturn::pack_no_element(tc_move_if_owned(rng));
	}

	namespace no_adl {
		BOOST_MPL_HAS_XXX_TRAIT_DEF(key_type)
	}
	using no_adl::has_key_type;

	namespace find_first_or_unique_default {
		template< typename RangeReturn, IF_TC_CHECKS(bool c_bCheckUnique,) typename Rng, typename T >
		[[nodiscard]] constexpr decltype(auto) find_first_or_unique_impl(std::type_identity<RangeReturn>, IF_TC_CHECKS(tc::constant<c_bCheckUnique>,) Rng&& rng, T const& t) MAYTHROW {
			static_assert(
				!tc::has_key_type<std::remove_cvref_t<Rng>>::value,
				"Do you want to use tc::cont_find?"
			);
			return find_first_if_detail::find_first_if<RangeReturn IF_TC_CHECKS(, c_bCheckUnique)>(tc_move_if_owned(rng), [&](auto const& _) MAYTHROW { return tc::equal_to(_, t); });
		}
	}
	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(find_first_or_unique)

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] constexpr decltype(auto) find_unique(Rng&& rng, T&& t) MAYTHROW {
		return find_first_or_unique(std::type_identity<RangeReturn>(), IF_TC_CHECKS(tc::constant<true>(),) tc_move_if_owned(rng), tc_move_if_owned(t));
	}

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] constexpr decltype(auto) find_first(Rng&& rng, T&& t) MAYTHROW {
		return find_first_or_unique(std::type_identity<RangeReturn>(), IF_TC_CHECKS(tc::constant<false>(),) tc_move_if_owned(rng), tc_move_if_owned(t));
	}

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] constexpr decltype(auto) find_last(Rng&& rng, T const& t) noexcept {
		return tc::find_last_if<RangeReturn>( tc_move_if_owned(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}
}
