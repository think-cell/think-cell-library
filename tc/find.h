
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "equal.h"
#include "meta.h"
#include "scope.h"
#include "subrange.h"
#include "storage_for.h"
#include "iterator_cache.h"

namespace tc {

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity, std::enable_if_t<RangeReturn::requires_iterator>* = nullptr >
	[[nodiscard]] constexpr decltype(auto) find_first_if(Rng&& rng, Pred&& pred = Pred()) MAYTHROW {
		auto const itEnd=tc::end(rng);
		for( auto it=tc::begin(rng); it!=itEnd; ++it ) {
			auto && ref = *it;
			if (tc::bool_cast(tc::invoke(pred, tc::as_const(ref)))) {
				return RangeReturn::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
		}
		return RangeReturn::pack_no_element(std::forward<Rng>(rng));
	}

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity, std::enable_if_t<
		!RangeReturn::requires_iterator &&
		std::is_trivially_default_constructible<tc::element_return_type_t<RangeReturn, Rng>>::value &&
		std::is_trivially_move_constructible<tc::element_return_type_t<RangeReturn, Rng>>::value
	>* = nullptr >
	[[nodiscard]] constexpr tc::element_return_type_t<RangeReturn, Rng> find_first_if(Rng&& rng, Pred&& pred = Pred()) MAYTHROW {
		tc::element_return_type_t<RangeReturn, Rng> rt{};
		if(tc::break_ == tc::for_each(std::forward<Rng>(rng), [&](auto&& t) noexcept {
			if (tc::bool_cast(tc::invoke(pred, tc::as_const(t)))) {
				rt=RangeReturn::template pack_element<Rng>(std::forward<decltype(t)>(t));
				return tc::break_;
			} else {
				return tc::continue_;
			}
		})) {
			return rt;
		} else {
			return RangeReturn::template pack_no_element<Rng>();
		}
	}

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity, std::enable_if_t<
		!RangeReturn::requires_iterator &&
		!(std::is_trivially_default_constructible<tc::element_return_type_t<RangeReturn, Rng>>::value &&
		std::is_trivially_move_constructible<tc::element_return_type_t<RangeReturn, Rng>>::value)
	>* = nullptr >
	[[nodiscard]] constexpr tc::element_return_type_t<RangeReturn, Rng> find_first_if(Rng&& rng, Pred&& pred = Pred()) MAYTHROW {
		tc::storage_for<tc::element_return_type_t<RangeReturn, Rng>> ot;
		if(tc::break_ == tc::for_each(std::forward<Rng>(rng), [&](auto&& t) noexcept {
			if (tc::bool_cast(tc::invoke(pred, tc::as_const(t)))) {
				ot.ctor(RangeReturn::template pack_element<Rng>(std::forward<decltype(t)>(t)));
				return tc::break_;
			} else {
				return tc::continue_;
			}
		})) {
			scope_exit(ot.dtor());
			return *tc_move(ot);
		} else {
			return RangeReturn::template pack_no_element<Rng>();
		}
	}

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity, std::enable_if_t<RangeReturn::requires_iterator>* = nullptr >
#ifdef _CHECKS
	[[nodiscard]] constexpr decltype(auto) find_unique_if(Rng&& rng, Pred pred = Pred()) noexcept {
		auto const itEnd=tc::end(rng);
		for( auto it=tc::begin(rng); it!=itEnd; ++it ) {
			auto && ref=*it;
			if( tc::bool_cast(tc::invoke(pred, tc::as_const(ref))) ) {
				_ASSERTE( !tc::find_first_if<tc::return_bool>( tc::drop(rng, modified(it, ++_)), pred) );
				return RangeReturn::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
		}
		return RangeReturn::pack_no_element(std::forward<Rng>(rng));
#else
	[[nodiscard]] constexpr decltype(auto) find_unique_if(Rng&& rng, Pred&& pred) noexcept {
		return find_first_if<RangeReturn>(std::forward<Rng>(rng), std::forward<Pred>(pred));
#endif
	}

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity, std::enable_if_t<
		!RangeReturn::requires_iterator 
		&& std::is_trivially_default_constructible<tc::element_return_type_t<RangeReturn, Rng>>::value 
		&& std::is_trivially_move_constructible<tc::element_return_type_t<RangeReturn, Rng>>::value
	>* = nullptr >
#ifdef _CHECKS
	[[nodiscard]] constexpr decltype(auto) find_unique_if(Rng&& rng, Pred pred = Pred()) noexcept {
		tc::element_return_type_t<RangeReturn, Rng> rt{};
		bool bFound = false;
		tc::for_each(std::forward<Rng>(rng), [&](auto&& t) noexcept {
			if (tc::bool_cast(tc::invoke(pred, tc::as_const(t)))) {
				VERIFY(tc::change(bFound, true));
				rt=RangeReturn::template pack_element<Rng>(std::forward<decltype(t)>(t));
			}
		});
		if(bFound) {
			return rt;
		} else {
			return RangeReturn::template pack_no_element<Rng>();
		}
#else
	[[nodiscard]] constexpr decltype(auto) find_unique_if(Rng&& rng, Pred&& pred = Pred()) noexcept {
		return find_first_if<RangeReturn>(std::forward<Rng>(rng), std::forward<Pred>(pred));
#endif
	}

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity, std::enable_if_t<
		!RangeReturn::requires_iterator 
		&& !(std::is_trivially_default_constructible<tc::element_return_type_t<RangeReturn, Rng>>::value 
			&& std::is_trivially_move_constructible<tc::element_return_type_t<RangeReturn, Rng>>::value)
	>* = nullptr >
#ifdef _CHECKS
	[[nodiscard]] constexpr tc::element_return_type_t<RangeReturn, Rng> find_unique_if(Rng&& rng, Pred pred = Pred()) noexcept {
		tc::storage_for<tc::element_return_type_t<RangeReturn, Rng>> ot;
		bool bFound = false;
		tc::for_each(std::forward<Rng>(rng), [&](auto&& t) noexcept {
			if (tc::bool_cast(tc::invoke(pred, tc::as_const(t)))) {
				VERIFY(tc::change(bFound, true));
				ot.ctor(RangeReturn::template pack_element<Rng>(std::forward<decltype(t)>(t)));
			} 
		});
		if(bFound) {
			scope_exit(ot.dtor());
			return *tc_move(ot);
		} else {
			return RangeReturn::template pack_no_element<Rng>();
		}
#else
	[[nodiscard]] constexpr decltype(auto) find_unique_if(Rng&& rng, Pred&& pred = Pred()) noexcept {
		return find_first_if<RangeReturn>(std::forward<Rng>(rng), std::forward<Pred>(pred));
#endif
	}

	namespace find_last_if_detail {
		template< typename RangeReturn, typename Rng, typename Pred >
		[[nodiscard]] constexpr decltype(auto) find_last_if(Rng&& rng, Pred pred, boost::iterators::bidirectional_traversal_tag) noexcept {
			auto itBegin=tc::begin(rng);
			for( auto it=tc::end(rng); it!=itBegin; ) {
				--it;
				auto && ref = *it;
				if (tc::bool_cast(tc::invoke(pred, tc::as_const(ref)))) return RangeReturn::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
			return RangeReturn::pack_no_element(std::forward<Rng>(rng));
		}

		template< typename RangeReturn, typename Rng, typename Pred >
		[[nodiscard]] constexpr decltype(auto) find_last_if(Rng&& rng, Pred pred, boost::iterators::forward_traversal_tag) noexcept {
			auto const itEnd=tc::end(rng);
			for( auto it=tc::begin(rng); it!=itEnd; ++it ) {
				std::array<tc::storage_for<tc::iterator_cache<decltype(it)>>, 2> aic;
				int iFound = 0;
				aic[iFound].ctor(it);
				scope_exit(aic[iFound].dtor()); //iFound captured by reference
				if (tc::bool_cast(tc::invoke(pred, tc::as_const(**aic[iFound])))) {
					for (;;) {
						++it;
						if (itEnd==it) break;
						aic[1 - iFound].ctor(it);
						if (tc::invoke(pred, tc::as_const(**aic[1 - iFound]))) {
							iFound = 1 - iFound;
						}
						aic[1 - iFound].dtor();
					}
					
					return RangeReturn::pack_element(
						aic[iFound]->m_it_(), // do not move because the iterator must stay alive for the reference to stay valid
						std::forward<Rng>(rng),
						*tc_move_always(*aic[iFound])
					);
				}
			}
			return RangeReturn::pack_no_element(std::forward<Rng>(rng));
		}
	}

	template< typename RangeReturn, typename Rng, typename Pred = tc::identity >
	[[nodiscard]] constexpr decltype(auto) find_last_if(Rng&& rng, Pred&& pred = Pred()) noexcept {
		return find_last_if_detail::find_last_if<RangeReturn>(
			std::forward<Rng>(rng),
			std::forward<Pred>(pred),
			typename boost::range_traversal<Rng>::type()
		);
	}

	template< typename Rng, typename It, typename Func >
	auto for_each_iterator_pair_outwards(Rng&& rng, It itOrigin, bool bSkipSelf, Func func) noexcept
		-> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<tc::ptr_range<It> const&>())), INTEGRAL_CONSTANT(tc::continue_)>
	{
		std::array<It, 2> const aitLimit = { tc::begin(rng), tc::end(rng) };
		std::array<It, 2> ait = { itOrigin, itOrigin };

		if (!bSkipSelf) {
			_ASSERT(tc::back(aitLimit) != tc::back(ait));
			RETURN_IF_BREAK(tc::continue_if_not_break(func, tc::begin_next<tc::return_take>(tc::as_const(ait))));
			++tc::back(ait);
		} else if(tc::back(aitLimit) != tc::back(ait)) {
			++tc::back(ait);
		}

		for (;;) {
			if (tc::front(aitLimit) == tc::front(ait)) {
				for (; tc::back(ait) != tc::back(aitLimit); ++tc::back(ait)) {
					RETURN_IF_BREAK(tc::continue_if_not_break(func, tc::begin_next<tc::return_drop>(tc::as_const(ait))));
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
			if (tc::back(aitLimit) == tc::back(ait)) {
				for (; tc::front(ait) != tc::front(aitLimit); ) {
					--tc::front(ait);
					RETURN_IF_BREAK(tc::continue_if_not_break(func, tc::begin_next<tc::return_take>(tc::as_const(ait))));
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
			--tc::front(ait);
			RETURN_IF_BREAK(tc::continue_if_not_break(func, tc::as_const(ait)));
			++tc::back(ait);
		}
	}

	template < typename RangeReturn, typename Rng, typename It, typename Pred = tc::identity>
	[[nodiscard]] constexpr decltype(auto) find_closest_if(Rng&& rng, It it, bool bSkipSelf, Pred pred = Pred()) noexcept {
		tc::storage_for<tc::iterator_cache<It>> oitc;
		if (tc::break_ == tc::for_each_iterator_pair_outwards(rng, tc_move(it), bSkipSelf, [&](auto const& rngit) noexcept {
			return tc::for_each(rngit, [&](auto const& it) noexcept {
				oitc.ctor(it);
				if (tc::bool_cast(tc::invoke(pred, tc::as_const(**oitc)))) { return tc::break_; }
				oitc.dtor();
				return tc::continue_;
			});
		})) {
			scope_exit(oitc.dtor());
			return RangeReturn::pack_element(oitc->m_it_(), std::forward<Rng>(rng), **tc_move(oitc));
		} else {
			return RangeReturn::pack_no_element(std::forward<Rng>(rng));
		}
	}

	template < typename RangeReturn, typename Rng, typename Index, typename Pred = tc::identity>
	decltype(auto) find_closest_if_with_index(Rng&& rng, Index&& n, bool bSkipSelf, Pred&& pred = Pred()) noexcept {
		return find_closest_if<RangeReturn>(std::forward<Rng>(rng), tc::begin_next<tc::return_border>(rng, std::forward<Index>(n)), bSkipSelf, std::forward<Pred>(pred));
	}
	
	namespace no_adl {
		BOOST_MPL_HAS_XXX_TRAIT_DEF(key_type)
	}
	using no_adl::has_key_type;

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] constexpr decltype(auto) find_unique(Rng&& rng, T const& t) noexcept {
		static_assert(
			!tc::has_key_type<tc::remove_cvref_t<Rng>>::value,
			"Do you want to use tc::cont_find?"
		);
		return find_unique_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] constexpr decltype(auto) find_first(Rng&& rng, T const& t) noexcept {
		static_assert(
			!tc::has_key_type<tc::remove_cvref_t<Rng>>::value,
			"Do you want to use tc::cont_find?"
		);
		return tc::find_first_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] constexpr decltype(auto) find_last(Rng&& rng, T const& t) noexcept {
		return tc::find_last_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}
	
	template <typename RangeReturn, typename Rng, std::enable_if_t<!RangeReturn::requires_iterator>* = nullptr>
	[[nodiscard]] decltype(auto) front(Rng&& rng) noexcept {
		static_assert( !std::is_same<RangeReturn, tc::return_void>::value );
		static_assert( !std::is_same<RangeReturn, tc::return_bool>::value, "Use tc::empty instead of tc::front<tc::return_bool>" );
		return tc::find_first_if<RangeReturn>(std::forward<Rng>(rng), MAKE_CONSTEXPR_FUNCTION(true));
	}

	template <typename RangeReturn, typename Rng, std::enable_if_t<!RangeReturn::requires_iterator>* = nullptr>
	[[nodiscard]] decltype(auto) back(Rng&& rng) noexcept {
		static_assert( !std::is_same<RangeReturn, tc::return_void>::value );
		static_assert( !std::is_same<RangeReturn, tc::return_bool>::value, "Use tc::empty instead of tc::back<tc::return_bool>" );
		static_assert( tc::is_bidirectional_range<Rng>::value ); // TODO reverse generator would also be fine, but find_last_if is not specialized for this case yet.
		return tc::find_last_if<RangeReturn>(std::forward<Rng>(rng), MAKE_CONSTEXPR_FUNCTION(true));
	}

	template <typename RangeReturn, typename Rng, std::enable_if_t<!RangeReturn::requires_iterator>* = nullptr>
	[[nodiscard]] decltype(auto) linear_back(Rng&& rng) noexcept {
		static_assert( !std::is_same<RangeReturn, tc::return_void>::value );
		static_assert( !std::is_same<RangeReturn, tc::return_bool>::value, "Use tc::empty instead of tc::linear_back<tc::return_bool>" );
		static_assert( !tc::is_bidirectional_range<Rng>::value, "Use tc::back for bidirectional ranges" );
		return tc::find_last_if<RangeReturn>(std::forward<Rng>(rng), MAKE_CONSTEXPR_FUNCTION(true));
	}
	
	template< typename RangeReturn, typename RngLhs, typename RngRhs>
	[[nodiscard]] constexpr decltype(auto) longest_common_prefix(RngLhs&& rnglhs, RngRhs&& rngrhs) MAYTHROW {
		static_assert(RangeReturn::allowed_if_always_has_border);

		auto_cref(itlhsEnd, tc::end(rnglhs));
		auto_cref(itrhsEnd, tc::end(rngrhs));
		auto itlhs=tc::begin(rnglhs);
		auto itrhs=tc::begin(rngrhs);
		while(itlhs < itlhsEnd && itrhs < itrhsEnd && tc::equal_to_or_parse_match(*itlhs, *itrhs)) {
			++itlhs;
			++itrhs;
		}
		return std::make_pair(
			RangeReturn::pack_border(itlhs, std::forward<RngLhs>(rnglhs)),
			RangeReturn::pack_border(itrhs, std::forward<RngRhs>(rngrhs))
		);
	}
}