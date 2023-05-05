
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "find.h"
#include "../range/subrange.h"

namespace tc {
	template <typename RangeReturn>
	[[nodiscard]] constexpr decltype(auto) front(auto&& rng) noexcept {
		return tc::find_first_if<RangeReturn>(tc_move_if_owned(rng), tc::constexpr_function<true>());
	}

	template <typename RangeReturn>
	[[nodiscard]] constexpr decltype(auto) back(auto&& rng) noexcept {
		static_assert( !std::is_same<RangeReturn, tc::return_void>::value );
		static_assert( !std::is_same<RangeReturn, tc::return_bool>::value, "Use tc::empty instead of tc::back<tc::return_bool>" );
		static_assert( tc::bidirectional_range<std::remove_reference_t<decltype(rng)>> ); // TODO reverse generator would also be fine, but find_last_if is not specialized for this case yet.
		return tc::find_last_if<RangeReturn>(tc_move_if_owned(rng), tc::constexpr_function<true>());
	}

	template <typename RangeReturn>
	[[nodiscard]] decltype(auto) linear_back(auto&& rng) noexcept {
		static_assert( !std::is_same<RangeReturn, tc::return_void>::value );
		static_assert( !std::is_same<RangeReturn, tc::return_bool>::value, "Use tc::empty instead of tc::linear_back<tc::return_bool>" );
		static_assert( !tc::bidirectional_range<std::remove_reference_t<decltype(rng)>>, "Use tc::back for bidirectional ranges" );
		auto it = tc::begin(rng);
		tc_auto_cref(itEnd, tc::end(rng));
		if (it!=itEnd) {
			auto itNext = it;
			while (itEnd!=++itNext) {
				it = itNext;
			}
			return RangeReturn::pack_element(tc_move(it), tc_move_if_owned(rng));
		} else {
			return RangeReturn::pack_no_element(tc_move_if_owned(rng));
		}
	}

	template <typename RangeReturn>
	[[nodiscard]] constexpr auto at(auto&& rng, typename boost::range_size< std::remove_reference_t<decltype(rng)> >::type n) noexcept {
		static_assert( !std::is_same<RangeReturn, tc::return_void>::value );
		static_assert( !std::is_same<RangeReturn, tc::return_bool>::value, "Use tc::size instead of tc::at<tc::return_bool>" );
		if (n<tc::size_raw(rng)) {
			_ASSERTE(0<=n);
			auto it=tc::begin(rng);
			it += n;
			return RangeReturn::pack_element(tc_move(it), tc_move_if_owned(rng));
		} else {
			return RangeReturn::pack_no_element(tc_move_if_owned(rng));
		}
	}

	template <typename RangeReturn>
	[[nodiscard]] constexpr auto linear_at(auto&& rng, typename boost::range_size< std::remove_reference_t<decltype(rng)> >::type n) noexcept {
		static_assert( !std::is_same<RangeReturn, tc::return_void>::value );
		static_assert( !std::is_same<RangeReturn, tc::return_bool>::value, "Use tc::size_bounded instead of tc::linear_at<tc::return_bool>" );
		if constexpr (std::convertible_to<typename boost::range_traversal<decltype(rng)>::type, boost::iterators::random_access_traversal_tag>) {
			return tc::at<RangeReturn>(tc_move_if_owned(rng), tc_move(n));
		} else {
			_ASSERTE(0<=n);
			tc_auto_cref(itEnd, tc::end(rng));
			for (auto it=tc::begin(rng); it!=itEnd; ++it, --n) {
				if (0==n) {
					return RangeReturn::pack_element(tc_move(it), tc_move_if_owned(rng));
				}
			}
			return RangeReturn::pack_no_element(tc_move_if_owned(rng));
		}
	}

	template <typename RangeReturn>
	[[nodiscard]] constexpr auto reverse_at(auto&& rng, typename boost::range_size< std::remove_reference_t<decltype(rng)> >::type n) noexcept {
		if (n<tc::size_raw(rng)) {
			_ASSERTE(0<=n);
			auto it=tc::end(rng);
			it -= n+1;
			return RangeReturn::pack_element(tc_move(it), tc_move_if_owned(rng));
		} else {
			return RangeReturn::pack_no_element(tc_move_if_owned(rng));
		}
	}

	namespace only_detail {
		template<typename RangeReturn, typename Rng>
		tc::element_return_type_t<RangeReturn, Rng> only_not_X(Rng&& rng, auto func) MAYTHROW {
			if constexpr(RangeReturn::requires_iterator) {
				auto const itEnd = tc::end(rng);
				auto const itBegin = tc::begin(rng);
				if(itEnd != itBegin && (itEnd == tc_modified(itBegin, ++_) || tc::explicit_cast<bool>(func()))) {
					return RangeReturn::pack_element(itBegin, tc_move_if_owned(rng));
				} else {
					return RangeReturn::pack_no_element(tc_move_if_owned(rng));
				}
			} else {
				std::optional<tc::element_return_type_t<RangeReturn, Rng>> ot;
				auto const breakorcontinue = tc::for_each(tc_move_if_owned(rng), [&](auto&& t) MAYTHROW {
					if(!ot) {
						ot.emplace(RangeReturn::template pack_element<Rng>(tc_move_if_owned(t))); // MAYTHROW
						return tc::continue_;
					} else {
						return tc::break_;
					}
				});
				if(ot && (tc::continue_ == breakorcontinue || tc::explicit_cast<bool>(func()))) {
					return *tc_move(ot);
				} else {
					return RangeReturn::template pack_no_element<Rng>();
				}
			}
		}
	}

	template<typename RangeReturn>
	[[nodiscard]] constexpr decltype(auto) only(auto&& rng) MAYTHROW {
		static_assert(std::is_same<RangeReturn, tc::return_value>::value || std::is_same<RangeReturn, tc::return_element>::value, "decide between tc::only_not_0<...> and tc::only_not_N<...>"); // Exclude tc::return_XXX_or_YYY. TODO: make static_assert more generic
		return tc::find_unique_if<RangeReturn>(tc_move_if_owned(rng), tc::constexpr_function<true>());
	}

	template<typename RangeReturn>
	[[nodiscard]] decltype(auto) only_not_0(auto&& rng) MAYTHROW {
		static_assert(!std::is_same<RangeReturn, tc::return_value>::value && !std::is_same<RangeReturn, tc::return_element>::value, "use tc::only<...> instead"); // TODO: make static_assert more generic
		return only_detail::only_not_X<RangeReturn>(tc_move_if_owned(rng), tc::never_called<tc::constant<true>>());
	}

	template<typename RangeReturn>
	[[nodiscard]] decltype(auto) only_not_N(auto&& rng) MAYTHROW {
		static_assert(!std::is_same<RangeReturn, tc::return_value>::value && !std::is_same<RangeReturn, tc::return_element>::value, "use tc::only<...> instead"); // TODO: make static_assert more generic
		return only_detail::only_not_X<RangeReturn>(tc_move_if_owned(rng), tc::constexpr_function<false>());
	}

	namespace no_adl{
		template<typename Element>
		struct element_stash_impl final {
			static_assert(tc::decayed<Element>);
			[[nodiscard]] static constexpr decltype(auto) extract(Element&& elem) noexcept(noexcept(*tc_move(elem))) {
#ifdef _DEBUG
				if constexpr(std::is_reference<decltype(*elem)>::value) {
					_ASSERTE(std::addressof(*tc::decay_copy(elem))==std::addressof(*elem)); // specialize is_stashing_element<Element>
				}
#endif
				return *tc_move(elem);
			}
		};

#ifdef _MSC_VER
		// If Element is of type T(*)[N], MSVC erroneously deduces return type T(*)[N] for element_stash_impl<Element>::extract,
		// and then raises a compiler error that it cannot convert from T[N] to T(*)[N] in the return statement.
		// Clang correctly deduces return type T(&)[N]. Add specialization for array types to workaround this problem.
		template<typename T, std::size_t N>
		struct element_stash_impl<T(*)[N]> final {
			using array_type=T[N];
			[[nodiscard]] static constexpr array_type& extract(T(*a)[N]) noexcept {
				return *a;
			}
		};
#endif

		// MSVC 19.31.31104 claims tc::is_stashing_element<Element>::value is not of type bool when directly used in a requires clause on element_stash_impl<Element>.
		// Outline to template variable to avoid silent bad codegen due to wrong sfinae.
		template<typename Element>
		inline bool constexpr is_stashing_element_v = tc::is_stashing_element<Element>::value;

		// Lifetime of reference is bound to the iterator -> must keep iterator alive
		template<typename Element> requires is_stashing_element_v<Element>
		struct element_stash_impl<Element> final : tc::storage_for_dtor_at_end_of_scope<Element> {
			static_assert(tc::decayed<Element>);
			[[nodiscard]] constexpr decltype(auto) extract(Element&& elem) & noexcept(noexcept(*tc_move_always(this->operator*()))) { // note that constexpr only works for trivially constructible/assignable/destructible type
				this->ctor(tc_move(elem));
				return *tc_move_always(this->operator*());
			}
		};
	}
	template<typename Rng>
	using element_stash = no_adl::element_stash_impl<tc::iterator_t<Rng>>;

#pragma push_macro("RETURN_REFERENCE_FROM_ELEMENT")
	// Make first template argument non-type to avoid name(rng) instantiating name<tc::return_element, Rng> and then name<tc::return_element, tc::return_element> which leads to hard error.
#define RETURN_REFERENCE_FROM_ELEMENT(name) \
	template <int = 0, typename Rng> \
	[[nodiscard]] constexpr decltype(auto) name( \
		Rng&& rng, \
		tc::element_stash<Rng>&& stash={} \
	) return_MAYTHROW( \
		stash.extract(tc::name<tc::return_element>(std::forward<Rng>(rng))) \
	) \
	\
	/* Use tc::fn_front() instead of tc_fn(tc::front) because the latter returns a dangling xvalue reference when used with counting ranges. */ \
	/* Support tc::fn_front<tc::return_xxx>() for consistency*/ \
	namespace fn_ ## name ## _adl { \
		template<typename RangeReturn=void> \
		struct fn_ ## name { \
			template<typename Rng> \
			decltype(auto) operator()(Rng&& rng) const& return_MAYTHROW( \
				tc::name<RangeReturn>(std::forward<Rng>(rng)) \
			) \
			friend tc::constant<true> returns_reference_to_argument(fn_ ## name); /*mark as returning reference to argument. */ \
		}; \
		template<> \
		struct fn_ ## name<void> { \
			template<typename Rng> \
			decltype(auto) operator()(Rng&& rng, tc::element_stash<Rng>&& stash={}) const& return_MAYTHROW( \
				tc::name(std::forward<Rng>(rng), tc_move(stash)) \
			) \
			friend tc::constant<true> returns_reference_to_argument(fn_ ## name); /*mark as returning reference to argument. */ \
		}; \
	} \
	using fn_ ## name ## _adl::fn_ ## name;

	RETURN_REFERENCE_FROM_ELEMENT(front)
	RETURN_REFERENCE_FROM_ELEMENT(back)
	RETURN_REFERENCE_FROM_ELEMENT(linear_back)
	RETURN_REFERENCE_FROM_ELEMENT(only)
#pragma pop_macro("RETURN_REFERENCE_FROM_ELEMENT")

#pragma push_macro("RETURN_REFERENCE_FROM_INDEXED_ELEMENT")
#define RETURN_REFERENCE_FROM_INDEXED_ELEMENT(fn) \
	template <int = 0, typename Rng> \
	[[nodiscard]] constexpr decltype(auto) fn( \
		Rng&& rng, \
		typename boost::range_size< std::remove_reference_t<Rng> >::type i, \
		tc::element_stash<Rng>&& stash={} \
	) return_MAYTHROW( \
		stash.extract(tc::fn<tc::return_element>(std::forward<Rng>(rng), tc_move(i))) \
	)

	RETURN_REFERENCE_FROM_INDEXED_ELEMENT(at)
	RETURN_REFERENCE_FROM_INDEXED_ELEMENT(linear_at)
	RETURN_REFERENCE_FROM_INDEXED_ELEMENT(reverse_at)
#pragma pop_macro("RETURN_REFERENCE_FROM_INDEXED_ELEMENT")

// Write as macros to keep temporary iterators alive.
// By standard, the lifetime of a reference is limited to the lifetime of the iterator.
#define tc_front_nodebug(rng) (*tc::begin(rng))
#define tc_at_nodebug(rng, i) *(tc::begin(rng) + static_cast<decltype(tc::begin(rng) - tc::begin(rng))>(i))

	template<typename Rng, typename Func>
	bool if_nonempty_with_only(Rng&& rng, Func func) MAYTHROW {
		bool bCalled=false;
		tc::for_each(std::forward<Rng>(rng), [&](auto&&... args) MAYTHROW {
			VERIFY(tc::change(bCalled, true)); // only a single element is allowed

			RETURNS_VOID( func(tc_move_if_owned(args)...) ); // MAYTHROW

			return tc::constant<tc::continue_>();
		});
		return bCalled;
	}

	template<typename Rng, typename Func>
	bool if_nonempty_with_front(Rng&& rng, Func func) MAYTHROW {
		return tc::break_==tc::for_each(std::forward<Rng>(rng), [&](auto&&... args) MAYTHROW{
			RETURNS_VOID(func(tc_move_if_owned(args)...)); // MAYTHROW
			return tc::constant<tc::break_>();
		});
	}

	template<typename RangeReturn, typename T, typename Rng>
	[[nodiscard]] std::pair<tc::element_return_type_t<RangeReturn, Rng>, T> front_and_size_bounded(Rng&& rng, T nMax) MAYTHROW {
		static_assert(!RangeReturn::requires_iterator, "implement iterator version if needed");
		tc::storage_for<tc::element_return_type_t<RangeReturn, Rng>> ot;
		T n=0;
		tc::for_each(std::forward<Rng>(rng), [&](auto&& t) MAYTHROW {
			if (0==n) {
				ot.ctor(RangeReturn::template pack_element<Rng>(tc_move_if_owned(t)));
			}
			++n;
			return (nMax<=n) ? tc::break_ : tc::continue_;
		});
		if (0==n) {
			tc_return_cast(RangeReturn::template pack_no_element<Rng>(), tc_move(n));
		} else {
			tc_scope_exit { ot.dtor(); };
			tc_return_cast(*tc_move(ot), tc_move(n));
		}
	}
}
