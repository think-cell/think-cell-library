
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "subrange.h"

namespace tc {
	namespace return_detail {
		template<typename Rng>
		constexpr decltype(auto) empty_slice(Rng&& rng) noexcept {
			auto it = tc::begin(rng);
			auto it2 = it;
			return tc::slice(std::forward<Rng>(rng), tc_move(it), tc_move(it2));
		}

		namespace no_adl {
			// Pack border as subrange

			struct return_take_base {
				template<typename It, typename Rng>
				static constexpr auto pack_border(It&& it, Rng&& rng) return_decltype_xvalue_by_ref_noexcept(
					tc::take(std::forward<Rng>(rng), std::forward<It>(it))
				)
			};

			struct return_drop_base {
				template<typename It, typename Rng>
				static constexpr auto pack_border(It&& it, Rng&& rng) return_decltype_xvalue_by_ref_noexcept(
					tc::drop(tc_move_if_owned(rng), tc_move_if_owned(it))
				)
			};

			template<typename ReturnSubrange>
			struct return_subrange_or_assert final : ReturnSubrange {
				static constexpr bool allowed_if_always_has_border = true;

				template<typename... Args>
				static decltype(auto) pack_no_border(Args&&... args) noexcept {
					_ASSERTFALSE;
					return ReturnSubrange::pack_no_border(std::forward<Args>(args)...);
				}
			};

			template<typename ReturnSubrange>
			struct return_subrange_or_none final {
				static constexpr bool allowed_if_always_has_border = false;

				template<typename It, typename Rng>
				static constexpr auto pack_border(It&& it, Rng&& rng) noexcept {
					return std::optional(ReturnSubrange::pack_border(std::forward<It>(it), std::forward<Rng>(rng)));
				}

				template<typename Rng, typename... OptEndIt>
				static constexpr decltype(std::optional(ReturnSubrange::pack_border(tc::begin(std::declval<Rng&>()), std::declval<Rng>()))) pack_no_border(Rng&&, OptEndIt&&...) noexcept {
					return std::nullopt;
				}
			};

			template<typename ReturnSubrange>
			struct return_subrange_or_all final : ReturnSubrange {
				static constexpr bool allowed_if_always_has_border = false;

				template<typename Rng, typename... OptEndIt>
				static constexpr decltype(ReturnSubrange::pack_border(tc::begin(std::declval<Rng&>()), std::declval<Rng>())) pack_no_border(Rng&& rng, OptEndIt&&...) noexcept {
					return std::forward<Rng>(rng);
				}
			};

			// Pack element as border before/after

			template<typename ReturnBorder, bool bSupportsNoElement>
			struct pack_as_border_base {
				static_assert( !ReturnBorder::allowed_if_always_has_border, "Specify what to return in the no_element case. Use bSupportsNoElement=false to _ASSERT it does not occur." );
				static constexpr bool requires_iterator = true;

				template<typename Rng>
				static constexpr auto pack_no_element(Rng&& rng) noexcept code_return_decltype_xvalue_by_ref(
					if constexpr( !bSupportsNoElement ) _ASSERTFALSE;,
					ReturnBorder::pack_no_border(std::forward<Rng>(rng))
				)
			};

			template<typename ReturnBorder, bool bSupportsNoElement = true>
			struct pack_as_border_before : pack_as_border_base<ReturnBorder, bSupportsNoElement> {
				static constexpr bool allowed_if_always_has_border = true;
				template<typename It, typename Rng>
				static constexpr auto pack_element(It&& it, Rng&& rng, tc::unused /*ref*/={}) return_decltype_xvalue_by_ref_noexcept(
					ReturnBorder::pack_border(std::forward<It>(it), std::forward<Rng>(rng))
				)
				template<typename It, typename Rng>
				static constexpr decltype(auto) pack_border(It&& it, Rng&& rng) noexcept(noexcept(--it)) {
					if( tc::begin(rng) != it ) {
						--it;
						return ReturnBorder::pack_border(std::forward<It>(it), std::forward<Rng>(rng));
					} else {
						return ReturnBorder::pack_no_border(std::forward<Rng>(rng));
					}
				}
				template<typename Rng, typename Begin, typename End>
				static constexpr auto pack_view(Rng&& rng, Begin&& begin, End&& end) return_decltype_xvalue_by_ref_noexcept(
					ReturnBorder::pack_border(std::forward<Begin>(begin), std::forward<Rng>(rng))
				)
			};

			template<typename ReturnBorder, bool bSupportsNoElement = true>
			struct pack_as_border_after : pack_as_border_base<ReturnBorder, bSupportsNoElement> {
				static constexpr bool allowed_if_always_has_border = true;
				template<typename It, typename Rng>
				static constexpr auto pack_element(It&& it, Rng&& rng, tc::unused /*ref*/={}) return_decltype_xvalue_by_ref_MAYTHROW(
					++it, // MAYTHROW
					ReturnBorder::pack_border(std::forward<It>(it), std::forward<Rng>(rng))
				)
				template<typename It, typename Rng>
				static constexpr decltype(auto) pack_border(It&& it, Rng&& rng) noexcept(noexcept(--it)) {
					if( tc::end(rng) != it ) {
						++it;
						return ReturnBorder::pack_border(std::forward<It>(it), std::forward<Rng>(rng));
					} else {
						return ReturnBorder::pack_no_border(std::forward<Rng>(rng));
					}
				}
				template<typename Rng, typename Begin, typename End>
				static constexpr auto pack_view(Rng&& rng, Begin&&, End&& end) return_decltype_xvalue_by_ref_noexcept(
					ReturnBorder::pack_border(std::forward<End>(end), std::forward<Rng>(rng))
				)
			};

			// Pack border as element before/after

			template<typename ReturnElement>
			struct pack_as_element_base {
				static constexpr bool allowed_if_always_has_border = true; // element before/after may not exist, even if we have a border.
				static_assert( ReturnElement::requires_iterator );

				template<typename Rng, typename... OptEndIt>
				static constexpr auto pack_no_border(Rng&& rng, OptEndIt&&...) return_decltype_noexcept(
					ReturnElement::pack_no_element(std::forward<Rng>(rng))
				)
			};

			template<typename ReturnElement>
			struct pack_as_element_before final : pack_as_element_base<ReturnElement> {
				template<typename It, typename Rng>
				static constexpr decltype(auto) pack_border(It&& it, Rng&& rng) noexcept(noexcept(--it)) {
					if( tc::begin(rng) != it ) {
						--it;
						return ReturnElement::pack_element(std::forward<It>(it), std::forward<Rng>(rng));
					} else {
						return ReturnElement::pack_no_element(std::forward<Rng>(rng));
					}
				}
			};

			template<typename ReturnElement>
			struct pack_as_element_after final : pack_as_element_base<ReturnElement> {
				template<typename It, typename Rng>
				static constexpr decltype(auto) pack_border(It&& it, Rng&& rng) noexcept {
					if( tc::end(rng) != it ) {
						return ReturnElement::pack_element(std::forward<It>(it), std::forward<Rng>(rng));
					} else {
						return ReturnElement::pack_no_element(std::forward<Rng>(rng));
					}
				}
			};
		}
	}

	namespace no_adl {
		/////////////////////////////////////
		// return void

		struct return_void final {
			static constexpr bool requires_iterator = false;
			static constexpr bool allowed_if_always_has_border = true;

			static constexpr void pack_border(tc::unused /*it*/, tc::unused /*rng*/) noexcept {}
			static constexpr void pack_no_border(tc::unused /*rng*/) noexcept {}
			static constexpr void pack_no_border(tc::unused /*rng*/, tc::unused /*itEnd*/) noexcept {}
			static constexpr void pack_element(tc::unused /*it*/, tc::unused /*rng*/) noexcept {}
			static constexpr void pack_element(tc::unused /*it*/, tc::unused /*rng*/, tc::unused /*ref*/) noexcept {}
			template<typename Rng>
			static constexpr void pack_element(tc::unused /*ref*/) noexcept {}
			static constexpr void pack_no_element(tc::unused) noexcept {}
			template<typename Rng>
			static constexpr void pack_no_element() noexcept {}
			static constexpr void pack_view(tc::unused /*rng*/, tc::unused /*itBegin*/, tc::unused /*itEnd*/) noexcept {}
		};

		/////////////////////////////////////
		// return bool

		struct return_bool {
			static constexpr bool requires_iterator = false;
			static constexpr bool allowed_if_always_has_border = false;

			static constexpr bool pack_border(tc::unused /*it*/, tc::unused /*rng*/) noexcept {
				return true;
			}
			static constexpr bool pack_no_border(tc::unused /*rng*/) noexcept {
				return false;
			}
			static constexpr bool pack_no_border(tc::unused /*rng*/, tc::unused /*itEnd*/) noexcept {
				return false;
			}

			static constexpr bool pack_element(tc::unused /*it*/, tc::unused /*rng*/) noexcept {
				return true;
			}
			static constexpr bool pack_element(tc::unused /*it*/, tc::unused /*rng*/, tc::unused /*ref*/) noexcept {
				return true;
			}
			template<typename Rng>
			static constexpr bool pack_element(tc::unused /*ref*/) noexcept {
				return true;
			}
			static constexpr bool pack_no_element(tc::unused) noexcept {
				return false;
			}
			template<typename Rng>
			static constexpr bool pack_no_element() noexcept {
				return false;
			}

			static constexpr bool pack_view(tc::unused /*rng*/, tc::unused /*itBegin*/, tc::unused /*itEnd*/) noexcept {
				return true;
			}
		};

		/////////////////////////////////////
		// controlling bound return

		// returning bound

		struct return_border {
			static constexpr bool allowed_if_always_has_border = true;

			template<typename It>
			static constexpr tc::decay_t<It> pack_border(It&& it, tc::unused /*rng*/) noexcept {
				return std::forward<It>(it);
			}

			template<typename Rng, typename... OptEndIt>
			static auto pack_no_border(Rng&& rng, OptEndIt&&...) noexcept {
				_ASSERTFALSE;
				return tc::begin(rng);
			}
		};

		struct return_border_or_begin final : return_border {
			static constexpr bool allowed_if_always_has_border = false;

			template<typename Rng, typename... OptEndIt>
			static constexpr auto pack_no_border(Rng&& rng, OptEndIt&&...) noexcept {
				return tc::begin(rng);
			}
		};

		struct return_border_or_end final : return_border {
			static constexpr bool allowed_if_always_has_border = false;

			template<typename Rng>
			static constexpr auto pack_no_border(Rng&& rng) noexcept {
				return tc::end(rng);
			}

			template<typename Rng>
			static constexpr auto pack_no_border(Rng&& rng, tc::iterator_t<Rng>&& itEnd) noexcept {
				return tc_move(itEnd);
			}
		};

		struct return_border_or_null final {
			static constexpr bool allowed_if_always_has_border = false;

			template<typename It, typename Rng>
			static constexpr auto pack_border(It&& it, Rng&&) noexcept {
				return tc::border_t<tc::decay_t<It>>(std::forward<It>(it));
			}

			template<typename Rng, typename... OptEndIt>
			static constexpr auto pack_no_border(Rng&&, OptEndIt&&...) noexcept {
				return tc::border_t<tc::decay_t<decltype(tc::begin(std::declval<Rng&>()))>>{};
			}
		};

		struct return_border_index final {
			static constexpr bool allowed_if_always_has_border = true;

			template<typename It, typename Rng>
			static constexpr auto pack_border(It&& it, Rng&& rng) noexcept {
				return tc::make_size_proxy(it - tc::begin(rng));
			}

			template<typename Rng, typename... OptEndIt>
			static auto pack_no_border(Rng&&, OptEndIt&&...) noexcept {
				_ASSERTFALSE;
				return decltype(tc::make_size_proxy(tc::begin(std::declval<Rng&>()) - tc::begin(std::declval<Rng&>())))(0);
			}
		};

		// returning range

		struct return_take_or_empty : return_detail::no_adl::return_take_base {
			static constexpr bool allowed_if_always_has_border = false;

			template<typename Rng, typename... OptEndIt>
			static constexpr auto pack_no_border(Rng&& rng, OptEndIt&&...) return_decltype_xvalue_by_ref_MAYTHROW(
				tc::take(std::forward<Rng>(rng), tc::begin(rng))
			)
		};

		struct return_drop_or_empty : return_detail::no_adl::return_drop_base {
			static constexpr bool allowed_if_always_has_border = false;

			template<typename Rng>
			static constexpr auto pack_no_border(Rng&& rng) return_decltype_xvalue_by_ref_MAYTHROW(
				tc::drop(tc_move_if_owned(rng), tc::end(rng))
			)

			template<typename Rng>
			static constexpr auto pack_no_border(Rng&& rng, tc::iterator_t<Rng>&& itEnd) return_decltype_xvalue_by_ref_MAYTHROW(
				tc::drop(tc_move_if_owned(rng), tc_move(itEnd))
			)
		};

		/////////////////////////////////////
		// controlling element return

		// returning element

		struct return_element {
			static constexpr bool requires_iterator = true;

			template<typename It>
			static constexpr tc::decay_t<It> pack_element(It&& it, tc::unused /*rng*/, tc::unused /*ref*/={}) noexcept {
				return std::forward<It>(it);
			}

			template<typename Rng>
			static auto pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return tc::begin(rng);
			}
		};

		struct return_element_or_null final {
			static constexpr bool requires_iterator = true;

			template<typename It>
			static constexpr auto pack_element(It&& it, tc::unused /*rng*/, tc::unused /*ref*/={}) noexcept {
				return tc::make_element(std::forward<It>(it));
			}

			template<typename Rng>
			static constexpr auto pack_no_element(Rng&&) noexcept {
				return tc::element_t<tc::decay_t<decltype(tc::begin(std::declval<Rng&>()))>>{}; // value initialization to initialize pointers to nullptr
			}
		};

		struct return_element_or_front final : return_element {
			template<typename Rng>
			static constexpr auto pack_no_element(Rng&& rng) noexcept {
				return tc::begin(rng);
			}
		};

		struct return_element_or_back final : return_element {
			template<typename Rng>
			static constexpr auto pack_no_element(Rng&& rng) noexcept {
				return tc::end_prev<tc::return_border>(std::forward<Rng>(rng));
			}
		};

		struct return_value {
			static constexpr bool requires_iterator = false;

			// Don't take it by value, or ref may be invalidated
			template<typename Rng, typename Ref>
			static constexpr tc::range_value_t<Rng> pack_element(tc::unused /*it*/, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename It, typename Rng>
			static constexpr tc::range_value_t<Rng> pack_element(It&& it, Rng&&) noexcept {
				return *std::forward<It>(it);
			}
			template<typename Rng, typename Ref>
			static constexpr tc::range_value_t<Rng> pack_element(Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Rng>
			static auto pack_no_element(Rng&&) noexcept {
				_ASSERTFALSE;
				return tc::construct_default_or_terminate<tc::range_value_t<Rng>>();
			}
			template<typename Rng>
			static auto pack_no_element() noexcept {
				_ASSERTFALSE;
				return tc::construct_default_or_terminate<tc::range_value_t<Rng>>();
			}
		};

		struct return_value_or_default final : return_value {
			template<typename Rng>
			static constexpr tc::range_value_t<Rng> pack_no_element(Rng&&) noexcept {
				return {};
			}
			template<typename Rng>
			static constexpr tc::range_value_t<Rng> pack_no_element() noexcept {
				return {};
			}
		};

		struct return_value_or_none final {
			static constexpr bool requires_iterator = false;

			// Don't take it by value, or ref may be invalidated
			template<typename Rng, typename Ref>
			static constexpr std::optional<tc::range_value_t<Rng>> pack_element(tc::unused /*it*/, Rng&&, Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename It, typename Rng>
			static constexpr std::optional<tc::range_value_t<Rng>> pack_element(It&& it, Rng&&) noexcept {
				return *std::forward<It>(it);
			}
			template<typename Rng, typename Ref>
			static constexpr std::optional<tc::range_value_t<Rng>> pack_element(Ref&& ref) noexcept {
				return std::forward<Ref>(ref);
			}
			template<typename Rng>
			static constexpr std::optional<tc::range_value_t<Rng>> pack_no_element(Rng&&) noexcept {
				return std::nullopt;
			}
			template<typename Rng>
			static constexpr std::optional<tc::range_value_t<Rng>> pack_no_element() noexcept {
				return std::nullopt;
			}
		};

		struct return_element_index {
			static constexpr bool requires_iterator = true;

			template<typename It, typename Rng>
			static constexpr auto pack_element(It&& it, Rng&& rng, tc::unused /*ref*/={}) noexcept {
				return tc::make_size_proxy(it - tc::begin(rng));
			}

			template<typename Rng>
			static auto pack_no_element(Rng&&) noexcept {
				_ASSERTFALSE;
				return decltype(tc::make_size_proxy(tc::begin(std::declval<Rng&>()) - tc::begin(std::declval<Rng&>())))(0);
			}
		};

		struct return_element_index_or_none final {
			static constexpr bool requires_iterator = true;

			template<typename It, typename Rng>
			static constexpr auto pack_element(It&& it, Rng&& rng, tc::unused /*ref*/={}) noexcept {
				return std::optional(tc::make_size_proxy(it - tc::begin(rng)));
			}

			template<typename Rng>
			static constexpr decltype(std::optional(tc::make_size_proxy(tc::begin(std::declval<Rng&>()) - tc::begin(std::declval<Rng&>())))) pack_no_element(Rng&&) noexcept {
				return std::nullopt;
			}
		};

		struct return_element_index_or_npos final : return_element_index {
			// Note: prefer return_element_index_or_none
			// static_cast<int>(npos) must be -1. Anything else is error-prone. So use range_difference instead of range_size.
			// Alternatively, we could return a special type that casts npos to -1.
			static constexpr bool requires_iterator = true;

			template<typename Rng>
			static constexpr auto pack_no_element(Rng&&) noexcept {
				return decltype(tc::make_size_proxy(tc::begin(std::declval<Rng&>()) - tc::begin(std::declval<Rng&>())))(-1);
			}
		};

		struct return_element_index_or_size final : return_element_index {
			template<typename Rng>
			static constexpr auto pack_no_element(Rng&& rng) noexcept {
				return decltype(tc::make_size_proxy(tc::begin(std::declval<Rng&>()) - tc::begin(std::declval<Rng&>())))(tc::size(rng));
			}
		};

		struct return_singleton_range {
			static constexpr bool requires_iterator = true;

			template<typename It, typename Rng>
			static constexpr decltype(auto) pack_element(It&& it, Rng&& rng, tc::unused /*ref*/={}) noexcept(noexcept(++tc::as_lvalue(tc::decay_copy(it)))) {
				return tc::slice(std::forward<Rng>(rng), it, tc_modified(it, ++_));
			}
			template<typename Rng>
			static decltype(auto) pack_no_element(Rng&& rng) noexcept {
				// safe choice is empty because result may be empty
				_ASSERTFALSE;
				return tc::return_detail::empty_slice(std::forward<Rng>(rng));
			}
		};

		struct return_singleton_range_or_empty final : return_singleton_range {
			template<typename Rng>
			static constexpr auto pack_no_element(Rng&& rng) return_decltype_noexcept(
				tc::return_detail::empty_slice(std::forward<Rng>(rng))
			)
		};

		/////////////////////////////////////
		// controlling view return

		struct return_view {
			template<typename Rng, typename Begin, typename End>
			static constexpr auto pack_view(Rng&& rng, Begin&& begin, End&& end) return_decltype_noexcept(
				tc::slice(std::forward<Rng>(rng), std::forward<Begin>(begin), std::forward<End>(end))
			)

			template<typename Rng>
			static decltype(auto) pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return tc::slice(std::forward<Rng>(rng), tc::begin(rng), tc::end(rng));
			}
		};

		struct return_view_or_none final {
			template<typename Rng, typename Begin, typename End>
			static constexpr auto pack_view(Rng&& rng, Begin&& begin, End&& end) noexcept {
				return std::optional(return_view::pack_view(std::forward<Rng>(rng), std::forward<Begin>(begin), std::forward<End>(end)));
			}

			template<typename Rng>
			static decltype(std::optional(return_view::pack_no_element(std::declval<Rng>()))) pack_no_element(Rng&&) noexcept {
				return std::nullopt;
			}
		};

		struct return_view_or_empty final : return_view {
			template<typename Rng>
			static constexpr auto pack_no_element(Rng&& rng) return_decltype_noexcept(
				tc::return_detail::empty_slice(std::forward<Rng>(rng))
			)
		};

		struct return_begin_index final {
			template<typename Rng, typename Begin, typename End>
			static constexpr decltype(auto) pack_view(Rng&& rng, Begin&& begin, End&& end) noexcept {
				return tc::make_size_proxy(begin - tc::begin(rng));
			}

			template<typename Rng>
			static decltype(auto) pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return decltype(tc::make_size_proxy(tc::begin(std::declval<Rng&>()) - tc::begin(std::declval<Rng&>())))(0);
			}
		};

		struct return_end_index final {
			template<typename Rng, typename Begin, typename End>
			static constexpr decltype(auto) pack_view(Rng&& rng, Begin&& begin, End&& end) noexcept {
				return tc::make_size_proxy(end - tc::begin(rng));
			}

			template<typename Rng>
			static decltype(auto) pack_no_element(Rng&& rng) noexcept {
				_ASSERTFALSE;
				return decltype(tc::make_size_proxy(tc::begin(std::declval<Rng&>()) - tc::begin(std::declval<Rng&>())))(0);
			}
		};
	} // namespace no_adl
	using no_adl::return_void;
	using no_adl::return_bool;

	// return border
	using no_adl::return_border;
	using no_adl::return_border_or_begin;
	using no_adl::return_border_or_end;
	using no_adl::return_border_or_null;
	using no_adl::return_border_index;

	using no_adl::return_take_or_empty;
	using return_take = return_detail::no_adl::return_subrange_or_assert<tc::return_take_or_empty>;
	using return_take_or_none = return_detail::no_adl::return_subrange_or_none<return_detail::no_adl::return_take_base>;
	using return_take_or_all = return_detail::no_adl::return_subrange_or_all<return_detail::no_adl::return_take_base>;

	using no_adl::return_drop_or_empty;
	using return_drop = return_detail::no_adl::return_subrange_or_assert<tc::return_drop_or_empty>;
	using return_drop_or_none = return_detail::no_adl::return_subrange_or_none<return_detail::no_adl::return_drop_base>;
	using return_drop_or_all = return_detail::no_adl::return_subrange_or_all<return_detail::no_adl::return_drop_base>;

	// return element
	using no_adl::return_element;
	using no_adl::return_element_or_null;
	using no_adl::return_element_or_front;
	using no_adl::return_element_or_back;
	using no_adl::return_value;
	using no_adl::return_value_or_default;
	using no_adl::return_value_or_none;
	using no_adl::return_element_index;
	using no_adl::return_element_index_or_none;
	using no_adl::return_element_index_or_npos;
	using no_adl::return_element_index_or_size;
	using no_adl::return_singleton_range;
	using no_adl::return_singleton_range_or_empty;

	template< tc::actual_integer T >
	[[nodiscard]] constexpr bool is_npos(T t) noexcept {
		if constexpr( std::is_signed<T>::value ) {
			return -1 == t;
		} else {
			return std::numeric_limits<T>::max() == t;
		}
	}

	// return border element
	using return_element_before = return_detail::no_adl::pack_as_element_before<tc::return_element>;
	using return_element_before_or_null = return_detail::no_adl::pack_as_element_before<tc::return_element_or_null>;
	using return_element_before_or_front = return_detail::no_adl::pack_as_element_before<tc::return_element_or_front>;
	using return_element_after = return_detail::no_adl::pack_as_element_after<tc::return_element>;
	using return_element_after_or_null = return_detail::no_adl::pack_as_element_after<tc::return_element_or_null>;
	using return_element_after_or_back = return_detail::no_adl::pack_as_element_after<tc::return_element_or_back>;

	// return element border
	using return_border_after = return_detail::no_adl::pack_as_border_after<tc::return_border_or_end, false>;
	using return_border_after_or_begin = return_detail::no_adl::pack_as_border_after<tc::return_border_or_begin>;
	using return_border_before = return_detail::no_adl::pack_as_border_before<tc::return_border_or_begin, false>;
	using return_border_before_or_end = return_detail::no_adl::pack_as_border_before<tc::return_border_or_end>;

	// return element border or border element
	using return_border_before_or_begin = return_detail::no_adl::pack_as_border_before<tc::return_border_or_begin>;
	using return_border_after_or_end = return_detail::no_adl::pack_as_border_after<tc::return_border_or_end>;

	using return_take_before = return_detail::no_adl::pack_as_border_before<tc::return_take_or_empty, false>; // safe choice is empty because result may be empty
	using return_take_before_or_empty = return_detail::no_adl::pack_as_border_before<tc::return_take_or_empty>;
	using return_take_before_or_all = return_detail::no_adl::pack_as_border_before<tc::return_take_or_all>;
	using return_take_before_or_none = return_detail::no_adl::pack_as_border_before<tc::return_take_or_none>;
	using return_take_after = return_detail::no_adl::pack_as_border_after<tc::return_take_or_all, false>; // safe choice is all because result is never empty
	using return_take_after_or_empty = return_detail::no_adl::pack_as_border_after<tc::return_take_or_empty>;
	using return_take_after_or_all = return_detail::no_adl::pack_as_border_after<tc::return_take_or_all>;
	using return_take_after_or_none = return_detail::no_adl::pack_as_border_after<tc::return_take_or_none>;

	using return_drop_before = return_detail::no_adl::pack_as_border_before<tc::return_drop_or_all, false>; // safe choice is all because result is never empty
	using return_drop_before_or_empty = return_detail::no_adl::pack_as_border_before<tc::return_drop_or_empty>;
	using return_drop_before_or_all = return_detail::no_adl::pack_as_border_before<tc::return_drop_or_all>;
	using return_drop_before_or_none = return_detail::no_adl::pack_as_border_before<tc::return_drop_or_none>;
	using return_drop_after = return_detail::no_adl::pack_as_border_after<tc::return_drop_or_empty, false>; // safe choice is empty because result may be empty
	using return_drop_after_or_empty = return_detail::no_adl::pack_as_border_after<tc::return_drop_or_empty>;
	using return_drop_after_or_all = return_detail::no_adl::pack_as_border_after<tc::return_drop_or_all>;
	using return_drop_after_or_none = return_detail::no_adl::pack_as_border_after<tc::return_drop_or_none>;

	// return view
	using no_adl::return_view;
	using no_adl::return_view_or_none;
	using no_adl::return_view_or_empty;
	using no_adl::return_begin_index;
	using no_adl::return_end_index;
}
