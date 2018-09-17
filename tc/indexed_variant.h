
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "casts.h"

#if _HAS_CXX17

#include "type_traits.h"
#include "return_decltype.h"

#include <variant>

namespace tc {
	template<typename... Ts>
	using indexed_variant = std::variant<Ts...>;

	template<std::size_t I, typename IndexedVariant, std::enable_if_t<tc::is_instance<std::variant, std::remove_reference_t<IndexedVariant>>::value>* = nullptr>
	auto get(IndexedVariant&& indexedvariant) MAYTHROW return_decltype(
		std::get<I>(std::forward<IndexedVariant>(indexedvariant))
	)

	template<std::size_t I>
	constexpr auto in_place_index = std::in_place_index<I>;
}

#else

#include "utility.h"
#include "meta.h"

#include <boost/variant.hpp>
#include <boost/fusion/include/pair.hpp>

namespace tc {
	namespace indexed_variant_impl {
		template<typename... Ts, std::size_t... Is>
		static boost::variant<tc::tagged_type<INTEGRAL_CONSTANT(Is), Ts>...> indexed_variant_base(std::index_sequence<Is...>);

		template<typename... Ts>
		using indexed_variant_base_t = decltype(indexed_variant_base<Ts...>(std::index_sequence_for<Ts...>()));
	};

	template <std::size_t I> struct in_place_index_t {
		explicit in_place_index_t() = default;
	};
	template <std::size_t I>
	constexpr in_place_index_t<I> in_place_index{};

	template<typename... Ts>
	struct indexed_variant : indexed_variant_impl::indexed_variant_base_t<Ts...> {
	private:
		using base_ = indexed_variant_impl::indexed_variant_base_t<Ts...>;

		template<std::size_t I>
		using type_at = typename type_by_index<I, Ts...>::type;

		template<std::size_t I>
		using variant_member_type_at = tc::tagged_type<INTEGRAL_CONSTANT(I), type_at<I>>;

	public:
		indexed_variant() = default;

		template<std::size_t I, typename... Args>
		explicit indexed_variant(in_place_index_t<I>, Args&&... args) noexcept
			: base_(variant_member_type_at<I>(type_at<I>(std::forward<Args>(args)...))) {
		}

		template<std::size_t I>
		type_at<I>& get() & noexcept {
			return boost::get<variant_member_type_at<I>>(*this);
		}

		template<std::size_t I>
		type_at<I> const& get() const & noexcept {
			return boost::get<variant_member_type_at<I>>(*this);
		}

		std::size_t index() const& noexcept {
			return tc::unsigned_cast(this->which());
		}
	};

	template<std::size_t I, typename IndexedVariant, std::enable_if_t<tc::is_instance<indexed_variant, std::remove_reference_t<IndexedVariant>>::value>* = nullptr>
	auto get(IndexedVariant&& indexedvariant) MAYTHROW return_decltype(
		std::forward<IndexedVariant>(indexedvariant).template get<I>()
	)
}

#endif
