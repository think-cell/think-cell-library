//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
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

#if _HAS_CXX17

#include <variant>

namespace tc {
	template<typename... Ts>
	using indexed_variant = std::variant<Ts...>;

	template<std::size_t I, typename IndexedVariant, std::enable_if_t<tc::is_instance<std::variant, std::remove_reference_t<IndexedVariant>>::value>* = nullptr>
	auto get(IndexedVariant&& indexedvariant) MAYTHROW return_decltype(
		std::get<I>(std::forward<IndexedVariant>(indexedvariant))
	)

	template<std::size_t I>
	constexpr auto const in_place_index = std::in_place_index<I>;
}

#else

#include "utility.h"
#include "meta.h"

#include <boost/variant.hpp>
#include <boost/fusion/include/pair.hpp>

namespace tc {
	namespace indexed_variant_impl {
		template<typename... Ts, std::size_t... Is>
		static boost::variant<tc::tagged_type<std::integral_constant<std::size_t, Is>, Ts>...> indexed_variant_base(std::index_sequence<Is...>);

		template<typename... Ts>
		using indexed_variant_base_t = decltype(indexed_variant_base<Ts...>(std::index_sequence_for<Ts...>()));
	};

	template <std::size_t I> struct in_place_index_t {
		explicit in_place_index_t() = default;
	};
	template <std::size_t I>
	constexpr in_place_index_t<I> const in_place_index{};

	template<typename... Ts>
	struct indexed_variant : indexed_variant_impl::indexed_variant_base_t<Ts...> {
	private:
		using base_ = indexed_variant_impl::indexed_variant_base_t<Ts...>;

		template<std::size_t I>
		using type_at = typename type_by_index<I, Ts...>::type;

		template<std::size_t I>
		using variant_member_type_at = tc::tagged_type<std::integral_constant<std::size_t, I>, type_at<I>>;

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
