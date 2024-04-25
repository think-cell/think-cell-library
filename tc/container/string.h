
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "../base/assert_defs.h"
#include "../base/return_decltype.h"
#include <string>
#ifndef TC_PRIVATE
#include <string_view>
#endif

namespace tc {
	namespace no_adl {
		template<typename Char>
		struct char_traits_selector;

		template<typename Char> requires tc::char_like<Char> && (!(std::is_same<Char, char>::value && std::is_signed<char>::value))
		struct char_traits_selector<Char> final {
			using type = std::char_traits<Char>;
		};

		template<typename Char> requires std::is_same<Char, char>::value && std::is_signed<char>::value
		struct char_traits_selector<Char> final {
			struct type final: std::char_traits<char> {
				static constexpr bool lt(char_type lhs, char_type rhs) noexcept {
					return lhs < rhs;
				}

				static constexpr int compare(char_type const* lhs, char_type const* rhs, std::size_t count) noexcept {
					for (; 0<count; --count, ++lhs, ++rhs) {
						if (lt(*lhs, *rhs))
							return -1;
						if (lt(*rhs, *lhs))
							return 1;
					}
					return 0;
				}
			};
		};
	}

	// tc::char_traits<char>: compares chars as chars
	// std::char_traits<char>: compares chars as if they are unsigned chars
	template<typename Char>
	using char_traits = typename tc::no_adl::char_traits_selector<Char>::type;

	// We want consistent result when lexicographically comparing char ranges. When it comes to element comparison,
	//   1. All the other char ranges except std::basic_string<char> simply compare elements with less operator,
	//   2. std::basic_string<char, std::char_traits<char>> compares elements via std::char_traits<char>::lt() which
	//      casts elements to unsigned chars before comparing. This might change the signedness of a char value on
	//      x86 platforms and eventually lead to inconsistent comparing result.
	// Therefore we introduce tc::string with tc::char_traits as the CharTraits to make comparisons between char ranges consistent.
	template<typename Char, typename Alloc=std::allocator<Char>>
	using string = std::basic_string<Char, tc::char_traits<Char>, Alloc>;

}

#ifndef TC_PRIVATE // We don't want std::hash.
template <typename Alloc> requires std::is_signed<char>::value
struct std::hash<std::basic_string<char, tc::char_traits<char>, Alloc>> {
	auto operator()(std::basic_string<char, tc::char_traits<char>, Alloc> const& str) const& return_decltype_MAYTHROW(std::hash<std::string_view>{}(std::string_view(str.data(), str.size())))
};
#endif
