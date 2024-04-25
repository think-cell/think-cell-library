
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/string_template_param.h"
#include "../range/literal_range.h"
#include "../array.h"
#include "../restricted_enum.h"

namespace tc {
	using char_ascii = tc::restricted_enum<char, '\0', '\x7f'>;
	using char_asciidigit = tc::restricted_enum<char, '0', '9'>;
	using char_asciilower = tc::restricted_enum<char, 'a', 'z'>;
	using char_asciiupper = tc::restricted_enum<char, 'A', 'Z'>;

	namespace no_adl {
#pragma push_macro("CUSTOMIZE_COMMON_TYPE_CHAR_ASCII")
#define CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(char_type) \
		template<> \
		struct common_type_decayed_impl<tc::char_ascii, char_type> { \
			using type = char_type; \
		}; \
		template<> \
		struct common_type_decayed_impl<char_type, tc::char_ascii> { \
			using type = char_type; \
		};

		CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(char)
		CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(wchar_t)
		CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(char16_t)
		CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(char32_t)
#pragma pop_macro("CUSTOMIZE_COMMON_TYPE_CHAR_ASCII")
	}
}

template<tc::char_type Char, Char chFirst, Char chLast>
struct std::char_traits<tc::restricted_enum<Char, chFirst, chLast>> final {
	using char_type = tc::restricted_enum<Char, chFirst, chLast>;
	using int_type = typename tc::char_traits<Char>::int_type;
	using off_type = typename tc::char_traits<Char>::off_type;
	using pos_type = typename tc::char_traits<Char>::pos_type;
	using state_type = typename tc::char_traits<Char>::state_type;

	static constexpr void assign(char_type& r, const char_type& a) noexcept {
		r = a;
	}

	static char_type* assign(char_type* p, std::size_t count, char_type a) {
		std::fill(p, p+count, a);
		return p;
	}

	static constexpr bool eq(char_type a, char_type b) noexcept {
		return a == b;
	}

	static constexpr bool lt(char_type a, char_type b) noexcept {
		return a < b;
	}

	static constexpr std::size_t length(const char_type* s) noexcept {
		std::size_t n = 0;
		if(s) {
			while(char_type() != *s++)
				++n;
		}
		return n;
	}

	static constexpr const char_type* find(const char_type* p, std::size_t count, const char_type& ch) noexcept {
		while(0<count) {
			if(*p == ch) {
				return p;
			} else {
				++p;
				--count;
			}
		}
		return nullptr;
	}

	static constexpr int compare(const char_type* s1, const char_type* s2, std::size_t count) noexcept {
		for(std::size_t i=0; i<count; ++i) {
			if (*s1<*s2) return -1;
			else if (*s2<*s1) return 1;
		}
		return 0;
	}

	static char_type* move(char_type* dest, const char_type* src, std::size_t count) noexcept {
		if (src <= dest && dest < src+count) {
			std::copy_backward(src, src+count, dest+count);
		} else {
			std::copy(src, src+count, dest);
		}
		return dest;
	}

	static char_type* copy(char_type* dest, const char_type* src, std::size_t count) noexcept {
		return move(dest, src, count); // copy works correctly just like move in clang and gcc's implementation
	}
};

namespace tc {
#if WCHAR_MAX == 0xFFFF // == 2 bytes unsigned: We want tc::char16 to be unsigned. wchar_t is unsigned on MSVC, but that's implementation-specific.
	using char16 = wchar_t;
	#define tc_raw_utf16_(x) L ## x
#else
	using char16 = char16_t;
	#define tc_raw_utf16_(x) u ## x
#endif
#define tc_raw_utf16(x) tc_raw_utf16_(x)
#define tc_utf16(x) tc_raw_utf16(x)
}

#define tc_ascii_val(str) tc::make_array<tc::char_ascii>(str)  // avoid copy
#define tc_ascii(str) tc_as_constexpr(tc_ascii_val(str))

namespace tc {
	template <tc::string_template_param String>
	constexpr bool is_ascii_string_literal = []{
		// Simple for loop to avoid any cyclic dependencies.
		for (auto c : String) {
			if (static_cast<unsigned>(tc::to_underlying(c)) > 0x7F) return false;
		}
		return true;
	}();

	template <tc::string_template_param String>
	constexpr auto string_literal = []<std::size_t ... I>(std::index_sequence<I...>){
		using char_type = tc::range_value_t<decltype(String)>;
		static_assert(!std::same_as<char_type, wchar_t>, "use either u or U prefix to force UTF-16 or UTF-32; not L");

		if constexpr (std::same_as<char_type, char>) {
			return tc::literal_range<tc::char_ascii, String[I]...>{};
		} else if constexpr (std::same_as<char_type, char8_t>) {
			return tc::literal_range<char, String[I]...>{};
		} else if constexpr (std::same_as<char_type, char16_t>) {
			return tc::literal_range<tc::char16, String[I]...>{};
		} else {
			return tc::literal_range<char_type, String[I]...>{};
		}
	}(std::make_index_sequence<tc::constexpr_size<decltype(String)>()>());
}

// "abc"_tc -> ASCII-8 string with char type tc::char_ascii
// u8"abc"_tc -> UTF-8 string with char type char
// u"abc"_tc -> UTF-16 string with char type tc::char16
// U"abc"_tc -> UTF-32 string with char type char32_t
template <tc::string_template_param String>
[[nodiscard]] constexpr auto operator""_tc() noexcept {
	return tc::string_literal<String>;
}

// 8-bit character literal is always ASCII.
[[nodiscard]] constexpr auto operator""_tc(char const c) noexcept {
	return tc::char_ascii(c);
}
[[nodiscard]] constexpr auto operator""_tc(char8_t const c) noexcept {
	return char(c);
}
[[nodiscard]] constexpr auto operator""_tc(char16_t const c) noexcept {
	return tc::char16(c);
}
[[nodiscard]] constexpr auto operator""_tc(char32_t const c) noexcept {
	return c;
}
// This is not portable.
void operator""_tc(wchar_t) = delete;
