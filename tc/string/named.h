
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/string_template_param.h"
#include "char.h"

namespace tc {
	namespace no_adl {
		template <tc::string_template_param strName, typename T>
			requires tc::is_ascii_string_literal<strName>
		struct named final {
			T m_t;

			static auto constexpr c_strName = tc::string_literal<strName>;
		};
	}

	IS_INSTANCE_TRAIT(_str, ((tc::string_template_param)(str))((typename)(...)(T)), using arguments=boost::mp11::mp_list<T...>; static constexpr auto first_argument=str;)

	template<typename T>
	using is_named = tc::constant<tc::instance_str<std::remove_reference_t<T>, no_adl::named>>;

	static_assert(is_named< no_adl::named<"test",int> >::value);

	template<tc::string_template_param strName, typename T>
	constexpr auto named(T&& t) return_ctor_MAYTHROW(
		TC_FWD(no_adl::named<strName, std::remove_cv_t<T>>), {tc_move_if_owned(t)}
	)
}
