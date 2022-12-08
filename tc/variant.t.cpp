// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt


#include "base/assert_defs.h"
#include "unittest.h"
#include "base/enum.h"
#include "variant.h"

namespace {
	DEFINE_ENUM(EState, estate, (COPIEDONCE)(ONLYMOVED)(COPIEDFROM)(MOVEDFROM))
	struct copy_move_tracker {
		mutable EState m_estate;
		copy_move_tracker() : m_estate(estateONLYMOVED) {}
		copy_move_tracker(copy_move_tracker const& tracker) noexcept
			: m_estate(estateCOPIEDONCE)
		{
			VERIFYPRED(tracker.m_estate, estateONLYMOVED == _) = estateCOPIEDFROM;
		}
		copy_move_tracker(copy_move_tracker&& tracker) noexcept
			: m_estate(VERIFYPRED(tracker.m_estate, estateONLYMOVED == _ || estateCOPIEDONCE == _))
		{
			VERIFYPRED(tracker.m_estate, estateONLYMOVED == _ || estateCOPIEDONCE == _) = estateMOVEDFROM;
		}
		copy_move_tracker& operator=(copy_move_tracker const& rhs) = delete;
	};

	struct convert1to2_tag {};
	struct copy_move_tracker1 : copy_move_tracker {};
	struct copy_move_tracker2 : copy_move_tracker {
		using copy_move_tracker::copy_move_tracker;
		copy_move_tracker2(copy_move_tracker2 const&) noexcept = delete;
		copy_move_tracker2(copy_move_tracker2&&) noexcept = default;
		copy_move_tracker2(convert1to2_tag, copy_move_tracker const& tracker) noexcept : copy_move_tracker(tracker) {}
		copy_move_tracker2(convert1to2_tag, copy_move_tracker&& tracker) noexcept : copy_move_tracker(tc_move(tracker)) {}
	};

	copy_move_tracker2 explicit_convert_impl(tc::explicit_convert_adl::adl_tag_t, tc::type::identity<copy_move_tracker2>, copy_move_tracker1 const& tracker) noexcept {
		return {convert1to2_tag{}, tracker};
	}
	copy_move_tracker2 explicit_convert_impl(tc::explicit_convert_adl::adl_tag_t, tc::type::identity<copy_move_tracker2>, copy_move_tracker1&& tracker) noexcept {
		return {convert1to2_tag{}, tc_move(tracker)};
	}

	UNITTESTDEF(VariantMovedByExplicitCast) { // TODO: naming

		// Check that explicit_cast<variant> moves the variant's internal data when possible (depending on the category of variant being passed to the function).
		// Variants cannot hold references so we don't need to test different reference types held within the source and target variants - only the type of the variant itself.

		{
			std::variant<copy_move_tracker1, int> vars;
			auto vart = tc::explicit_cast<std::variant<copy_move_tracker2, double>>(vars);
			static_assert(std::is_same<decltype(vart), std::variant<copy_move_tracker2, double>>::value);
			_ASSERTEQUAL(tc::get<0>(vars).m_estate, estateCOPIEDFROM);
			_ASSERTEQUAL(tc::get<0>(vart).m_estate, estateCOPIEDONCE);
		}

		{
			std::variant<copy_move_tracker1, int> vars;
			auto vart = tc::explicit_cast<std::variant<copy_move_tracker2, double>>(static_cast<decltype(vars) const&>(vars));
			static_assert(std::is_same<decltype(vart), std::variant<copy_move_tracker2, double>>::value);
			_ASSERTEQUAL(tc::get<0>(vars).m_estate, estateCOPIEDFROM);
			_ASSERTEQUAL(tc::get<0>(vart).m_estate, estateCOPIEDONCE);
		}

		{
			std::variant<copy_move_tracker1, int> vars;
			auto vart = tc::explicit_cast<std::variant<copy_move_tracker2, double>>(static_cast<decltype(vars)&&>(vars));
			static_assert(std::is_same<decltype(vart), std::variant<copy_move_tracker2, double>>::value);
			_ASSERTEQUAL(tc::get<0>(vars).m_estate, estateMOVEDFROM);
			_ASSERTEQUAL(tc::get<0>(vart).m_estate, estateONLYMOVED);
		}

		{
			std::variant<copy_move_tracker1, int> vars;
			auto vart = tc::explicit_cast<std::variant<copy_move_tracker2, double>>(static_cast<decltype(vars) const&&>(vars));
			static_assert(std::is_same<decltype(vart), std::variant<copy_move_tracker2, double>>::value);
			_ASSERTEQUAL(tc::get<0>(vars).m_estate, estateCOPIEDFROM);
			_ASSERTEQUAL(tc::get<0>(vart).m_estate, estateCOPIEDONCE);
		}
	}
}
