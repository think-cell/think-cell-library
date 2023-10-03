#pragma once

#include "assert_defs.h"
#include <exception>

namespace tc {
	namespace no_adl {
		struct inside_unwinding {
			inside_unwinding() noexcept
			: m_nUncaughtExceptions(std::uncaught_exceptions())
			{}
			inside_unwinding(inside_unwinding const&) noexcept
			: inside_unwinding()
			{}
			inside_unwinding& operator=(inside_unwinding const&) & noexcept {
				return *this;
			}
			bool inside_stack_unwinding() const& noexcept {
				int const nUncaughtExceptions = std::uncaught_exceptions();
				_ASSERT(m_nUncaughtExceptions<=nUncaughtExceptions);
				return m_nUncaughtExceptions<nUncaughtExceptions;
			}
		private:
			int const m_nUncaughtExceptions;
		};
	}
	using no_adl::inside_unwinding;
}
