#include "../Range.h"
#include "range.t.h"

namespace {
	void static_tests() {
		auto rngSize = tc::transform(std::vector<int>(), [](int) { return 0; });
		static_assert(tc::size_impl::has_size<decltype(rngSize)>::value, "");

		auto rngNoSize = tc::transform(tc::filter(std::vector<int>(), [](int){ return false; }), [](int) { return 0; });
		static_assert(!tc::size_impl::has_size<decltype(rngNoSize)>::value, "");
	}
}
