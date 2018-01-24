#pragma once

#include "range_fwd.h"
#include "assign.h"
#include "size.h"
#include "conditional.h"

namespace tc {
	namespace best_adl_barrier {	
		template<typename Better>
		struct best_impl final {

			template<typename BetterRef>
			constexpr best_impl(BetterRef&& better) : m_better(std::forward<BetterRef>(better))
			{}

		private:
			static_assert( tc::is_decayed<Better>::value );
			Better m_better;

		public:
			template<typename T>
			constexpr auto operator()(T&& t) const& noexcept -> T&& {
				return std::forward<T>(t);
			}
			
			template<typename T0, typename T1, typename... Args>
			constexpr auto operator()(T0&& t0, T1&& t1, Args&&... args) const& noexcept -> decltype(auto) {
				return CONDITIONAL(
					m_better(t0, t1),
					operator()(std::forward<T0>(t0), std::forward<Args>(args)...),
					operator()(std::forward<T1>(t1), std::forward<Args>(args)...)
				);
			}

		};
	}

	template<
		typename Better,
		typename... Args
	>
	constexpr auto best(Better&& better, Args&&... args) return_decltype_rvalue_by_ref(
		best_adl_barrier::best_impl<tc::decay_t<Better>>(std::forward<Better>(better))(std::forward<Args>(args)...)
	)

	template<typename... Args>
	constexpr auto min(Args&&... args) return_decltype_rvalue_by_ref(
		best(tc::fn_less{}, std::forward<Args>(args)...)
	)

	template<typename... Args>
	constexpr auto max(Args&&... args) return_decltype_rvalue_by_ref(
		best(tc::fn_greater{}, std::forward<Args>(args)...)
	)

	DEFINE_FN(min);
	std::true_type returns_reference_to_argument(tc::fn_min) noexcept;
}

