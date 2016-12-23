#pragma once

#include "range_fwd.h"
#include "assign.h"
#include "size.h"

namespace tc {
	template<typename Lhs, typename Rhs>
	auto conditional(tc::bool_context b, Lhs&& lhs, Rhs&& rhs) noexcept->tc::common_reference_t<Lhs&&, Rhs&&> {
		using result_type = tc::common_reference_t<Lhs&&, Rhs&&>;
		if (b) {
			return static_cast<result_type>(std::forward<Lhs>(lhs)); // static_cast needed for conversion from const& to const&&
		} else {
			return static_cast<result_type>(std::forward<Rhs>(rhs)); // static_cast needed for conversion from const& to const&&
		}
	}

	namespace best_adl_barrier {
		template<typename T, std::enable_if_t<tc::is_instance<tc::size_proxy,T>::value && std::is_signed<T>::value>* =nullptr>
		void AssertSizeProxy(T const& t) {
			_ASSERT(-1!=t);
		}
		template<typename T, std::enable_if_t<!(tc::is_instance<tc::size_proxy,T>::value && std::is_signed<T>::value)>* =nullptr>
		void AssertSizeProxy(T const&) {}
		
		template<typename Better>
		struct best_impl final {

			template<typename BetterRef>
			constexpr best_impl(BetterRef&& better) : m_better(std::forward<BetterRef>(better))
			{}

		private:
			static_assert( tc::is_decayed<Better>::value, "" );
			Better m_better;

		public:
			template<typename T0, typename T1>
			struct common_min_type_decayed final : std::conditional< // in case of equality, tc::min returns the left value
					std::is_signed<T0>::value && std::is_signed<T1>::value || std::numeric_limits<T0>::max() == std::numeric_limits<T1>::max(),
					tc::common_type_t<T0,T1>,
					std::conditional_t<
						std::numeric_limits<T1>::max() < std::numeric_limits<T0>::max(),
						T1,
						T0
					>
				>
			{};

			template<typename T0, typename T1>
			using common_min_type_t = typename common_min_type_decayed<tc::decay_t<T0>,tc::decay_t<T1>>::type;

			template<typename T0, typename T1>
			struct common_min_type_decayed<tc::size_proxy<T0>, tc::size_proxy<T1>> final {
				using type = tc::size_proxy<common_min_type_t<std::make_unsigned_t<T0>, std::make_unsigned_t<T1>>>;
			};

			template<typename T0, typename T1>
			struct common_min_type_decayed<tc::size_proxy<T0>, T1> {
				using type = T1;
			};

			template<typename T0, typename T1>
			struct common_min_type_decayed<T0, tc::size_proxy<T1>> final : common_min_type_decayed<tc::size_proxy<T1>, T0> {
			};

			template<typename T>
			struct is_actual_integer_or_size_proxy final
			:	std::integral_constant<bool, tc::is_actual_integer<T>::value || tc::is_instance<tc::size_proxy,T>::value > {
			};

			template<typename T>
			constexpr auto operator()(T&& t) const& noexcept -> T&& {
				return std::forward<T>(t);
			}
			
			template<
				typename T0,
				typename T1,
				std::enable_if_t<
					std::is_same<Better, tc::fn_less>::value &&
					is_actual_integer_or_size_proxy<
						tc::common_reference_t<T0&&, T1&&>
					>::value
				>* = nullptr
			>
			auto operator()(T0&& t0, T1&& t1) const& noexcept
				-> common_min_type_t<T0, T1>
			{
				using result_type = common_min_type_t<T0,T1>;
				AssertSizeProxy(t0);
				AssertSizeProxy(t1);
				if( m_better(t1,t0) ) {
					return static_cast<result_type>(t1);
				} else {
					return static_cast<result_type>(t0);
				}
			}

			template<
				typename T0,
				typename T1,
				std::enable_if_t<!(
					std::is_same<Better, tc::fn_less>::value &&
					is_actual_integer_or_size_proxy<
						tc::common_reference_t<T0&&, T1&&>
					>::value)
				>* = nullptr
			>
			auto operator()(T0&& t0, T1&& t1) const& noexcept ->decltype(auto) {
				return tc::conditional(m_better(t1,t0), std::forward<T1>(t1), std::forward<T0>(t0) );
			}

			template<
				typename First,
				typename Second,
				typename Third,
				typename... Args
			>
			constexpr auto operator()(First&& first, Second&& second, Third&& third, Args&&... args) const& noexcept return_decltype_rvalue_by_ref(
				operator()(operator()(std::forward<First>(first), std::forward<Second>(second)), std::forward<Third>(third), std::forward<Args>(args)...)
			)
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

