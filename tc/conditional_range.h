#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "index_range.h"
#include "meta.h"
#include "size.h"
#include "type_list.h"
#include "variant.h"
#include "interval.h"

namespace tc {
	namespace no_adl {
		// MSVC 15.8 complains if declared inside select_range_adaptor
		template<typename... Rng>
		using select_range_adaptor_size_result_t = tc::common_type_t<decltype(tc::size_raw(std::declval<Rng const&>()))...>;

		template<typename... Rng>
		struct select_range_adaptor {
		private:
			std::variant<decltype(tc::make_reference_or_value(std::declval<Rng>()))...> m_ubaserng;

		public:
			template<typename... FuncRng>
			explicit select_range_adaptor(aggregate_tag_t, int n, FuncRng&&... funcrng) noexcept
				: m_ubaserng([&]() noexcept {
					STATICASSERTEQUAL( sizeof...(Rng), sizeof...(FuncRng) );
					_ASSERT( tc::make_interval(0, tc::explicit_cast<int>(sizeof...(Rng))).contains(n) );
					return tc::invoke_with_constant<std::index_sequence_for<FuncRng...>>(
						[&](auto nconstIndex) noexcept {
							return decltype(m_ubaserng)(
								std::in_place_index<nconstIndex()>, tc::aggregate_tag, std::get<nconstIndex()>(std::forward_as_tuple(std::forward<FuncRng>(funcrng)...))()
							);
						},
						n
					);
				}())
			{}

		private:
			template<typename Sink>
			using enumerate_result_t = tc::common_type_t<decltype(tc::for_each(*tc::as_const(tc::as_lvalue(tc::make_reference_or_value(std::declval<Rng>()))), std::declval<Sink>()))..., INTEGRAL_CONSTANT(tc::continue_)>;

		public:
			template<typename Sink>
			auto operator()(Sink&& sink) const& MAYTHROW -> enumerate_result_t<Sink> {
				return tc::visit<enumerate_result_t<Sink>>(
					m_ubaserng,
					[&](auto const& baserng) noexcept {
						return tc::for_each(*baserng, std::forward<Sink>(sink));
					}
				);
			}

		private:
			template<typename Sink>
			using enumerate_reversed_result_t = tc::common_type_t<decltype(tc::for_each(tc::reverse(*tc::as_const(tc::as_lvalue(tc::make_reference_or_value(std::declval<Rng>())))), std::declval<Sink>()))..., INTEGRAL_CONSTANT(tc::continue_)>;

		public:
			template<typename Sink>
			static auto enumerate_reversed(select_range_adaptor<Rng...> const& rngThis, Sink&& sink) MAYTHROW -> enumerate_reversed_result_t<Sink> {
				return tc::visit<enumerate_reversed_result_t<Sink>>(
					rngThis.m_ubaserng,
					[&](auto const& baserng) noexcept {
						return tc::for_each(tc::reverse(*baserng), std::forward<Sink>(sink));
					}
				);
			}

			template<ENABLE_SFINAE>
			auto size() const& noexcept -> select_range_adaptor_size_result_t<SFINAE_TYPE(Rng)...> {
				return tc::visit<select_range_adaptor_size_result_t<Rng...>>(
					m_ubaserng,
					[](auto const& baserng) noexcept {
						return tc::size_raw(*baserng);
					}
				);
			}

			bool empty() const& noexcept {
				return tc::visit(
					m_ubaserng,
					[](auto const& baserng) noexcept {
						return tc::empty(*baserng);
					}
				);
			}
		};
		
		template<typename SelectRangeAdaptor, typename... Rng>
		// Using tc::void_t<...> instead of decltype(std::declval<...>(), void()) does not compile with MSVC 15.8
		struct range_value<SelectRangeAdaptor, select_range_adaptor<Rng...>, decltype(std::declval<tc::common_range_value_t<decltype(std::declval<Rng>())...>>(), void())> final {
			using type = tc::common_range_value_t<decltype(std::declval<Rng>())...>;
		};
	}

	using no_adl::select_range_adaptor;

	template<typename... FuncRng>
	auto select_range_impl(std::true_type, int n, FuncRng&&... funcrng) noexcept -> tc::common_reference_xvalue_as_ref_t<decltype(std::declval<FuncRng>()())...>
	{
#if TC_WIN
		// The following assert must not hold: A function pointer to a function that returns a fixed size array by reference must also return a fixed size array by reference, not by value!
		// If MSVC fixed that bug, please unify select_range.
		static_assert(
			std::is_same<
				decltype(std::declval<
					int (&())[3]
				>()()),
				int [3]
			>::value
		);

		tc::storage_for<tc::common_reference_xvalue_as_ref_t<decltype(std::declval<FuncRng>()())...>> result;
		scope_exit(result.dtor());

		_ASSERT(0<=n && n<sizeof...(FuncRng));
		tc::invoke_with_constant<std::index_sequence_for<FuncRng...>>(
			[&](auto nconstIndex) noexcept {
				result.ctor(std::get<nconstIndex()>(std::forward_as_tuple(std::forward<FuncRng>(funcrng)...))());
			},
			n
		);
		return *tc_move(result);
#else
		static_assert(
			std::is_same<
				decltype(std::declval<
					int (&())[3]
				>()()),
				int (&)[3]
			>::value
		);

		return tc::invoke_with_constant<std::index_sequence_for<FuncRng...>>(
			[&](auto nconstIndex) noexcept -> tc::common_reference_xvalue_as_ref_t<decltype(std::declval<FuncRng>()())...> {
				return std::get<nconstIndex()>(std::forward_as_tuple(std::forward<FuncRng>(funcrng)...))();
			},
			n
		);
#endif
	}

	template<typename... FuncRng>
	auto select_range_impl(std::false_type, int n, FuncRng&&... funcrng) return_ctor_noexcept(
		select_range_adaptor<decltype(std::declval<FuncRng>()())...>,
		(aggregate_tag, n, std::forward<FuncRng>(funcrng)...)
	)

	template<typename... FuncRng>
	auto select_range(int n, FuncRng&&... funcrng) noexcept {
		return select_range_impl(tc::has_common_reference_xvalue_as_ref<tc::type::list<decltype(std::forward<FuncRng>(funcrng)())...>>(), n, std::forward<FuncRng>(funcrng)...);
	}

	template<typename FuncRngTrue, typename FuncRngFalse>
	auto conditional_range(tc::bool_context b, FuncRngTrue&& funcrngTrue, FuncRngFalse&& funcrngFalse) {
		return select_range(b ? 0 : 1, std::forward<FuncRngTrue>(funcrngTrue), std::forward<FuncRngFalse>(funcrngFalse));
	}

	template<typename FuncRngTrue>
	auto conditional_range(tc::bool_context b, FuncRngTrue&& funcrngTrue) noexcept {
		return conditional_range(b, std::forward<FuncRngTrue>(funcrngTrue), MAKE_LAZY(tc::empty_range()));
	}
}

#include <boost/vmd/assert.hpp>

// BOOST_PP_VARIADIC_SIZE returns always at least 1, so there is no point of checking against 0. However, empty __VA_ARGS__ will trigger a compilation error in MAKE_LAZY
// The macro might not compile, when the ranges contain top-level commas, even when expanded from another macro. Using parens helps, e.g., 
//   tc_conditional_range(b, MAKE_CONSTEXR_ARRAY(0))
// fails to compile on MSVC 15.8. However,
//   tc_conditional_range(b, (MAKE_CONSTEXR_ARRAY(0)))
// compiles.
#define tc_conditional_range(b, ...) \
	BOOST_VMD_ASSERT(BOOST_PP_LESS_EQUAL(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 2)) \
	tc::conditional_range(b, \
		MAKE_LAZY(BOOST_PP_VARIADIC_ELEM(0, __VA_ARGS__)) \
		BOOST_PP_COMMA_IF(BOOST_PP_EQUAL(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 2)) \
		BOOST_PP_EXPR_IF(BOOST_PP_EQUAL(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 2), MAKE_LAZY(BOOST_PP_VARIADIC_ELEM(1, __VA_ARGS__))) \
	)

namespace tc {
	template<typename Rng, typename Fn>
	auto transform_range_if(tc::bool_context b, Rng&& rng, Fn fn) noexcept {
		return conditional_range(b,
			/*funcrngTrue*/[&]() noexcept -> decltype(auto) { return fn(std::forward<Rng>(rng)); },
			/*funcrngFalse*/[&]() noexcept -> decltype(auto) { return std::forward<Rng>(rng); }
		);
	}
}
