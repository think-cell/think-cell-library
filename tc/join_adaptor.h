
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "range_adaptor.h"
#include "reference_or_value.h"
#include "index_range.h"
#include "for_each.h"
#include "accumulate.h"
#include "const_forward.h"
#include "modified.h"

#include <functional>

namespace tc {
	void join();

	namespace no_adl {
		namespace join_adaptor_detail {
			template <typename RngRng, typename = void>
			struct is_joinable_with_iterators : std::false_type {};

			template <typename RngRng>
			struct is_joinable_with_iterators<
				RngRng,
				std::enable_if_t<tc::is_range_with_iterators<RngRng>::value>
			> : std::bool_constant<
				tc::is_range_with_iterators<tc::range_value_t<RngRng>>::value &&
				std::is_lvalue_reference<tc::range_reference_t<RngRng>>::value
			> {};
		}

		template<typename RngRng, bool bHasIterator = join_adaptor_detail::is_joinable_with_iterators<RngRng>::value>
		struct join_adaptor;

		template<typename Sink, bool bReverse>
		struct join_sink;

		template<typename Sink>
		struct join_sink<Sink, /*bReverse*/false> {
			static_assert( tc::is_decayed<Sink>::value );
			Sink m_sink;

			template<typename Rng>
			auto operator()(Rng&& rng) const& return_decltype_MAYTHROW(
				tc::for_each(std::forward<Rng>(rng), m_sink)
			)

			template<typename SubRngRng, ENABLE_SFINAE>
			auto chunk(SubRngRng&& rngrng) const& return_decltype_MAYTHROW(
				SFINAE_VALUE(m_sink).chunk(tc::join(std::forward<SubRngRng>(rngrng)))
			)
		};

		template<typename Sink>
		struct join_sink<Sink, /*bReverse*/true> {
			static_assert( tc::is_decayed<Sink>::value );
			Sink m_sink;

			template<typename Rng>
			auto operator()(Rng&& rng) const& return_decltype_MAYTHROW(
				tc::for_each(tc::reverse(std::forward<Rng>(rng)), m_sink)
			)
		};

		template<typename RngRng>
		struct [[nodiscard]] join_adaptor<RngRng, false> : tc::generator_range_adaptor<RngRng> {
			constexpr join_adaptor() = default;
			using tc::generator_range_adaptor<RngRng>::generator_range_adaptor;

			template<typename Sink, bool bReverse>
			static constexpr auto adapted_sink(Sink&& sink, std::bool_constant<bReverse>) noexcept {
				return join_sink<tc::decay_t<Sink>, bReverse>{std::forward<Sink>(sink)};
			}

			template<ENABLE_SFINAE>
			auto size() const& return_decltype_noexcept(
				// Use tc::mul and boost::multiprecision::number?
				tc::size_raw(SFINAE_VALUE(this)->base_range()) * tc::constexpr_size<tc::range_value_t<SFINAE_TYPE(RngRng)>>::value
			)
		};

		template<typename RngRng>
		struct [[nodiscard]] join_adaptor<RngRng, true> :
			join_adaptor<RngRng, false>,
			tc::range_iterator_from_index<
				join_adaptor<RngRng>,
				tc::tuple<
					tc::index_t<std::remove_reference_t<RngRng>>,
					tc::index_t<std::remove_reference_t<tc::range_reference_t<RngRng>>>
				>
			>
		{
		private:
			using this_type = join_adaptor;
		public:
			using typename this_type::range_iterator_from_index::index;

			static_assert(!tc::has_stashing_index<std::remove_reference_t<RngRng>>::value,
				"RngRgn must not have \"stashing\" index/iterator: copying the composite index would invalidate the inner index/iterator."
			);
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<tc::range_reference_t<RngRng>>>::value;
		private:
			tc::index_t<std::remove_reference_t<tc::range_reference_t<RngRng>>> find_valid_index(tc::index_t<std::remove_reference_t<RngRng>>& idxFirst) const& noexcept {
				while (!tc::at_end_index(this->base_range(), idxFirst)) {
					auto& rngSecond = tc::dereference_index(this->base_range_best_access(), idxFirst);
					auto idxSecond = tc::begin_index(rngSecond);
					if (tc::at_end_index(rngSecond, idxSecond)) {
						tc::increment_index(this->base_range(), idxFirst);
					} else {
						return idxSecond;
					}
				}
				return {};
			}
		public:
			constexpr join_adaptor() = default;
			using join_adaptor<RngRng, false>::join_adaptor;

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				auto idxFirst = this->base_begin_index();
				auto idxSecond = find_valid_index(idxFirst);
				return { tc_move(idxFirst), tc_move(idxSecond) };
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return { this->base_end_index(), {} };
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::at_end_index(this->base_range(), tc::get<0>(idx));
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				_ASSERT(!tc::at_end_index(this->base_range(), tc::get<0>(idx)));
				auto_cref(rngSecond, tc::dereference_index(this->base_range(), tc::get<0>(idx)));
				tc::increment_index(rngSecond, tc::get<1>(idx));
				if (tc::at_end_index(rngSecond, tc::get<1>(idx))) {
					tc::increment_index(this->base_range(), tc::get<0>(idx));
					tc::get<1>(idx) = find_valid_index(tc::get<0>(idx));
				}
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				auto const funcReverseFindValidIndex = [&]() noexcept {
					for (;;) {
						tc::decrement_index(this->base_range(), tc::get<0>(idx));
						auto& rngSecond = tc::dereference_index(this->base_range_best_access(), tc::get<0>(idx));
						tc::get<1>(idx) = tc::end_index(rngSecond);
						if (tc::begin_index(rngSecond) != tc::get<1>(idx)) {
							tc::decrement_index(rngSecond, tc::get<1>(idx));
							break;
						}
					}
				};
				if(tc::at_end_index(this->base_range(), tc::get<0>(idx))) {
					funcReverseFindValidIndex();
				} else {
					auto_cref(rngSecond, tc::dereference_index(this->base_range(), tc::get<0>(idx)));
					if(tc::begin_index(rngSecond) == tc::get<1>(idx)) {
						funcReverseFindValidIndex();
					} else {
						tc::decrement_index(rngSecond, tc::get<1>(idx));
					}
				}
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept -> decltype(auto) {
				return tc::dereference_index(tc::dereference_index(this->base_range(), tc::get<0>(idx)), tc::get<1>(idx));
			}

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept -> decltype(auto) {
				return tc::dereference_index(tc::dereference_index(this->base_range(), tc::get<0>(idx)), tc::get<1>(idx));
			}

			auto element_base_index(index const& idx) const& noexcept {
				return tc::get<0>(idx);
			}
		};

		template<typename RngRng>
		struct constexpr_size_base<join_adaptor<RngRng>, std::void_t<typename tc::constexpr_size<RngRng>::type, typename tc::constexpr_size<tc::range_value_t<RngRng>>::type>>
			: std::integral_constant<std::size_t, tc::constexpr_size<RngRng>::value * tc::constexpr_size<tc::range_value_t<RngRng>>::value>
		{};

		template<typename JoinAdaptor, typename RngRng>
		struct range_value<
			JoinAdaptor,
			join_adaptor<RngRng>,
			std::enable_if_t<
				!join_adaptor_detail::is_joinable_with_iterators<RngRng>::value,
				std::void_t<tc::range_value_t<tc::range_value_t<RngRng>>>
			>
		> final {
			using type = tc::range_value_t<tc::range_value_t<RngRng>>;
		};
	}
	using no_adl::join_adaptor;

	namespace join_default {
		template<typename RngRng>
		constexpr join_adaptor<RngRng> join_impl(RngRng&& rngrng) noexcept {
			return {aggregate_tag, std::forward<RngRng>(rngrng)};
		}
	}

	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(join)

	namespace cartesian_product_adaptor_detail {
		// MSVC 15.8 complains if declared inside cartesian_product_adaptor
		// Use variadic tc::mul and boost::multiprecision::number?
		template<typename... Rng>
		using size_result_t = std::enable_if_t<std::conjunction<typename std::is_convertible<decltype(tc::size_raw(std::declval<Rng const&>())), std::size_t>::type...>::value, std::size_t>;

		template<typename T>
		static T&& select_element(T&& val, std::true_type) noexcept {
			return std::forward<T>(val);
		}

		template<typename T>
		static T const&& select_element(T&& val, std::false_type) noexcept {
			return tc::const_forward<T>(val);
		}
	}

	namespace cartesian_product_adaptor_adl {
		template<typename... Rng>
		struct [[nodiscard]] cartesian_product_adaptor {
		private:
			tc::tuple<reference_or_value<Rng>...> m_baserng;

		public:
			template<typename... Rhs>
			constexpr cartesian_product_adaptor(aggregate_tag_t, Rhs&&... rhs) noexcept
				: m_baserng{{ {{aggregate_tag, std::forward<Rhs>(rhs)}}... }}
			{}

		private:
			template<typename Self, typename Sink, typename... Ts>
			struct cartesian_product_sink {
				using guaranteed_break_or_continue = std::conditional_t<
					std::is_same<INTEGRAL_CONSTANT(tc::continue_), tc::guaranteed_break_or_continue_t<Sink>>::value,
					INTEGRAL_CONSTANT(tc::continue_),
					tc::break_or_continue
				>;

				Self& m_self;
				Sink const& m_sink;
				tc::tuple<Ts...>& m_ts;

				template<typename T>
				constexpr auto operator()(T&& val) const& return_decltype_MAYTHROW(
					std::remove_reference_t<Self>::internal_for_each_impl( // recursive MAYTHROW
						std::forward<Self>(m_self),
						m_sink,
						tc::tuple_cat(
							/*cast to const rvalue*/std::move(tc::as_const(m_ts)),
							tc::forward_as_tuple(
								cartesian_product_adaptor_detail::select_element(
									std::forward<T>(val),
									std::bool_constant<sizeof...(Rng) == sizeof...(Ts) + 1>()
								)
							)
						)
					)
				)
			};

			template<typename Self, typename Sink, typename... Ts, std::enable_if_t<sizeof...(Ts) == sizeof...(Rng)>* = nullptr>
			static constexpr auto internal_for_each_impl(Self const&, Sink const& sink, tc::tuple<Ts...> ts) return_decltype_MAYTHROW(
				tc::continue_if_not_break(sink, tc_move(ts)) // MAYTHROW
			)

			template<typename Self, typename Sink, typename... Ts, std::enable_if_t<sizeof...(Ts) < sizeof...(Rng)>* = nullptr>
			static constexpr auto internal_for_each_impl(Self&& self, Sink const& sink, tc::tuple<Ts...> ts) return_decltype_MAYTHROW(
				tc::for_each(*tc::get<sizeof...(Ts)>(std::forward<Self>(self).m_baserng), cartesian_product_sink<Self, Sink, Ts...>{self, sink, ts}) // recursive MAYTHROW
			)

		public:
			template<typename Self, typename Sink, std::enable_if_t<tc::is_base_of_decayed<cartesian_product_adaptor, Self>::value>* = nullptr>
			friend constexpr auto for_each_impl(Self&& self, Sink const sink) return_decltype_MAYTHROW(
				std::remove_reference_t<Self>::internal_for_each_impl(std::forward<Self>(self), sink, tc::make_tuple()) // MAYTHROW
			)

			template<ENABLE_SFINAE>
			constexpr auto size() const& noexcept -> cartesian_product_adaptor_detail::size_result_t<SFINAE_TYPE(Rng)...> {
				using size_type = cartesian_product_adaptor_detail::size_result_t<Rng...>;
				return tc::accumulate(
					m_baserng,
					tc::explicit_cast<size_type>(1),
					[](size_type& nAccu, auto const& baserng) noexcept {
						size_type const nBase = tc::size_raw(*baserng);
						_ASSERT( nAccu <= std::numeric_limits<std::size_t>::max() / nBase );
						nAccu *= nBase;
					}
				);
			}
		};
	}
	using cartesian_product_adaptor_adl::cartesian_product_adaptor;

	namespace no_adl {
		template<typename... Rng>
		struct constexpr_size_base<cartesian_product_adaptor<Rng...>, tc::void_t<typename tc::constexpr_size<Rng>::type...>>
			: std::integral_constant<std::size_t, (... * tc::constexpr_size<Rng>::value)>
		{};

		template<typename CartesianProductAdaptor, typename... Rng>
		struct range_value<CartesianProductAdaptor, cartesian_product_adaptor<Rng...>, tc::void_t<tc::range_value_t<Rng>...>> final {
			using type = tc::tuple<tc::range_value_t<Rng>...>;
		};
	}

	template<typename... Rng>
	constexpr cartesian_product_adaptor<Rng...> cartesian_product(Rng&&... rng) noexcept {
		return {tc::aggregate_tag, std::forward<Rng>(rng)...};
	}
}
