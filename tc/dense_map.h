
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "array.h"
#include "base/enum.h"
#include "base/tag_type.h"
#include "base/as_lvalue.h"
#include "algorithm/algorithm.h"
#include "algorithm/compare.h"
#include "container/cont_assign.h"
#include "range/iota_range.h"
#include "range/zip_range.h"
#include "range/cartesian_product_adaptor.h"

#ifdef TC_PRIVATE
#include "Library/Persistence/types.h"
#endif

namespace tc {
	namespace no_adl {
		template<>
		struct [[nodiscard]] all_values<bool> final {
			static auto constexpr c_rngsete = tc::transform(tc::iota_range_constant<0, 2>(), tc::fn_static_cast<bool>());

		public:
			static constexpr auto begin() noexcept {
				return tc::begin(c_rngsete);
			}
			static constexpr auto end() noexcept {
				return tc::end(c_rngsete);
			}

			static constexpr auto size() {
				return tc::least_uint_constant<2>{};
			}

			static constexpr std::size_t index_of(bool const b) noexcept {
				return tc::to_underlying(b);
			}
#ifdef _MSC_VER
		private:
			static auto constexpr _natvis_begin = false; // natvis visualizations cannot call functions even if they are constexpr.
#endif
		};

		template<>
		struct [[nodiscard]] all_values<std::weak_ordering> final {
			static auto constexpr c_rngsetorders = tc::make_array(tc::aggregate_tag, std::weak_ordering::less, std::weak_ordering::equivalent, std::weak_ordering::greater);

		public:
			static constexpr auto begin() noexcept {
				return tc::begin(c_rngsetorders);
			}
			static constexpr auto end() noexcept {
				return tc::end(c_rngsetorders);
			}

			static constexpr auto size() noexcept {
				return tc::least_uint_constant<3>{};
			}

			static constexpr std::size_t index_of(std::weak_ordering order) noexcept {
				return tc::find_unique<tc::return_element_index>(c_rngsetorders, order);
			}
		};

		template<>
		struct [[nodiscard]] all_values<std::partial_ordering> final {
			static auto constexpr c_rngsetorders = tc::make_array(tc::aggregate_tag, std::partial_ordering::less, std::partial_ordering::equivalent, std::partial_ordering::greater, std::partial_ordering::unordered);

		public:
			static constexpr auto begin() noexcept {
				return tc::begin(c_rngsetorders);
			}
			static constexpr auto end() noexcept {
				return tc::end(c_rngsetorders);
			}

			static constexpr auto size() noexcept {
				return tc::least_uint_constant<4>{};
			}

			static constexpr std::size_t index_of(std::partial_ordering order) noexcept {
				return tc::find_unique<tc::return_element_index>(c_rngsetorders, order);
			}
		};
		
		template<typename... Ts>
		struct [[nodiscard]] all_values<tc::tuple<Ts...>> final : decltype(tc::cartesian_product(tc::all_values<Ts>()...)) {
			constexpr std::size_t index_of(tc::tuple<Ts...> const& tpl) const& noexcept {
				return tc::find_unique<tc::return_element_index>(tc::cartesian_product(tc::all_values<Ts>()...), tpl);
			}
		};
	}

	// all_values are views
	namespace dense_map_adl {
#ifdef TC_PRIVATE
		template< typename Key, typename Value >
		struct dense_map;

		template< typename Key, typename Value >
		void LoadType_impl( dense_map<Key, Value>& dm, CXmlReader& loadhandler ) THROW(ExLoadFail);
#endif
		template< typename Key, typename Value >
		struct dense_map {
			static constexpr tc::all_values<Key> c_rngkey{};
		private:
			template<typename KeyOther, typename ValueOther>
			friend struct dense_map;

			template<typename ValueOther>
			using other_dense_map = dense_map<Key, ValueOther>;

			static_assert(std::is_reference<Value>::value ? std::is_lvalue_reference<Value>::value : !std::is_const<Value>::value && !std::is_volatile<Value>::value);

			using Array = std::conditional_t<
				std::is_reference<Value>::value,
				tc::array<Value, tc::size(c_rngkey)>,
				std::array<Value, tc::size(c_rngkey)>
			>;

			Array m_a;
		public:
			using dense_map_key_type = Key;

			// We cannot tell if *this is constructed using value-initialization syntax or default-initialization syntax. Therefore, we must value-initialize here.
			constexpr dense_map() noexcept(std::is_nothrow_default_constructible<decltype(m_a)>::value) requires std::is_default_constructible<decltype(m_a)>::value: m_a{} {}

			// std::is_default_constructible checks for value-initialization, instead of default-initialization. However, T cannot be const, so there should be no observable difference
			explicit dense_map(boost::container::default_init_t) noexcept(std::is_nothrow_constructible<Value>::value) requires std::is_default_constructible<Value>::value {}

			template<typename... Args> requires (0 < sizeof...(Args)) // dense_map(fill_tag) could exist, but it should be explicit
			constexpr dense_map( tc::fill_tag_t, Args&&... val ) noexcept
			: tc_member_init_cast(m_a, tc::fill_tag, std::forward<Args>(val)...) {}

			template<typename Rng>
			constexpr dense_map(tc::range_tag_t, Rng&& rng) noexcept : tc_member_init_cast( m_a, std::forward<Rng>(rng) ) {}

			// aggregate construction of tc::dense_map does not require tc::aggregate_tag because
			//   1. number of arguments must be the same as the size of dense_map,
			//   2. it's very inconvenient for derived types, e.g. geo types.
			// make sure forwarding ctor has at least two parameters, so no ambiguity with filling ctor and implicit copy/move ctors
			template< typename First, typename Second, typename... Args>
				requires
					(!tc::tag<std::remove_reference_t<First>>)
					&& (tc::econstructionIMPLICIT == tc::elementwise_construction_restrictiveness<Value, First&&, Second&&, Args&&...>::value)
			constexpr dense_map(First&& first, Second&& second, Args&& ...args) noexcept(noexcept(tc::explicit_cast<decltype(m_a)>(tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...)))
			: tc_member_init_cast(m_a, tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...) {}

			template< typename First, typename Second, typename... Args>
				requires
					(!tc::tag<std::remove_reference_t<First>>)
					&& (tc::econstructionEXPLICIT == tc::elementwise_construction_restrictiveness<Value, First&&, Second&&, Args&&...>::value)
			constexpr explicit dense_map(First&& first, Second&& second, Args&& ...args) noexcept(noexcept(tc::explicit_cast<decltype(m_a)>(tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...)))
				: tc_member_init_cast( m_a, tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)... )
			{}

			template< typename Func > requires tc::invocable<Func&, Key>
			constexpr dense_map(tc::func_tag_t, Func func) MAYTHROW
				: tc_member_init_cast( m_a, tc::func_tag, [&func](std::size_t const n) MAYTHROW -> Value { // force return of Value
					//STATICASSERTSAME(decltype(tc_at_nodebug(c_rngkey, n))&&, Key&&); does not hold, if c_rngkey is all_values<tuple<...>>
					// TODO Remove static_cast<Key> below after we change counting_iterator::operator*() to return small trivial by value.
					static_assert(
						std::is_same<Value, decltype(tc::invoke(func, std::declval<Key>()))>::value || // guaranteed copy elision, Value does not need to be copy/move constructible
						tc::safely_constructible_from<Value, decltype(tc::invoke(func, std::declval<Key>()))>
					);
					return tc::invoke(func, static_cast<Key>(tc_at_nodebug(c_rngkey, n)));
				} )
			{}

			template< typename Func > requires (!tc::invocable<Func&, Key>)
			constexpr dense_map(tc::func_tag_t, Func func) MAYTHROW
				: dense_map(tc::func_tag, [&](auto const key) MAYTHROW -> Value {
					return {tc::func_tag, [&](auto const... keys) return_decltype_MAYTHROW( tc::invoke(func, key, keys...) )};
				})
			{}

			template< typename Value2 >
			explicit dense_map(Key keyPri, other_dense_map<Value2> const& mapOther) noexcept(std::is_nothrow_constructible<dense_map, Value2 const&, Value2 const&>::value)
				: dense_map(mapOther[keyPri], mapOther[~keyPri])
			{}

			template< typename Value2 >
			explicit dense_map(Key keyPri, other_dense_map<Value2>&& mapOther) noexcept(std::is_nothrow_constructible<dense_map, Value2, Value2>::value)
				: dense_map(tc_move_always(mapOther[keyPri]), tc_move_always(mapOther[~keyPri]))
			{}

			template< typename Func, typename Value2 >
			constexpr dense_map(transform_tag_t, other_dense_map<Value2> const& mapOther, Func&& func) MAYTHROW
				: tc_member_init_cast( m_a, tc::transform(mapOther.m_a, std::forward<Func>(func)) )
			{}

			template< typename Func, typename Value2 >
			constexpr dense_map(transform_tag_t, other_dense_map<Value2>&& mapOther, Func func) MAYTHROW
				: tc_member_init_cast( m_a,
					// TODO rvalue elements for rvalue ranges: tc::transform(tc_move(mapOther.m_a), std::forward<Func>(func))
					tc::transform(mapOther.m_a, tc::chained(std::forward<Func>(func), tc::fn_static_cast<Value2&&>()))
				)
			{}

			DEFINE_MEMBER_TRANSFORM(other_dense_map, tc::type::deducible_identity_t, Value)

			template< typename ValuePri, typename ValueSec >
			constexpr dense_map(Key keyPri, ValuePri&& valPri, ValueSec&& valSec) MAYTHROW :
				dense_map(
					tc_conditional_rvalue_as_ref(tc_front_nodebug(c_rngkey) == keyPri, std::forward<ValuePri>(valPri), std::forward<ValueSec>(valSec)),
					tc_conditional_rvalue_as_ref(tc_front_nodebug(c_rngkey) == keyPri, std::forward<ValueSec>(valSec), std::forward<ValuePri>(valPri))
				)
			{}
			template <typename Value2>
				requires (tc::econstructionIMPLICIT==tc::construction_restrictiveness<Value, Value2 const&>::value)
			constexpr dense_map(other_dense_map<Value2> const& mapOther) noexcept(std::is_nothrow_constructible<Value, Value2 const&>::value)
				: tc_member_init_cast( m_a, mapOther.m_a )
			{}

			template <typename Value2>
				requires (tc::econstructionEXPLICIT==tc::construction_restrictiveness<Value, Value2 const&>::value)
			constexpr explicit dense_map(other_dense_map<Value2> const& mapOther) noexcept(std::is_nothrow_constructible<dense_map, transform_tag_t, decltype(mapOther), tc::fn_explicit_cast_with_rounding<Value>>::value)
				: dense_map(transform_tag, mapOther, tc::fn_explicit_cast_with_rounding<Value>()) // TODO: replace with tc::fn_explicit_cast as soon as geometry no longer relies on it
			{}

			template <typename Value2>
				requires (tc::econstructionIMPLICIT==tc::construction_restrictiveness<Value, Value2&&>::value)
			constexpr dense_map(other_dense_map<Value2>&& mapOther) noexcept(std::is_nothrow_constructible<Value, Value2&&>::value)
				: tc_member_init_cast( m_a, tc_move(mapOther).m_a )
			{}

			template <typename Value2>
				requires (tc::econstructionEXPLICIT==tc::construction_restrictiveness<Value, Value2&&>::value)
			constexpr explicit dense_map(other_dense_map<Value2>&& mapOther) noexcept(std::is_nothrow_constructible<dense_map, transform_tag_t, decltype(tc_move(mapOther)), tc::fn_explicit_cast_with_rounding<Value>>::value)
				: dense_map(transform_tag, tc_move(mapOther), tc::fn_explicit_cast_with_rounding<Value>()) // TODO: replace with tc::fn_explicit_cast as soon as geometry no longer relies on it
			{}

			template <typename Value2>
				requires (tc::safely_assignable_from<Value&, Value2 const&>)
			dense_map& operator=(other_dense_map<Value2> const& rhs) & noexcept(std::is_nothrow_assignable<Array, typename other_dense_map<Value2>::Array const&>::value) {
				tc::cont_assign(m_a, rhs.m_a);
				return *this;
			}

			template <typename Value2>
				requires (tc::safely_assignable_from<Value&, Value2&&>)
			dense_map& operator=(other_dense_map<Value2>&& rhs) & noexcept(std::is_nothrow_assignable<Array, typename other_dense_map<Value2>::Array&&>::value) {
				tc::cont_assign(m_a, tc_move(rhs.m_a));
				return *this;
			}

			// access
			[[nodiscard]] constexpr Value& operator[](Key key) & noexcept {
				if constexpr (std::is_reference<Value>::value) {
					return m_a[c_rngkey.index_of(key)];
				} else {
					return tc::at(m_a, c_rngkey.index_of(key));
				}
			}
			[[nodiscard]] constexpr Value const& operator[](Key key) const& noexcept {
				if constexpr (std::is_reference<Value>::value) {
					return m_a[c_rngkey.index_of(key)];
				} else {
					return tc::at(m_a, c_rngkey.index_of(key));
				}
			}
			[[nodiscard]] constexpr Value&& operator[](Key key) && noexcept {
				return static_cast<Value &&>((*this)[key]);
			}
			[[nodiscard]] constexpr Value const&& operator[](Key key) const&& noexcept {
				return static_cast<Value const&&>((*this)[key]);
			}

			// comparison
			template<typename Key_, typename LHS, typename RHS>
			friend constexpr bool operator==( dense_map<Key_, LHS> const& lhs, dense_map<Key_, RHS> const& rhs ) noexcept;

			// iterators
			constexpr auto begin() const& noexcept {
				return tc::begin(m_a);
			}
			constexpr auto end() const& noexcept {
				return tc::end(m_a);
			}
			constexpr auto begin() & noexcept {
				return tc::begin(m_a);
			}
			constexpr auto end() & noexcept {
				return tc::end(m_a);
			}

			static constexpr auto size() noexcept {
				return tc::constexpr_size<tc::all_values<Key>>;
			}
			
#ifdef _DEBUG
			friend void uninitialize_impl(dense_map& dm) noexcept {
				UNINITIALIZED(dm.m_a);
			}
#endif

#ifdef TC_PRIVATE
			// persistence
			friend void LoadType_impl<>( dense_map<Key, Value>& dm, CXmlReader& loadhandler ) THROW(ExLoadFail);
			void DoSave(CSaveHandler& savehandler) const& MAYTHROW;

		private:
			template<typename HashAlgorithm>
			void hash_append_impl(HashAlgorithm& h) const& noexcept;

			template<typename HashAlgorithm>
			friend void hash_append_impl(HashAlgorithm& h, dense_map const& dm) noexcept {
				dm.hash_append_impl(h);
			}
#endif
		};

		template<typename Key_, typename LHS, typename RHS>
		[[nodiscard]] constexpr bool operator==(dense_map<Key_, LHS> const& lhs, dense_map<Key_, RHS> const& rhs) noexcept {
			return EQUAL_MEMBERS(m_a);
		}

	} // namespace dense_map_adl
	using dense_map_adl::dense_map;

	template <typename Map, typename T>
	concept indexed_by = std::same_as<typename Map::dense_map_key_type, T>;

	////////////////////
	// special RangeReturns for dense_maps

	struct return_element_key final {
		static constexpr bool requires_iterator = true;

		template<typename It, typename DenseMap>
		static auto pack_element(It&& it, DenseMap&& rng, tc::unused /*ref*/={}) noexcept {
			return tc_at_nodebug(rng.c_rngkey, it - tc::begin(rng));
		}
		template<typename DenseMap>
		static auto pack_no_element(DenseMap&& rng) noexcept {
			_ASSERTFALSE;
			return tc_front_nodebug(rng.c_rngkey);
		}
	};

	struct return_element_key_or_none final {
		static constexpr bool requires_iterator = true;

		template<typename It, typename DenseMap>
		static std::optional<typename std::remove_reference_t<DenseMap>::dense_map_key_type> pack_element(It&& it, DenseMap&& rng, tc::unused /*ref*/={}) noexcept {
			return tc_at_nodebug(rng.c_rngkey, it - tc::begin(rng));
		}
		template<typename DenseMap>
		static std::optional<typename std::remove_reference_t<DenseMap>::dense_map_key_type> pack_no_element(DenseMap&&) noexcept {
			return std::nullopt;
		}
	};

	template <typename Key, typename Func>
	[[nodiscard]] constexpr auto make_dense_map(tc::func_tag_t, Func&& func) return_ctor_MAYTHROW(
		TC_FWD(tc::dense_map<Key, tc::decay_t<decltype(tc::invoke(tc::as_lvalue(tc::decay_copy(func)), tc_front_nodebug(tc::all_values<Key>())))>>),
		(tc::func_tag, std::forward<Func>(func))
	)

	template <typename Key, /*always deduce Ts, otherwise use dense_map<Key, T>*/int = 0, typename... Ts>
	[[nodiscard]] constexpr auto make_dense_map(tc::fill_tag_t, Ts&&... ts) noexcept {
		return tc::dense_map<Key, tc::common_type_t<Ts...>>(tc::fill_tag, std::forward<Ts>(ts)...);
	}

	template <typename Key, typename Rng>
	[[nodiscard]] constexpr auto make_dense_map(tc::range_tag_t, Rng&& rng) noexcept {
		return tc::dense_map<Key, tc::range_value_t<Rng>>(tc::range_tag, std::forward<Rng>(rng));
	}

	template <typename Key, /*always deduce Ts, otherwise use dense_map<Key, T>*/int = 0, typename... Ts>
	[[nodiscard]] constexpr auto make_dense_map(Ts&&... ts) noexcept {
		return tc::dense_map<Key, tc::common_type_t<Ts...>>(std::forward<Ts>(ts)...);
	}

	namespace no_adl {
		///////////////////
		// all_values specialization dense_maps
		template< typename Key, typename Value >
		struct [[nodiscard]] all_values<tc::dense_map<Key,Value>> final {
		private:
			using baserng = tc::all_values<tc::type::apply_t<tc::tuple, tc::type::repeat_n_t<tc::size(tc::all_values<Key>()), Value>>>;
			static constexpr baserng c_baserng{};
			static constexpr auto c_rngmapkv = tc::transform(c_baserng, tc_fn(tc::dense_map<Key, Value>));

		public:
			static constexpr auto begin() noexcept {
				return tc::begin(c_rngmapkv);
			}
			static constexpr auto end() noexcept {
				return tc::end(c_rngmapkv);
			}

			static constexpr auto size() noexcept {
				return tc::constexpr_size<baserng>;
			}

			static constexpr std::size_t index_of(tc::dense_map<Key, Value> const& mapkv) noexcept {
				return c_baserng.index_of(tc::apply([&](auto... key) { return tc::make_tuple(mapkv[key]...); }, tc::all_constants<Key>));
			}
#ifdef _MSC_VER
		private:
			static auto constexpr _natvis_begin = false; // natvis visualizations cannot call functions even if they are constexpr.
#endif
		};
	}

	namespace less_key_adl {
		template<typename Key, typename ValueLhs, typename ValueRhs>
		bool less_key_helper(less_key_tag_t, tc::dense_map<Key, ValueLhs> const& lhs, tc::dense_map<Key, ValueRhs> const& rhs) noexcept {
			return std::is_lt(tc::lexicographical_compare_3way(lhs, rhs));
		}
	}

	template<typename DenseMap, typename... DenseMaps> requires
		tc::instance_or_derived<std::remove_reference_t<DenseMap>, tc::dense_map> &&
		(... && tc::instance_or_derived<std::remove_reference_t<DenseMaps>, tc::dense_map>)
	[[nodiscard]] constexpr auto zip(DenseMap&& dm, DenseMaps&&... dms) noexcept {
		static_assert((tc::indexed_by<std::remove_reference_t<DenseMaps>, typename std::remove_reference_t<DenseMap>::dense_map_key_type> && ...));

		return tc::make_dense_map<
			typename std::remove_reference_t<DenseMap>::dense_map_key_type
		>(tc::func_tag, [&](auto const key) noexcept
			-> tc::tuple<tc::xvalue_decay_t<decltype(std::declval<DenseMap>()[key])>, tc::xvalue_decay_t<decltype(std::declval<DenseMaps>()[key])>...>
		{
			return {std::forward<DenseMap>(dm)[key], std::forward<DenseMaps>(dms)[key]...};
		});
	}

	template<typename DenseMap> requires tc::instance_or_derived<std::remove_reference_t<DenseMap>, tc::dense_map>
	[[nodiscard]] constexpr auto enumerate(DenseMap&& dm) noexcept {
		using Key = typename std::remove_reference_t<DenseMap>::dense_map_key_type;
		return tc::zip(tc::dense_map<Key, Key>(tc::func_tag, tc::identity()), std::forward<DenseMap>(dm));
	}

	namespace dense_map_adl {
		template<
			typename NestedDenseMap,
			typename FirstKey = typename std::remove_reference_t<NestedDenseMap>::dense_map_key_type,
			typename SecondKey = typename std::remove_reference_t<tc::range_value_t<NestedDenseMap>>::dense_map_key_type
		>
		auto join_impl(NestedDenseMap&& dm) noexcept
			-> tc::dense_map<
				tc::tuple<FirstKey, SecondKey>,
				tc::xvalue_decay_t<decltype(std::declval<NestedDenseMap>()[std::declval<FirstKey>()][std::declval<SecondKey>()])>
			>
		{
			return {tc::func_tag, [&](auto const key0, auto const key1) return_decltype_xvalue_by_ref_noexcept(
				std::forward<NestedDenseMap>(dm)[key0][key1]
			)};
		}
	}
}

#define TC_DENSE_MAP_SUPPORT_1(class_name) \
	TC_DENSE_MAP_SUPPORT_2(class_name, tc::type::deducible_identity_t)

#define TC_DENSE_MAP_SUPPORT_2(class_name, value_template) \
	class_name() noexcept(std::is_nothrow_default_constructible<base_>::value) requires std::is_default_constructible<base_>::value \
		: base_() {} \
	\
	explicit class_name(boost::container::default_init_t) noexcept(std::is_nothrow_constructible<base_, boost::container::default_init_t>::value) requires std::is_constructible<base_, boost::container::default_init_t>::value \
		: base_(boost::container::default_init) {} \
	\
	/* inherit default ctor and constructors with at least two arguments from dense_map*/ \
	template <typename A0, typename A1, typename... Args> \
		requires (tc::econstructionIMPLICIT==tc::construction_restrictiveness<base_, A0&&, A1&&, Args&&...>::value) \
	constexpr class_name(A0&& a0, A1&& a1, Args&& ... args) noexcept(std::is_nothrow_constructible<base_, A0, A1, Args...>::value) \
		: base_(std::forward<A0>(a0), std::forward<A1>(a1), std::forward<Args>(args)...) \
	{} \
	\
	template <typename A0, typename A1, typename... Args> \
		requires (tc::econstructionEXPLICIT==tc::construction_restrictiveness<base_, A0&&, A1&&, Args&&...>::value) \
	constexpr explicit class_name(A0&& a0, A1&& a1, Args&& ... args) noexcept(std::is_nothrow_constructible<base_, A0, A1, Args...>::value) \
		: base_(std::forward<A0>(a0), std::forward<A1>(a1), std::forward<Args>(args)...) \
	{} \
	\
	/* inherit implicit copy and move constructors from dense_map (only if argument is actual class_name<T>)*/ \
	template <typename T2> \
		requires (tc::econstructionIMPLICIT==tc::construction_restrictiveness<base_, typename class_name<T2>::base_ const&>::value) \
	class_name(class_name<T2> const& other) noexcept(std::is_nothrow_constructible<base_, typename class_name<T2>::base_ const&>::value) \
		: base_(tc::base_cast<typename class_name<T2>::base_>(other)) \
	{} \
	\
	template <typename T2> \
		requires (tc::econstructionIMPLICIT==tc::construction_restrictiveness<base_, typename class_name<T2>::base_&&>::value) \
	class_name(class_name<T2>&& other) noexcept(std::is_nothrow_constructible<base_, typename class_name<T2>::base_&&>::value) \
		: base_(tc::base_cast<typename class_name<T2>::base_>(tc_move(other))) \
	{} \
	\
	/* inherit implicit and explicit copy and move constructors from dense_map (only if argument is actual dense_map<base_::dense_map_key_type, T>) as explict constructors*/ \
	template <typename K, typename T2> \
		requires tc::explicit_castable_from<base_, tc::dense_map<K, T2> const&> \
	explicit class_name(tc::dense_map<K, T2> const& other) noexcept(std::is_nothrow_constructible<base_, tc::dense_map<K, T2> const&>::value) \
		: base_(other) \
	{ \
		STATICASSERTSAME(K, typename base_::dense_map_key_type); /*Should use tc::dense_map<dense_map_key_type, T2> for parameter above, but MSVC turns this into dense_map<int, T2>*/ \
	} \
	template <typename K, typename T2> \
		requires tc::explicit_castable_from<base_, tc::dense_map<K, T2>&&> \
	explicit class_name(tc::dense_map<K, T2>&& other) noexcept(std::is_nothrow_constructible<base_, tc::dense_map<K, T2>&&>::value) \
		: base_(other) \
	{ \
		STATICASSERTSAME(K, typename base_::dense_map_key_type); /*Should use tc::dense_map<dense_map_key_type, T2> for parameter above, but MSVC turns this into dense_map<int, T2>*/ \
	} \
	\
	/* inherit assignment */ \
	template <typename T2> \
		requires tc::safely_assignable_from<base_&, typename class_name<T2>::base_ const&> \
	class_name& operator=(class_name<T2> const& rhs) & noexcept(std::is_nothrow_assignable<base_, typename class_name<T2>::base_ const&>::value) { \
		tc::base_cast<base_>(*this)=tc::base_cast<typename class_name<T2>::base_>(rhs); \
		return *this; \
	} \
	\
	template <typename T2> \
		requires tc::safely_assignable_from<base_&, typename class_name<T2>::base_&&> \
	class_name& operator=(class_name<T2>&& rhs) & noexcept(std::is_nothrow_assignable<base_, typename class_name<T2>::base_&&>::value) { \
		tc::base_cast<base_>(*this)=tc::base_cast<typename class_name<T2>::base_>(tc_move(rhs)); \
		return *this; \
	} \
	\
	/* inherit transform */ \
	DEFINE_MEMBER_TRANSFORM(class_name, value_template)

#define TC_DENSE_MAP_SUPPORT(...) \
	TC_EXPAND(BOOST_PP_OVERLOAD(TC_DENSE_MAP_SUPPORT_, __VA_ARGS__)(__VA_ARGS__))

#define TC_DENSE_MAP_DEDUCTION_GUIDES_1(template_name) \
	TC_DENSE_MAP_DEDUCTION_GUIDES_2(template_name, tc::type::deducible_identity_t)

#define TC_DENSE_MAP_DEDUCTION_GUIDES_2(template_name, type_function) \
	template< typename Arg > \
	template_name(tc::fill_tag_t, Arg&&) -> template_name< type_function< tc::decay_t<Arg> > >; \
	template< typename Arg, typename... Args> requires (std::is_same<tc::decay_t<Arg>, tc::decay_t<Args>>::value && ...) \
	template_name(Arg&&, Args&&...) -> template_name<type_function<tc::decay_t<Arg>>>; \
	template< typename Rng > \
	template_name(tc::range_tag_t, Rng&&) -> template_name< type_function< tc::range_value_t<Rng> > >; \
	template< typename DenseMap, typename Func > \
	template_name(tc::transform_tag_t, DenseMap&&, Func&&) -> template_name< type_function< tc::transform_value_t<Func, decltype(std::declval<DenseMap>()[{}])> > >; \
	template< typename Key, typename ValuePri, typename ValueSec > requires std::same_as<tc::decay_t<ValuePri>, tc::decay_t<ValueSec>> && std::same_as<Key, typename template_name<type_function<tc::decay_t<ValuePri>>>::dense_map_key_type> \
	template_name(Key, ValuePri&&, ValueSec&&) -> template_name<type_function<tc::decay_t<ValuePri>>>;

#define TC_DENSE_MAP_DEDUCTION_GUIDES(...) \
	TC_EXPAND(BOOST_PP_OVERLOAD(TC_DENSE_MAP_DEDUCTION_GUIDES_, __VA_ARGS__)(__VA_ARGS__))
