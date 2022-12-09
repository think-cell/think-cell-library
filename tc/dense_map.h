
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "array.h"
#include "base/enum.h"
#include "base/tag_type.h"
#include "base/as_lvalue.h"
#include "algorithm/compare.h"
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
			static auto constexpr c_rngsete = tc::transform(tc::iota(0, 2), tc::fn_static_cast<bool>());

		public:
			static constexpr auto begin() noexcept {
				return tc::begin(c_rngsete);
			}
			static constexpr auto end() noexcept {
				return tc::end(c_rngsete);
			}

			using const_iterator = decltype(tc::begin(c_rngsete));
			using iterator = const_iterator;

			static constexpr std::size_t index_of(bool b) noexcept {
				return tc::underlying_cast(b);
			}
#ifdef _MSC_VER
		private:
			static auto constexpr _natvis_begin = false; // natvis visualizations cannot call functions even if they are constexpr.
#endif
		};

		template<>
		struct constexpr_size_base<all_values<bool>> : tc::constant<tc::explicit_cast<std::uint8_t>(2)> {};

		template<>
		struct [[nodiscard]] all_values<std::strong_ordering> final {
			static auto constexpr c_rngsetorders = tc::make_array(tc::aggregate_tag, std::strong_ordering::less, std::strong_ordering::equal, std::strong_ordering::greater);

		public:
			static constexpr auto begin() noexcept {
				return tc::begin(c_rngsetorders);
			}
			static constexpr auto end() noexcept {
				return tc::end(c_rngsetorders);
			}

			using const_iterator = decltype(tc::begin(c_rngsetorders));
			using iterator = const_iterator;

			static constexpr std::size_t index_of(std::strong_ordering order) noexcept {
				return tc::find_unique<tc::return_element_index>(c_rngsetorders, order);
			}
		};

		template<>
		struct constexpr_size_base<all_values<std::strong_ordering>> : tc::constant<tc::explicit_cast<std::uint8_t>(3)> {};

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

			using const_iterator = decltype(tc::begin(c_rngsetorders));
			using iterator = const_iterator;

			static constexpr std::size_t index_of(std::weak_ordering order) noexcept {
				return tc::find_unique<tc::return_element_index>(c_rngsetorders, order);
			}
		};

		template<>
		struct constexpr_size_base<all_values<std::weak_ordering>> : tc::constant<tc::explicit_cast<std::uint8_t>(3)> {};

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

			using const_iterator = decltype(tc::begin(c_rngsetorders));
			using iterator = const_iterator;

			static constexpr std::size_t index_of(std::partial_ordering order) noexcept {
				return tc::find_unique<tc::return_element_index>(c_rngsetorders, order);
			}
		};

		template<>
		struct constexpr_size_base<all_values<std::partial_ordering>> : tc::constant<tc::explicit_cast<std::uint8_t>(4)> {};
		
		template<typename... Ts>
		struct [[nodiscard]] all_values<tc::tuple<Ts...>> final : decltype(tc::cartesian_product(tc::all_values<Ts>()...)) {
		private:
			using base_ = decltype(tc::cartesian_product(tc::all_values<Ts>()...));
		public:
			constexpr all_values()
			: base_(tc::cartesian_product(tc::all_values<Ts>()...)) {
			}

			constexpr std::size_t index_of(tc::tuple<Ts...> const& tpl) const& noexcept {
				return tc::find_unique<tc::return_element_index>(*this, tpl);
			}
		};

		template<typename... Ts>
		struct constexpr_size_base<all_values<tc::tuple<Ts...>>> : tc::constant<tc::size_raw(all_values<tc::tuple<Ts...>>())> {};
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
			template<ENABLE_SFINAE> requires std::is_default_constructible<SFINAE_TYPE(decltype(m_a))>::value // ctors - VS compiler 19.15.26726 didn't sfinae correctly on Array, using decltype(m_a) instead
			constexpr dense_map() noexcept(std::is_nothrow_default_constructible<decltype(m_a)>::value): m_a{} {}

			// std::is_default_constructible checks for value-initialization, instead of default-initialization. However, T cannot be const, so there should be no observable difference
			template<ENABLE_SFINAE> requires std::is_default_constructible<SFINAE_TYPE(Value)>::value
			explicit dense_map(boost::container::default_init_t) noexcept(std::is_nothrow_constructible<Value>::value) {}

			template<typename... Args> requires (0 < sizeof...(Args)) // dense_map(fill_tag) could exist, but it should be explicit
			constexpr dense_map( tc::fill_tag_t, Args&&... val ) noexcept
			: MEMBER_INIT_CAST(m_a, tc::fill_tag, std::forward<Args>(val)...) {}

			template<typename Rng>
			constexpr dense_map(tc::range_tag_t, Rng&& rng) noexcept : MEMBER_INIT_CAST( m_a, std::forward<Rng>(rng) ) {}

			// aggregate construction of tc::dense_map does not require tc::aggregate_tag because
			//   1. number of arguments must be the same as the size of dense_map,
			//   2. it's very inconvenient for derived types, e.g. geo types.
			// make sure forwarding ctor has at least two parameters, so no ambiguity with filling ctor and implicit copy/move ctors
			template< typename First, typename Second, typename... Args>
				requires
					(!tc::tag<std::remove_reference_t<First>>)
					&& (tc::econstructionIMPLICIT == tc::elementwise_construction_restrictiveness<Value, First&&, Second&&, Args&&...>::value)
			constexpr dense_map(First&& first, Second&& second, Args&& ...args) noexcept(noexcept(tc::explicit_cast<decltype(m_a)>(tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...)))
			: MEMBER_INIT_CAST(m_a, tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...) {}

			template< typename First, typename Second, typename... Args>
				requires
					(!tc::tag<std::remove_reference_t<First>>)
					&& (tc::econstructionEXPLICIT == tc::elementwise_construction_restrictiveness<Value, First&&, Second&&, Args&&...>::value)
			constexpr explicit dense_map(First&& first, Second&& second, Args&& ...args) noexcept(noexcept(tc::explicit_cast<decltype(m_a)>(tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...)))
				: MEMBER_INIT_CAST( m_a, tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)... )
			{}

			template< typename Func > requires tc::is_invocable<Func&, Key>::value
			constexpr dense_map(tc::func_tag_t, Func func) MAYTHROW
				: MEMBER_INIT_CAST( m_a, tc::func_tag, [&func](std::size_t n) MAYTHROW -> Value { // force return of Value
					STATICASSERTSAME(decltype(tc_at_nodebug(c_rngkey, n))&&, Key&&);
					static_assert(
						std::is_same<Value, decltype(func(std::declval<Key>()))>::value || // guaranteed copy elision, Value does not need to be copy/move constructible
						tc::is_safely_constructible<Value, decltype(func(std::declval<Key>()))>::value
					);
					return func(tc_at_nodebug(c_rngkey, n));
				} )
			{}

			template< typename Func > requires (!tc::is_invocable<Func&, Key>::value)
			constexpr dense_map(tc::func_tag_t, Func func) MAYTHROW
				: dense_map(tc::func_tag, [&](auto const key) MAYTHROW -> Value {
					return {tc::func_tag, [&](auto const... keys) return_decltype_MAYTHROW( func(key, keys...) )};
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
				: MEMBER_INIT_CAST( m_a, tc::transform(mapOther.m_a, std::forward<Func>(func)) )
			{}

			template< typename Func, typename Value2 >
			constexpr dense_map(transform_tag_t, other_dense_map<Value2>&& mapOther, Func func) MAYTHROW
				: MEMBER_INIT_CAST( m_a,
					// TODO rvalue elements for rvalue ranges: tc::transform(tc_move(mapOther.m_a), std::forward<Func>(func))
					tc::transform(mapOther.m_a, [&](auto& elem) MAYTHROW -> decltype(auto) {
						return func(static_cast<Value2&&>(elem));
					})
				)
			{}

			DEFINE_MEMBER_TRANSFORM(other_dense_map, tc::type::deducible_identity_t, Value)

			template< typename ValuePri, typename ValueSec >
			constexpr dense_map(Key keyPri, ValuePri&& valPri, ValueSec&& valSec) MAYTHROW :
				dense_map(
					CONDITIONAL_RVALUE_AS_REF(tc_front_nodebug(c_rngkey) == keyPri, std::forward<ValuePri>(valPri), std::forward<ValueSec>(valSec)),
					CONDITIONAL_RVALUE_AS_REF(tc_front_nodebug(c_rngkey) == keyPri, std::forward<ValueSec>(valSec), std::forward<ValuePri>(valPri))
				)
			{}
			template <typename Value2>
				requires (tc::econstructionIMPLICIT==tc::construction_restrictiveness<Value, Value2 const&>::value)
			constexpr dense_map(other_dense_map<Value2> const& mapOther) noexcept(std::is_nothrow_constructible<Value, Value2 const&>::value)
				: MEMBER_INIT_CAST( m_a, mapOther.m_a )
			{}

			template <typename Value2>
				requires (tc::econstructionEXPLICIT==tc::construction_restrictiveness<Value, Value2 const&>::value)
			constexpr explicit dense_map(other_dense_map<Value2> const& mapOther) noexcept(std::is_nothrow_constructible<dense_map, transform_tag_t, decltype(mapOther), tc::fn_explicit_cast_with_rounding<Value>>::value)
				: dense_map(transform_tag, mapOther, tc::fn_explicit_cast_with_rounding<Value>()) // TODO: replace with tc::fn_explicit_cast as soon as geometry no longer relies on it
			{}

			template <typename Value2>
				requires (tc::econstructionIMPLICIT==tc::construction_restrictiveness<Value, Value2&&>::value)
			constexpr dense_map(other_dense_map<Value2>&& mapOther) noexcept(std::is_nothrow_constructible<Value, Value2&&>::value)
				: MEMBER_INIT_CAST( m_a, tc_move(mapOther).m_a )
			{}

			template <typename Value2>
				requires (tc::econstructionEXPLICIT==tc::construction_restrictiveness<Value, Value2&&>::value)
			constexpr explicit dense_map(other_dense_map<Value2>&& mapOther) noexcept(std::is_nothrow_constructible<dense_map, transform_tag_t, decltype(tc_move(mapOther)), tc::fn_explicit_cast_with_rounding<Value>>::value)
				: dense_map(transform_tag, tc_move(mapOther), tc::fn_explicit_cast_with_rounding<Value>()) // TODO: replace with tc::fn_explicit_cast as soon as geometry no longer relies on it
			{}

			template <typename Value2>
				requires (tc::is_safely_assignable<Value&, Value2 const&>::value)
			dense_map& operator=(other_dense_map<Value2> const& rhs) & noexcept(std::is_nothrow_assignable<Array, typename other_dense_map<Value2>::Array const&>::value) {
				tc::cont_assign(m_a, rhs.m_a);
				return *this;
			}

			template <typename Value2>
				requires (tc::is_safely_assignable<Value&, Value2&&>::value)
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

			template<ENABLE_SFINAE>
			[[nodiscard]] constexpr auto data() & return_decltype_noexcept(
				SFINAE_VALUE(m_a).data()
			)

			template<ENABLE_SFINAE>
			[[nodiscard]] constexpr auto data() const& return_decltype_noexcept(
				SFINAE_VALUE(m_a).data()
			)

		private:
			template<typename Self>
			static constexpr auto enumerate_(Self&& self) noexcept {
				if constexpr( std::is_move_constructible<Self>::value ) {
					return tc::zip(c_rngkey, std::forward<Self>(self));
				} else {
					static_assert( !std::is_lvalue_reference<Self>::value );
					// Cannot static_assert(dependent_false<Self>::value) here.
					// tc::zip(..., NonMovable()) does not compile, but this overload gets instantiated even if not used.
				}
			}

		public:
			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(enumerate)

			// comparison
			template<typename Key_, typename LHS, typename RHS>
			friend constexpr bool operator==( dense_map<Key_, LHS> const& lhs, dense_map<Key_, RHS> const& rhs ) noexcept;

			// iterators
			using iterator = tc::iterator_t< Array >;
			using const_iterator = tc::iterator_t< Array const >;
			constexpr const_iterator begin() const& noexcept {
				return tc::begin(m_a);
			}
			constexpr const_iterator end() const& noexcept {
				return tc::end(m_a);
			}
			constexpr iterator begin() & noexcept {
				return tc::begin(m_a);
			}
			constexpr iterator end() & noexcept {
				return tc::end(m_a);
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

	namespace no_adl {
		template<typename DenseMapOrDerived> requires tc::is_instance_or_derived<dense_map, DenseMapOrDerived>::value
		struct constexpr_size_base<DenseMapOrDerived>
			: constexpr_size<tc::all_values<tc::type::front_t<typename is_instance_or_derived<dense_map, DenseMapOrDerived>::arguments>>>
		{};
	} // namespace no_adl

	BOOST_MPL_HAS_XXX_TRAIT_DEF(dense_map_key_type)
	
	template <typename Map, typename T>
	struct is_dense_map_with_key final : tc::constant<false> {};
	
	template <typename Map, typename T> requires tc::has_dense_map_key_type<Map>::value
	struct is_dense_map_with_key<Map, T> final
		: tc::constant<std::is_same<typename Map::dense_map_key_type, T>::value>
	{};

	////////////////////
	// special RangeReturns for dense_maps

	struct return_element_key final {
		static constexpr bool requires_iterator = true;

		template<typename It, typename DenseMap, typename... Ref>
		static auto pack_element(It&& it, DenseMap&& rng, Ref&&...) noexcept {
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

		template<typename It, typename DenseMap, typename... Ref>
		static std::optional<typename std::remove_reference_t<DenseMap>::dense_map_key_type> pack_element(It&& it, DenseMap&& rng, Ref&&...) noexcept {
			return tc_at_nodebug(rng.c_rngkey, it - tc::begin(rng));
		}
		template<typename DenseMap>
		static std::optional<typename std::remove_reference_t<DenseMap>::dense_map_key_type> pack_no_element(DenseMap&&) noexcept {
			return std::nullopt;
		}
	};

	template <typename Key, typename Func>
	[[nodiscard]] constexpr auto make_dense_map(tc::func_tag_t, Func&& func) return_ctor_MAYTHROW(
		TC_FWD(tc::dense_map<Key, tc::decay_t<decltype(tc::as_lvalue(tc::decay_copy(func))(tc_front_nodebug(tc::all_values<Key>())))>>),
		( tc::func_tag, std::forward<Func>(func) )
	)

	template <typename Key, /*always deduce Ts, otherwise use dense_map<Key, T>*/int = 0, typename... Ts>
	[[nodiscard]] constexpr auto make_dense_map(tc::fill_tag_t, Ts&&... ts) noexcept {
		return tc::dense_map<Key, tc::common_type_t<Ts...>>(tc::fill_tag, std::forward<Ts>(ts)...);
	}

	template <typename Key, /*always deduce Ts, otherwise use dense_map<Key, T>*/int = 0, typename... Ts>
	[[nodiscard]] constexpr auto make_dense_map(Ts&&... ts) noexcept {
		return tc::dense_map<Key, tc::common_type_t<Ts...>>(std::forward<Ts>(ts)...);
	}

	namespace no_adl {
		///////////////////
		// all_values specialization dense_maps
		template< typename Key, typename Value >
		struct [[nodiscard]] all_values<tc::dense_map<Key,Value> > final {
			friend auto range_output_t_impl(all_values const&) -> tc::type::list<tc::dense_map<Key, Value>>; // declaration only

		private:
			template< int... Ns >
			static decltype(auto) InternalGenerate(std::integer_sequence<int, Ns...>) noexcept {
				auto const AllValues = [](int) noexcept -> decltype(auto) { return tc::all_values<Value>(); };
				return tc::transform(tc::cartesian_product(AllValues(Ns)...), [](auto&& val0, auto&& val1, auto&& ... vals) noexcept {
					return tc::make_dense_map<Key>(tc_move_if_owned(val0), tc_move_if_owned(val1), tc_move_if_owned(vals)...);
				});
			}

		public:
			template< typename Func >
			auto operator()(Func&& func) const& MAYTHROW {
				return tc::for_each(InternalGenerate(tc::make_integer_sequence<int, 0, tc::enum_count<Key>::value>()), std::forward<Func>(func));
			}
		};

		template<typename Key, typename Value>
		struct constexpr_size_base<tc::all_values<tc::dense_map<Key,Value>>, void>
			: tc::constant<tc::pow(tc::constexpr_size<tc::all_values<Value>>::value, tc::constexpr_size<tc::all_values<Key>>::value)>
		{};
	}

	namespace less_key_adl {
		template<typename Key, typename ValueLhs, typename ValueRhs>
		bool less_key_helper(less_key_tag_t, tc::dense_map<Key, ValueLhs> const& lhs, tc::dense_map<Key, ValueRhs> const& rhs) noexcept {
			return std::is_lt(tc::lexicographical_compare_3way(lhs, rhs));
		}
	}
}

#define TC_DENSE_MAP_SUPPORT_1(class_name) \
	TC_DENSE_MAP_SUPPORT_2(class_name, tc::type::deducible_identity_t)

#define TC_DENSE_MAP_SUPPORT_2(class_name, value_template) \
	template <ENABLE_SFINAE> requires std::is_default_constructible<SFINAE_TYPE(base_)>::value \
	class_name() noexcept(std::is_nothrow_default_constructible<base_>::value) \
		: base_() {} \
	\
	template <ENABLE_SFINAE> requires std::is_constructible<SFINAE_TYPE(base_), boost::container::default_init_t>::value \
	explicit class_name(boost::container::default_init_t) noexcept(std::is_nothrow_constructible<base_, boost::container::default_init_t>::value) \
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
		requires tc::is_explicit_castable<base_, tc::dense_map<K, T2> const&>::value \
	explicit class_name(tc::dense_map<K, T2> const& other) noexcept(std::is_nothrow_constructible<base_, tc::dense_map<K, T2> const&>::value) \
		: base_(other) \
	{ \
		STATICASSERTSAME(K, typename base_::dense_map_key_type); /*Should use tc::dense_map<dense_map_key_type, T2> for parameter above, but MSVC turns this into dense_map<int, T2>*/ \
	} \
	template <typename K, typename T2> \
		requires tc::is_explicit_castable<base_, tc::dense_map<K, T2>&&>::value \
	explicit class_name(tc::dense_map<K, T2>&& other) noexcept(std::is_nothrow_constructible<base_, tc::dense_map<K, T2>&&>::value) \
		: base_(other) \
	{ \
		STATICASSERTSAME(K, typename base_::dense_map_key_type); /*Should use tc::dense_map<dense_map_key_type, T2> for parameter above, but MSVC turns this into dense_map<int, T2>*/ \
	} \
	\
	/* inherit assignment */ \
	template <typename T2> \
		requires tc::is_safely_assignable<base_&, typename class_name<T2>::base_ const&>::value \
	class_name& operator=(class_name<T2> const& rhs) & noexcept(std::is_nothrow_assignable<base_, typename class_name<T2>::base_ const&>::value) { \
		tc::base_cast<base_>(*this)=tc::base_cast<typename class_name<T2>::base_>(rhs); \
		return *this; \
	} \
	\
	template <typename T2> \
		requires tc::is_safely_assignable<base_&, typename class_name<T2>::base_&&>::value \
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
	template_name(tc::range_tag_t, Rng&&) -> template_name< type_function< tc::range_value_t<Rng> > >;

#define TC_DENSE_MAP_DEDUCTION_GUIDES(...) \
	TC_EXPAND(BOOST_PP_OVERLOAD(TC_DENSE_MAP_DEDUCTION_GUIDES_, __VA_ARGS__)(__VA_ARGS__))
